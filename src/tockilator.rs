/*
 * Copyright 2020 Oxide Computer Company
 */

use fallible_iterator::FallibleIterator;
use std::borrow::Cow;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::convert::Infallible;
use std::error::Error;
use std::fmt;
use std::fs;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::str;

use disc_v::*;
use goblin::elf::Elf;
use rustc_demangle::demangle;
use multimap::MultiMap;

pub const TOCKILATOR_NREGS: usize = 32;
const TOCKILATOR_REGPREFIX: &'static str = "x";

#[derive(Debug, Default)]
pub struct Tockilator {
    current: u32,                           // current object
    symbols: BTreeMap<u64, (String, u64)>,  // ELF symbols
    shortnames: BTreeMap<String, String>,   // demangled names from DWARF
    subprograms: BTreeMap<TockilatorGoff, String>,   // DWARF subprograms
    outlined: BTreeMap<u64, TockilatorGoff>, // outlined functions by address

    // Mapping from starting address to length, goff and origin
    inlined: BTreeMap<(u64, isize), (u64, TockilatorGoff, TockilatorGoff)>,

    // Mapping from goff to starting address and length
    bygoff: MultiMap<TockilatorGoff, (u64, u64)>,

    // Mapping from goff to name of parameter and goff of concrete parent
    parameters: BTreeMap<TockilatorGoff, (String, TockilatorGoff)>,

    // Mapping from goff to file/line -- horrifically inefficient!
    fileline: HashMap<TockilatorGoff, (String, u64)>,

    // Mapping from starting address + goff to length, origin and buffer
    expressions: BTreeMap<(u64, TockilatorGoff), (u64, TockilatorGoff, Vec<u8>)>,
    regs: [u32; TOCKILATOR_NREGS],          // current register state
    stack: Vec<(u32, usize)>,               // current stack
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
///
/// An identifier that corresponds to a global offset within a particular DWARF
/// object. This struction is opaque.
///
pub struct TockilatorGoff {
    object: u32,
    goff: usize,
}

#[derive(Debug)]
pub struct TockilatorInlined<'a> {
    pub addr: u32,
    pub name: &'a str,
    pub id: TockilatorGoff,
    pub origin: TockilatorGoff,
    // pub params: &'a [TockilatorVariable<'a>],
}

#[derive(Debug)]
pub struct TockilatorSymbol<'a> {
    pub addr: u32,
    pub name: &'a str,
    pub demangled: Cow<'a, str>,
}

#[derive(Debug)]
pub struct TockilatorVariable<'a> {
    /// identifier of this variable
    pub id: TockilatorGoff,
    /// name of this variable
    pub name: &'a str,
    /// expression for this variable
    pub expr: &'a [u8],
}

#[derive(Debug)]
pub struct TockilatorState<'a> {
    /// Line in input (1-based).
    pub line: u64,
    /// Time of event, measured in Verilog simulator units (essentially
    /// arbitrary but monotonic).
    pub time: usize,
    /// Cycle count in simulator.
    pub cycle: usize,
    /// Program counter value, giving address of current instruction.
    pub pc: u32,
    /// Last symbol before `pc`, if any.
    pub symbol: Option<&'a TockilatorSymbol<'a>>,
    /// Decoded instruction from disassembler.
    pub inst: &'a rv_decode,
    /// Machine general purpose registers.
    pub regs: &'a [u32; TOCKILATOR_NREGS],
    /// Instruction name as printed by Verilator.
    pub asm_op: &'a str,
    /// Instruction arguments as printed by Verilator.
    pub asm_args: Option<&'a str>,
    /// Instruction effects, as printed by Verilator.
    pub effects: &'a str,
    /// Current stack model.
    pub stack: &'a [(u32, usize)],
    /// Inlined stack
    pub inlined: &'a [TockilatorInlined<'a>],
    /// Parameters 
    pub params: &'a [TockilatorVariable<'a>],
    /// Inlined parameters
    pub iparams: &'a HashMap<TockilatorGoff, Vec<TockilatorVariable<'a>>>,
}

#[derive(Debug)]
pub struct TockilatorError {
    errmsg: String,
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum TockilatorLoadobjOptions {
    LoadDwarf,
    LoadDwarfOrDie,
    None,
}

impl<'a> From<&'a str> for TockilatorError {
    fn from(msg: &'a str) -> TockilatorError {
        msg.to_string().into()
    }
}

impl From<String> for TockilatorError {
    fn from(errmsg: String) -> TockilatorError {
        TockilatorError { errmsg }
    }
}

impl fmt::Display for TockilatorError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.errmsg)
    }
}

impl Error for TockilatorError {
    fn description(&self) -> &str {
        &self.errmsg
    }
}

fn err<S: ToString>(msg: S) -> Box<dyn Error> {
    Box::new(TockilatorError::from(msg.to_string()))
}

impl fmt::Display for TockilatorGoff {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.object > 0 {
            write!(f, "GOFF 0x{:x} in object {}", self.goff, self.object)
        } else {
            write!(f, "GOFF 0x{:x}", self.goff)
        }
    }
}

/// Parse Ibex Trace output from Verilator into our state fields.
///
/// The trace output from the Ibex verilator simulator is tab-delimited, but
/// contains a variable number of columns:
///
/// `TIME \t CYCLE \t PC \t BINARY_INSTRUCTION \t ASM (\t ARGS)? \t EFFECTS`
///
/// And so this parser is slightly more complex than you might expect for
/// basic TSV.
fn parse_verilator_line(
    line: &str,
) -> Option<(usize, usize, u32, u32, &str, Option<&str>, &str)> {
    let mut fields = line.match_indices("\t");

    let time = ..fields.next()?.0;
    let cycle = time.end + 1..fields.next()?.0;
    let pc = cycle.end + 1..fields.next()?.0;
    let ibin = pc.end + 1..fields.next()?.0;
    let asm_op = ibin.end + 1..fields.next()?.0;
    let asm_args = fields.next().map(|(end, _)| asm_op.end + 1..end);

    let fx_start = asm_args.as_ref().unwrap_or(&asm_op).end + 1;

    Some((
        usize::from_str_radix(&line[time].trim(), 10).ok()?,
        usize::from_str_radix(&line[cycle].trim(), 10).ok()?,
        u32::from_str_radix(&line[pc].trim(), 0x10).ok()?,
        u32::from_str_radix(&line[ibin].trim(), 0x10).ok()?,
        line[asm_op].trim(),
        asm_args.map(|args| line[args].trim()),
        line[fx_start..].trim(),
    ))
}

fn parse_verilator_effects(
    effects: &str,
) -> Result<Option<(usize, u32)>, Box<dyn Error>> {
    // The trace format includes register state changes, but it also includes
    // bits of state printed as an FYI. The state changes use an equals sign,
    // while the FYIs use a colon.
    let e: Vec<&str> = effects
        .split(" ")
        .filter(|eff| {
            if *eff == "" {
                false
            } else if eff.find("=").is_some() {
                true
            } else {
                // If we start getting effects that *aren't* = or :, we want to
                // find out:
                assert!(eff.find(":").is_some());
                false
            }
        })
        .collect();

    if e.len() == 0 {
        return Ok(None);
    }

    if e.len() > 1 {
        return Err(err("too many effects"));
    }

    let eff = e[0];
    let eq = eff.find("=0x").ok_or_else(|| err("bad effect value"))?;

    if &eff[..1] != TOCKILATOR_REGPREFIX {
        return Err(err("bad register name"));
    }

    let reg = u32::from_str_radix(&eff[1..eq], 10)? as usize;
    let val = u32::from_str_radix(&eff[eq + 1 + "0x".len()..], 0x10)?;

    if reg >= TOCKILATOR_NREGS {
        return Err(err("invalid register"));
    }

    Ok(Some((reg, val)))
}

impl TockilatorState<'_> {
    pub fn evaluate(
        &self,
        expr: &[u8]
    ) -> Result<Option<Vec<u32>>, Box<dyn Error>> {
        let bytes = gimli::EndianSlice::new(expr, gimli::LittleEndian);

        let encoding = gimli::Encoding {
            format: gimli::Format::Dwarf32,
            version: 4,
            address_size: 4
        };

        let mut eval = gimli::Evaluation::new(bytes, encoding);
        let mut rval: Vec<u32> = vec![];

        let mut result = eval.evaluate()?;

        while result != gimli::EvaluationResult::Complete {
            match result {
                gimli::EvaluationResult::RequiresRegister {
                    register,
                    base_type: _,
                } => {
                    let gimli::Register(r) = register;
                    let val = gimli::Value::Generic(self.regs[r as usize].into());

                    result = eval.resume_with_register(val)?;
                }
                gimli::EvaluationResult::RequiresMemory {
                    address: _,
                    size: _,
                    space: _,
                    base_type: _,
                } => {
                    return Ok(None);
                }
                _ => {
                    return Err(err(format!(
                        "unrecognized eval bail: {:?}", result
                    )));
                }
            }
        }

        let result = eval.result();

        for piece in result.iter() {
            match piece.location {
                gimli::Location::Register { register } => {
                    let gimli::Register(r) = register;
                    rval.push(self.regs[r as usize]);
                }

                gimli::Location::Value { value } => {
                    rval.push(value.to_u64(0xffff_ffff)? as u32);
                }

                gimli::Location::Empty => {}

                _ => { println!("piece: {:?}", piece); }
            }
        }

        Ok(Some(rval))
    }
}

impl Tockilator {
    pub fn loadobj(
        &mut self,
        obj: &str,
        options: TockilatorLoadobjOptions,
    ) -> Result<(), Box<dyn Error>> {
        let buffer = fs::read(obj)
            .map_err(|e| err(format!("failed to read: {}: {}", obj, e)))?;

        let elf = Elf::parse(&buffer).map_err(|e| {
            err(format!("unrecognized ELF object: {}: {}", e, obj))
        })?;

        match options {
            TockilatorLoadobjOptions::LoadDwarf => {
                let _ = self.loadobj_dwarf(&buffer, &elf);
            }
            TockilatorLoadobjOptions::LoadDwarfOrDie => {
                self.loadobj_dwarf(&buffer, &elf)?;
            }
            TockilatorLoadobjOptions::None => (),
        }

        for sym in elf.syms.iter() {
            if sym.st_name == 0 || sym.st_size == 0 {
                continue;
            }

            let name = match elf.strtab.get(sym.st_name) {
                Some(n) => n?,
                None => {
                    return Err(err(format!(
                        "bad symbol in object {}: {}",
                        obj, sym.st_name
                    )));
                }
            };

            self.symbols
                .insert(sym.st_value, (String::from(name), sym.st_size));
        }

        Ok(())
    }

    fn dwarf_goff<R: gimli::Reader<Offset = usize>>(
        &self,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
    ) -> TockilatorGoff {
        let goff = match entry.offset().to_unit_section_offset(unit) {
            gimli::UnitSectionOffset::DebugInfoOffset(o) => o.0,
            gimli::UnitSectionOffset::DebugTypesOffset(o) => o.0,
        };

        TockilatorGoff {
            object: self.current,
            goff: goff
        }
    }
        
    fn dwarf_value_goff<R: gimli::Reader<Offset = usize>>(
        &self,
        unit: &gimli::Unit<R>,
        value: &gimli::AttributeValue<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
    ) -> Option<TockilatorGoff> {
        let goff;

        match value {
            gimli::AttributeValue::UnitRef(offs) => {
                goff = match offs.to_unit_section_offset(unit) {
                    gimli::UnitSectionOffset::DebugInfoOffset(o) => o.0,
                    gimli::UnitSectionOffset::DebugTypesOffset(o) => o.0,
                };
            }

            gimli::AttributeValue::DebugInfoRef(offs) => {
                goff = offs.0;
            }

            _ => { return None; }
        }

        Some(TockilatorGoff {
            object: self.current,
            goff: goff
        })
    }

    fn dwarf_expr<R: gimli::Reader<Offset = usize>>(
        &mut self,
        goff: TockilatorGoff,
        origin: TockilatorGoff,
        begin: u64,
        len: u64,
        data: &gimli::Expression<R>,
    ) -> Result<(), Box<dyn Error>> {
        let v: Vec<u8> = data.0.to_slice()?.iter().map(|&x| x).collect();
        self.expressions.insert((begin, goff), (len, origin, v));

        Ok(())
    }

    fn dwarf_variable<R: gimli::Reader>(
        &mut self,
        _unit: &gimli::Unit<R>,
        _entry: &gimli::DebuggingInformationEntry<R>,
        _depth: isize,
    ) -> Result<(), Box<dyn Error>> {
        Ok(())
    }

    fn dwarf_formal_parameter<'a, R: gimli::Reader<Offset = usize>>(
        &mut self,
        dwarf: &'a gimli::Dwarf<gimli::EndianSlice<gimli::LittleEndian>>,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
        parent: TockilatorGoff,
    ) -> Result<(), Box<dyn Error>> {
        let mut origin: Option<TockilatorGoff> = None;
        let mut attrs = entry.attrs();
        let mut name = None;

        let goff = self.dwarf_goff(unit, entry);

        while let Some(attr) = attrs.next()? {
            match attr.name() {
                gimli::DW_AT_name => {
                    name = dwarf_name(dwarf, attr.value());
                }
                gimli::DW_AT_abstract_origin => {
                    origin = self.dwarf_value_goff(unit, &attr.value());

                    if origin.is_none() {
                        return Err(err(format!("bad origin for GOFF {}", goff)));
                    }
                }
                _ => {}
            }
        }

        match (origin, name) {
            (Some(_), Some(_)) => {
                return Err(err(format!(
                    "found both origin and name for {}", goff
                )));
            }
            
            (Some(_), None) => {}

            (None, None) => { return Ok(()); }

            (None, Some(name)) => {
                /*
                 * We have a name: insert this parameter, and set our origin to
                 * be ourselves to handle the case that we are a simple formal
                 * parameter in a non-inlined function.
                 */
                self.parameters.insert(goff, (String::from(name), parent));
                origin = Some(goff);
            }
        }

        let mut attrs = entry.attrs();

        while let Some(attr) = attrs.next()? {
            match (attr.name(), attr.value()) {
                (
                    gimli::DW_AT_location,
                    gimli::AttributeValue::LocationListsRef(offset),
                ) => {
                    let raw_locations =
                        dwarf.locations.raw_locations(offset, unit.encoding())?;

                    let raw_locations: Vec<_> = raw_locations.collect()?;

                    for location in raw_locations {
                        match location {
                            gimli::RawLocListEntry::AddressOrOffsetPair {
                                begin,
                                end,
                                ref data,
                            } => {
                                self.dwarf_expr(goff, origin.unwrap(),
                                    begin, end - begin, data)?;
                            }
                            _ => {}
                        }
                    }
                }
                (
                    gimli::DW_AT_location,
                    gimli::AttributeValue::Exprloc(data),
                ) => {
                    /*
                     * We need to look up our parent goff, and then add an
                     * expression for every address range we find.
                     */
                    match self.bygoff.get_vec(&parent) {
                        Some(ranges) => {
                            for range in ranges.iter() {
                                let v: Vec<u8> = data.0.iter()
                                    .map(|&x| x).collect();
                                self.expressions.insert(
                                    (range.0, goff),
                                    (range.1, origin.unwrap(), v)
                                );
                            }
                        }
                        None => {
                            return Err(err(format!(
                                "goff {}: missing parent {}", goff, parent
                            )));
                        }
                    };

                }
                _ => {}
            }
        }

        Ok(())
    }

    fn dwarf_inlined<R: gimli::Reader<Offset = usize>>(
        &mut self,
        dwarf: &gimli::Dwarf<R>,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
        depth: isize,
    ) -> Result<(), Box<dyn Error>> {
        /*
         * Iterate over our attributes looking for addresses
         */
        let mut attrs = entry.attrs();
        let mut low: Option<u64> = None;
        let mut high: Option<u64> = None;
        let mut origin: Option<TockilatorGoff> = None;

        let goff = self.dwarf_goff(unit, entry);

        while let Some(attr) = attrs.next()? {
            match (attr.name(), attr.value()) {
                (
                    gimli::constants::DW_AT_low_pc,
                    gimli::AttributeValue::Addr(addr),
                ) => {
                    low = Some(addr);
                }
                (
                    gimli::constants::DW_AT_high_pc,
                    gimli::AttributeValue::Udata(data),
                ) => {
                    high = Some(data);
                }
                (
                    gimli::constants::DW_AT_abstract_origin,
                    _
                ) => {
                    origin = self.dwarf_value_goff(unit, &attr.value());
                }
                _ => {}
            }
        }

        match (low, high, origin) {
            (Some(addr), Some(len), Some(origin)) => {
                self.inlined.insert((addr, depth), (len, goff, origin));
                self.bygoff.insert(goff, (addr, len));
                return Ok(());
            }
            (None, None, Some(_o)) => {}
            _ => {
                return Err(err(format!("missing origin for {}", goff)));
            }
        }

        let mut attrs = entry.attrs();
        while let Some(attr) = attrs.next()? {
            match (attr.name(), attr.value()) {
                (
                    gimli::constants::DW_AT_ranges,
                    gimli::AttributeValue::RangeListsRef(r),
                ) => {
                    let raw_ranges =
                        dwarf.ranges.raw_ranges(r, unit.encoding())?;
                    let raw_ranges: Vec<_> = raw_ranges.collect()?;

                    for r in raw_ranges {
                        match r {
                            gimli::RawRngListEntry::AddressOrOffsetPair {
                                begin,
                                end,
                            } => {
                                self.inlined.insert(
                                    (begin, depth),
                                    (end - begin, goff, origin.unwrap()),
                                );
                                self.bygoff.insert(goff, (begin, end - begin));
                            }
                            _ => {}
                        }
                    }

                    return Ok(());
                }
                _ => {}
            }
        }

        Err(err(format!("missing address range for {}", goff)))
    }

    fn dwarf_subprogram<'a, R: gimli::Reader<Offset = usize>>(
        &mut self,
        dwarf: &'a gimli::Dwarf<gimli::EndianSlice<gimli::LittleEndian>>,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
    ) -> Result<(), Box<dyn Error>> {
        let mut name = None;
        let mut linkage_name = None;
        let mut addr = None;
        let mut len = None;

        let goff = self.dwarf_goff(unit, entry);

        // Iterate over the attributes in the DIE.
        let mut attrs = entry.attrs();
        while let Some(attr) = attrs.next()? {
            match (attr.name(), attr.value()) {
                (
                    gimli::constants::DW_AT_low_pc,
                    gimli::AttributeValue::Addr(value),
                ) => {
                    addr = Some(value);
                }
                (
                    gimli::constants::DW_AT_high_pc,
                    gimli::AttributeValue::Udata(value),
                ) => {
                    len = Some(value);
                }
                (
                    gimli::constants::DW_AT_linkage_name,
                    _
                ) => {
                    linkage_name = dwarf_name(dwarf, attr.value());
                }
                (
                    gimli::constants::DW_AT_name,
                    _
                ) => {
                    name = dwarf_name(dwarf, attr.value());
                }
                _ => {}
            }
        }

        if let (Some(nn), Some(ln)) = (name, linkage_name) {
            if ln != nn {
                self.shortnames.insert(String::from(ln), String::from(nn));
            }
        }

        if let Some(nn) = name {
            self.subprograms.insert(goff, String::from(nn));

            if let Some(addr) = addr {
                if addr != 0 {
                    self.outlined.insert(addr, goff);
                }
            }
        }

        if let (Some(addr), Some(len)) = (addr, len) {
            self.bygoff.insert(goff, (addr, len));
        }

        Ok(())
    }

    fn dwarf_fileline<R: gimli::Reader<Offset = usize>>(
        &mut self,
        dwarf: &gimli::Dwarf<R>,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<R>,
        goff: &TockilatorGoff,
    ) -> Result<(), Box<dyn Error>> {
        let mut attrs = entry.attrs();
        let mut file = None;
        let mut line = None;

        while let Some(attr) = attrs.next()? {
             match (attr.name(), attr.value()) {
                (
                    gimli::constants::DW_AT_decl_file,
                    gimli::AttributeValue::FileIndex(value),
                ) => {
                    file = Some(value);
                }
                (
                    gimli::constants::DW_AT_decl_line,
                    gimli::AttributeValue::Udata(value),
                ) => {
                    line = Some(value);
                }
                _ => {}
            }
        }

        if let (Some(file), Some(line)) = (file, line) {
            let header = match unit.line_program {
                Some(ref program) => program.header(),
                None => return Ok(()),
            };
            let file = match header.file(file) {
                Some(header) => header,
                None => {
                    return Err(err(format!("no header at {}", goff)));
                }
            };

            let s = dwarf.attr_string(unit, file.path_name())?;
            let d = s.to_string_lossy()?;

            self.fileline.insert(*goff, (d.to_string(), line));
        }

        Ok(())
    }

    fn loadobj_dwarf(
        &mut self,
        buffer: &[u8],
        elf: &goblin::elf::Elf,
    ) -> Result<(), Box<dyn Error>> {
        // Load all of the sections. This "load" operation just gets the data in
        // RAM -- since we've already loaded the Elf file, this can't fail.
        let dwarf = gimli::Dwarf::<&[u8]>::load::<_, _, Infallible>(
            // Load the normal Dwarf section(s) from our Elf image.
            |id| {
                let sec_result = elf
                    .section_headers
                    .iter()
                    .filter(|sh| {
                        if let Some(Ok(name)) = elf.shdr_strtab.get(sh.sh_name)
                        {
                            name == id.name()
                        } else {
                            false
                        }
                    })
                    .next();
                Ok(sec_result
                    .map(|sec| {
                        let offset = sec.sh_offset as usize;
                        let size = sec.sh_size as usize;
                        buffer.get(offset..offset + size).unwrap()
                    })
                    .unwrap_or(&[]))
            },
            // We don't have a supplemental object file.
            |_| Ok(&[]),
        )?;
        // Borrow all sections wrapped in EndianSlices
        let dwarf = dwarf.borrow(|section| {
            gimli::EndianSlice::new(section, gimli::LittleEndian)
        });

        // Iterate over the compilation units.
        let mut iter = dwarf.units();
        while let Some(header) = iter.next()? {
            let unit = dwarf.unit(header)?;
            // Iterate over the Debugging Information Entries (DIEs) in the unit.
            let mut entries = unit.entries();
            let mut depth = 0;
            let mut stack: Vec<TockilatorGoff> = vec![];

            while let Some((delta, entry)) = entries.next_dfs()? {
                depth += delta;

                let goff = self.dwarf_goff(&unit, entry);

                if depth as usize >= stack.len() {
                    stack.push(goff);
                } else {
                    stack[depth as usize] = goff;
                }

                self.dwarf_fileline(&dwarf, &unit, &entry, &goff)?;

                match entry.tag() {
                    gimli::constants::DW_TAG_formal_parameter => {
                        self.dwarf_formal_parameter(&dwarf, &unit, &entry,
                            stack[(depth - 1) as usize]
                        )?;
                    }

                    gimli::constants::DW_TAG_variable => {
                        self.dwarf_variable(&unit, &entry, depth)?;
                    }

                    gimli::constants::DW_TAG_inlined_subroutine => {
                        self.dwarf_inlined(&dwarf, &unit, &entry, depth)?;
                    }

                    gimli::constants::DW_TAG_subprogram => {
                        self.dwarf_subprogram(&dwarf, &unit, &entry)?;
                    }
                    _ => {}
                }
            }
        }

        Ok(())
    }

    fn trace(
        &mut self,
        source: &mut std::io::BufReader<std::fs::File>,
        mut callback: impl FnMut(&TockilatorState) -> Result<(), Box<dyn Error>>,
    ) -> Result<(), Box<dyn Error>> {
        let mut lines = source.lines();

        // Expect, but discard, header line.
        lines.next().ok_or_else(|| err("zero-length input"))??;

        // 1 to make lines 1-based, 1 to account for the header we skipped.
        const OFFSET: usize = 2;

        for (lineno_zero, line) in lines.enumerate() {
            let lineno = lineno_zero + OFFSET;
            let l =
                line.map_err(|_| err(format!("I/O error on line {}", lineno)))?;

            let res = parse_verilator_line(&l).ok_or_else(|| {
                err(format!("invalid input on line {}", lineno))
            })?;

            let (time, cycle, pc, ibin, asm_op, asm_args, effects) = res;

            let val = parse_verilator_effects(effects).map_err(|e| {
                err(format!("invalid effect on line {}: {}", lineno, e))
            })?;

            if let Some((reg_no, value)) = val {
                self.regs[reg_no] = value;
            }

            /*
             * Now decode the instruction.
             */
            let inst = decode_inst(rv_isa::rv32, pc as u64, ibin as u64);

            let mut symbol: Option<TockilatorSymbol> = None;
            let mut base = pc;

            let mut params = vec![];

            if let Some(sym) = self.symbols.range(..=pc as u64).next_back() {
                if (pc as u64) < *sym.0 + (sym.1).1 {
                    let name = &(sym.1).0;

                    /*
                     * Check to see if we have a short name from DWARF;
                     * otherwise run it through the demangler.
                     */
                    let demangled =
                        if let Some(shortname) = self.shortnames.get(name) {
                            shortname.into()
                        } else {
                            demangle(name).to_string().into()
                        };

                    base = *sym.0 as u32;

                    symbol = Some(TockilatorSymbol {
                        addr: base,
                        name: &(sym.1).0,
                        demangled,
                    });
                }
            }

            let mut inlined: Vec<TockilatorInlined> = vec![];
            let mut iparams: HashMap<TockilatorGoff, Vec<TockilatorVariable>>;

            iparams = HashMap::new();

            for ((addr, _depth), (len, goff, origin)) in
                self.inlined.range(..=(pc as u64, std::isize::MAX)).rev()
            {
                if addr + len < base as u64 {
                    break;
                }

                if addr + len <= pc as u64 {
                    continue;
                }

                if let Some(func) = self.subprograms.get(origin) {
                    inlined.push(TockilatorInlined {
                        addr: *addr as u32,
                        name: &func,
                        id: *goff,
                        origin: *origin,
                    });
                }

                iparams.insert(*goff, Vec::new());
            }

            inlined.reverse();

            /*
             * The next step is to go through our parameters XXX.
             */

            /*
             * Now go through all of our expressions and find the ones that
             * are valid for our pc.
             */
            for ((addr, goff), (len, origin, expr)) in
                self.expressions.range(..=(pc as u64, TockilatorGoff {
                    object: std::u32::MAX,
                    goff: std::usize::MAX
                })).rev()
            {
                if *addr + len < base as u64 {
                    break;
                }

                if *addr + len <= pc as u64 {
                    continue;
                }

                if let Some((param, parent)) = self.parameters.get(origin) {
                    /*
                     * We have an expression that is valid that is a parameter.
                     * Now we need to see if the parameter's parent (that is,
                     * its subprogram) corresponds to our current PC.
                     */
                    if let Some(me) = self.outlined.get(&(pc as u64)) {
                        if parent == me {
                            params.push(TockilatorVariable {
                                id: *goff,
                                name: param,
                                expr: expr,
                            });
                        }
                    }

                    for inline in inlined.iter() {
                        if *parent == inline.origin {
                            iparams.get_mut(&inline.id)
                                .unwrap()
                                .push(TockilatorVariable {
                                    id: *goff,
                                    name: param,
                                    expr: expr,
                                }
                            );
                        }
                    }
                }
            }

            callback(&TockilatorState {
                line: lineno as u64,
                time,
                cycle,
                pc,
                symbol: symbol.as_ref(),
                inst: &inst,
                asm_op,
                asm_args,
                regs: &self.regs,
                effects: &effects,
                stack: &self.stack,
                inlined: inlined.as_slice(),
                params: &params,
                iparams: &iparams,
            })?;

            match inst.op {
                rv_op::jalr | rv_op::c_jalr | rv_op::jal | rv_op::c_jal => {
                    self.stack.push((pc, inlined.len()));
                }
                rv_op::ret => {
                    self.stack.pop().or_else(|| panic!("underrun"));
                }
                _ => (),
            }
        }

        Ok(())
    }

    pub fn tracefile(
        &mut self,
        file: &str,
        callback: impl FnMut(&TockilatorState) -> Result<(), Box<dyn Error>>,
    ) -> Result<(), Box<dyn Error>> {
        let mut file = BufReader::new(File::open(file)?);
        self.trace(&mut file, callback)
    }
}

fn dwarf_name<'a>(
    dwarf: &'a gimli::Dwarf<gimli::EndianSlice<gimli::LittleEndian>>,
    value: gimli::AttributeValue<gimli::EndianSlice<gimli::LittleEndian>>,
) -> Option<&'a str> {
    match value {
        gimli::AttributeValue::DebugStrRef(strref) => {
            let dstring = dwarf.debug_str.get_str(strref).ok()?;
            let ddstring = str::from_utf8(dstring.slice()).ok()?;
            Some(ddstring)
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_verilator_basic() {
        let res = parse_verilator_line(
            r##" 22	         6	00008080	0080006f	jal	x0,8088	  x0=0x00000000"##,
        );
        assert_eq!(
            res,
            Some((
                22,
                6,
                32896,
                0x80006f,
                "jal",
                Some("x0,8088"),
                "x0=0x00000000"
            ))
        );
    }

    #[test]
    fn parse_verilator_longer() {
        let res = parse_verilator_line(
            r##" 102	        46	000080e2	0002a023	sw	x0,0(x5)	  x5:0x10000000  x0:0x00000000 PA:0x10000000 store:0x00000000 load:0x00000000"##,
        );
        assert_eq!(
            res,
            Some((
                102,
                46,
                32994,
                0x2a023,
                "sw",
                Some("x0,0(x5)"),
                "x5:0x10000000  x0:0x00000000 PA:0x10000000 store:0x00000000 load:0x00000000"
            ))
        );
    }

    #[test]
    fn parse_verilator_badtime() {
        let res = parse_verilator_line(
            r##" iambad	         6	00008080	0080006f	jal	x0,8088	  x0=0x00000000"##,
        );
        assert_eq!(res, None);
    }

    #[test]
    fn parse_verilator_badcycle() {
        let res = parse_verilator_line(
            r##" 22	       bad	00008080	0080006f	jal	x0,8088	  x0=0x00000000"##,
        );
        assert_eq!(res, None);
    }

    #[test]
    fn parse_verilator_badpc() {
        let res = parse_verilator_line(
            r##" 22	         6	badpc	0080006f	jal	x0,8088	  x0=0x00000000"##,
        );
        assert_eq!(res, None);
    }

    #[test]
    fn parse_effect_basic() {
        let res = parse_verilator_effects(
            r##"x8:0x10000fb0 x10=0x10003c0c PA:0x10000f00 store:0x00000000"##,
        );
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Some((10, 0x10003c0c)));
    }

    #[test]
    fn parse_effect_multiple() {
        let res = parse_verilator_effects(
            r##"x8=0x10000fb0 x10=0x10003c0c PA:0x10000f00 store:0x00000000"##,
        );
        assert!(res.is_err());
    }

    #[test]
    fn parse_effect_none() {
        let res = parse_verilator_effects(r##"x8:0x10000fb0"##);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), None);
    }

    #[test]
    fn parse_effect_nohex() {
        let res = parse_verilator_effects(r##"x8=1230"##);
        assert!(res.is_err());
    }

    #[test]
    fn parse_effect_badhex() {
        let res = parse_verilator_effects(r##"x8=0xdoogleknowsallprobes"##);
        assert!(res.is_err());
    }

    #[test]
    fn parse_effect_highreg() {
        let res = parse_verilator_effects(r##"x45=0x10000fb0"##);
        assert!(res.is_err());
    }

    #[test]
    fn parse_effect_badreg() {
        let res = parse_verilator_effects(r##"j8=0x10000fb0"##);
        assert!(res.is_err());
    }

    #[test]
    fn parse_effect_badval() {
        let res = parse_verilator_effects(r##"x=0x10000fb0"##);
        assert!(res.is_err());
    }
}
