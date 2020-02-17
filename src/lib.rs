/*
 * Copyright 2020 Oxide Computer Company
 */

use std::borrow::Cow;
use std::collections::BTreeMap;
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

const TOCKILATOR_NREGS: usize = 32;
const TOCKILATOR_REGPREFIX: &'static str = "x";

#[derive(Debug, Default)]
pub struct Tockilator {
    symbols: BTreeMap<u64, (String, u64)>, // ELF symbols
    shortnames: BTreeMap<String, String>,  // demangled names from DWARF
    regs: [u32; TOCKILATOR_NREGS],         // current register state
    stack: Vec<u32>,
}

#[derive(Debug)]
pub struct TockilatorSymbol<'a> {
    pub addr: u32,
    pub name: &'a str,
    pub demangled: Cow<'a, str>,
}

#[derive(Debug)]
pub struct TockilatorState<'a> {
    pub line: u64,                                // line in input
    pub time: usize,                              // time
    pub cycle: usize,                             // cycle count
    pub pc: u32,                                  // program counter
    pub symbol: Option<&'a TockilatorSymbol<'a>>, // symbol for pc, if any
    pub inst: &'a rv_decode,                      // instruction decoded
    pub regs: &'a [u32; TOCKILATOR_NREGS],        // registers
    pub iasm: &'a str,    // instruction, as disp. by Verilator
    pub effects: &'a str, // effects, as disp. by Verilator
    pub stack: &'a [u32], // stack
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
) -> Option<(usize, usize, u32, u32, &str, &str)> {
    let mut fields = line.match_indices("\t");

    let time = ..fields.next()?.0;
    let cycle = time.end + 1..fields.next()?.0;
    let pc = cycle.end + 1..fields.next()?.0;
    let ibin = pc.end + 1..fields.next()?.0;
    let iasm = {
        let end_or_separator = fields.next()?;
        // If there's another tab, take it instead, but tolerate absence.
        ibin.end + 1..fields.next().unwrap_or(end_or_separator).0
    };

    Some((
        usize::from_str_radix(&line[time].trim(), 10).ok()?,
        usize::from_str_radix(&line[cycle].trim(), 10).ok()?,
        u32::from_str_radix(&line[pc].trim(), 0x10).ok()?,
        u32::from_str_radix(&line[ibin].trim(), 0x10).ok()?,
        &line[iasm.clone()].trim(),
        &line[iasm.end + 1..].trim(),
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
            while let Some((_, entry)) = entries.next_dfs()? {
                if entry.tag() != gimli::constants::DW_TAG_subprogram {
                    continue;
                }

                let mut name = None;
                let mut linkage_name = None;

                // Iterate over the attributes in the DIE.
                let mut attrs = entry.attrs();
                while let Some(attr) = attrs.next()? {
                    match attr.name() {
                        gimli::constants::DW_AT_linkage_name => {
                            linkage_name = dwarf_name(&dwarf, attr.value());
                        }
                        gimli::constants::DW_AT_name => {
                            name = dwarf_name(&dwarf, attr.value());
                        }
                        _ => (),
                    }
                }
                if let (Some(nn), Some(ln)) = (name, linkage_name) {
                    if ln != nn {
                        self.shortnames
                            .insert(String::from(ln), String::from(nn));
                    }
                }
            }
        }
        Ok(())
    }

    fn trace(
        &mut self,
        source: impl BufRead,
        callback: impl Fn(&TockilatorState) -> Result<(), Box<dyn Error>>,
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

            let (time, cycle, pc, ibin, iasm, effects) = res;

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

                    symbol = Some(TockilatorSymbol {
                        addr: *sym.0 as u32,
                        name: &(sym.1).0,
                        demangled,
                    });
                }
            }

            callback(&TockilatorState {
                line: lineno as u64,
                time,
                cycle,
                pc,
                symbol: symbol.as_ref(),
                inst: &inst,
                iasm: &iasm,
                regs: &self.regs,
                effects: &effects,
                stack: &self.stack,
            })?;

            match inst.op {
                rv_op::jalr | rv_op::c_jalr | rv_op::jal | rv_op::c_jal => {
                    self.stack.push(pc);
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
        callback: fn(&TockilatorState) -> Result<(), Box<dyn Error>>,
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
            Some((22, 6, 32896, 0x80006f, "jal\tx0,8088", "x0=0x00000000"))
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
                "sw\tx0,0(x5)",
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
