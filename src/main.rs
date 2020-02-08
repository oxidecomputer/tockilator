use std::collections::BTreeMap;
use std::fs;
use std::fs::File;
use std::io::{BufRead, BufReader};

use clap::{App, Arg};
use disc_v::*;
use goblin::*;
use rustc_demangle::demangle;

struct RVState {
    x: [u32; 32],
    stack: Vec<u32>,
}

fn verilog_parse(line: &str) -> Option<(usize, usize, u32, &str, &str, &str)> {
    let mut fields = line.match_indices("\t");
    let time = fields.next()?.0;
    let cycle = fields.next()?.0;
    let pc = fields.next()?.0;
    let ibin = fields.next()?.0;
    let mut iasm = fields.next()?.0;
    fields.next().map(|operands| iasm = operands.0);

    Some((
        usize::from_str_radix(&line[..time].trim(), 10).unwrap(),
        usize::from_str_radix(&line[time + 1..cycle].trim(), 10).unwrap(),
        u32::from_str_radix(&line[cycle + 1..pc].trim(), 0x10).unwrap(),
        &line[pc + 1..ibin].trim(),
        &line[ibin + 1..iasm].trim(),
        &line[iasm + 1..].trim(),
    ))
}

fn parse_effects(effects: &str) -> Vec<(u32, u32)> {
    effects
        .split(" ")
        .filter(|eff| {
            if *eff == "" {
                false
            } else if eff.find("=").is_some() {
                true
            } else {
                assert!(eff.find(":").is_some());
                false
            }
        })
        .map(|eff| {
            let eq = eff.find("=").expect("missing =");
            (
                u32::from_str_radix(&eff[1..eq], 10).unwrap(),
                u32::from_str_radix(&eff[eq + 1 + "0x".len()..], 0x10).unwrap(),
            )
        })
        .collect()
}

fn main() -> std::io::Result<()> {
    let matches = App::new("tockilator")
        .arg(
            Arg::with_name("elf")
                .short("e")
                .value_name("FILE")
                .help("Sets a custom config file")
                .multiple(true)
                .number_of_values(1)
                .takes_value(true),
        )
        .arg(Arg::with_name("tracefile").required(true).index(1))
        .get_matches();

    let mut symbols = BTreeMap::new();

    if let Some(elfs) = matches.values_of("elf") {
        for elf in elfs {
            let buffer = fs::read(elf).expect("bad elf");
            match Object::parse(&buffer).expect("bad elf") {
                Object::Elf(e) => {
                    for sym in e.syms.iter() {
                        if sym.st_name != 0 && sym.st_size != 0 {
                            symbols.insert(
                                sym.st_value,
                                (
                                    String::from(e.strtab.get(sym.st_name).unwrap().unwrap()),
                                    sym.st_size,
                                ),
                            );
                        }
                    }
                }
                _ => panic!("unexpected object"),
            }
        }
    }

    let file = BufReader::new(File::open(matches.value_of("tracefile").unwrap())?);
    let mut regs = RVState {
        x: [0; 32],
        stack: Vec::new(),
    };
    regs.x[1] = 7;
    for line in file.lines().skip(1) {
        match line {
            Ok(ll) => process_line(&ll, &mut regs, &symbols),
            Err(_err) => println!("bad"),
        }
    }
    Ok(())
}

fn process_line(ll: &str, state: &mut RVState, symbols: &BTreeMap<u64, (String, u64)>) {
    //println!("{}", ll);
    let time;
    let cycle;
    let pc;
    let ibin;
    let iasm;
    let effects;
    match verilog_parse(&ll) {
        None => panic!("bad line {}", ll),
        Some((t, c, p, b, a, e)) => {
            time = t;
            cycle = c;
            pc = p;
            ibin = b;
            iasm = a;
            effects = e;
        }
    }

    let _ibytes = hex::decode(ibin).unwrap();
    let iint = u64::from_str_radix(ibin, 0x10).unwrap();

    // XXX this thing is kind of janky; it should take a &[u8] rather than a u64
    let inst = decode_inst(rv_isa::rv32, pc as u64, iint);

    let mut symbol = format!("{:8x}", pc);
    if let Some(sym) = symbols.range(..=pc as u64).next_back() {
        if (pc as u64) < *sym.0 + (sym.1).1 {
            let offset = (pc as u64) - *sym.0;
            let dem = demangle(&(sym.1).0);
            if offset == 0 {
                symbol = format!("{}", dem)
            } else {
                symbol = format!("{}+0x{:x}", dem, offset)
            }
        }
    };

    let mut sp = iasm.split_ascii_whitespace().filter(|x| *x != "");
    let asmop = sp.next().unwrap();
    let asmarg = sp.next().unwrap_or("");

    /*
    println!(
        "{:15} {:10}      {:08x}\t{}\t{}\t{}",
        time, cycle, pc, ibin, iasm, effects,
    );
    */
    println!(
        "{:15} {:10} {:08x} {:8} {:30}{:width$}{}{}  {}",
        time,
        cycle,
        pc,
        ibin,
        format!("{:10} {}", asmop, asmarg),
        "",
        match inst.op {
            rv_op::jalr | rv_op::c_jalr | rv_op::jal | rv_op::c_jal => "->",
            rv_op::ret => "<-",
            rv_op::mret => "↓↓",
            rv_op::ecall => "↑↑",
            _ => " |",
        },
        symbol,
        effects,
        width = state.stack.len() * 2,
    );

    /*
    let dis = format_inst(32, &inst);

    println!("{}", iasm);
    println!("{}", dis);
    */

    /*
    let mut sp = dis.split(" ").filter(|x| *x != "");
    let daddr = sp.next().unwrap();
    let dop = sp.next().unwrap();
    let darg = sp.next().unwrap_or("");

    println!("{} {} {}", daddr, dop, darg);
    */

    for eff in parse_effects(effects) {
        state.x[eff.0 as usize] = eff.1;
    }

    /*
    for (x, v) in state.x.iter().enumerate() {
        println!("  %x{}: 0x{:x}", x, v);
    }
    */

    match inst.op {
        rv_op::jalr | rv_op::c_jalr | rv_op::jal | rv_op::c_jal => {
            state.stack.push(pc);
        }
        rv_op::ret => {
            state.stack.pop().or_else(|| panic!("underrun"));
        }
        _ => (),
    }
}
