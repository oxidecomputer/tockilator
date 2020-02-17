/*
 * Copyright 2020 Oxide Computer Company
 */

use std::error::Error;
use std::borrow::Borrow;

use clap::{App, Arg};
use disc_v::*;

mod tockilator;
use tockilator::*;

fn dump(state: &TockilatorState) -> Result<(), Box<dyn Error>> {
    let mut sp = state.iasm.split_ascii_whitespace().filter(|x| *x != "");
    let asmop = sp.next().unwrap();
    let asmarg = sp.next().unwrap_or("");
    let mut symbol = format!("{:8x}", state.pc);

    if let Some(sym) = state.symbol {
        let offset = state.pc - sym.addr;
        if offset == 0 {
            symbol = format!("{}", sym.demangled)
        } else {
            symbol = format!("{}+0x{:x}", sym.demangled, offset)
        }
    };

    println!(
        "{:15} {:10} {:08x} {:8} {:30}{:width$}{}{}  {}",
        state.time,
        state.cycle,
        state.pc,
        format!("{:0width$x}", state.inst.inst, width = state.inst.len),
        format!("{:10} {}", asmop, asmarg),
        "",
        match state.inst.op {
            rv_op::jalr | rv_op::c_jalr | rv_op::jal | rv_op::c_jal => "->",
            rv_op::ret => "<-",
            rv_op::ecall => "↓↓",
            rv_op::mret => "↑↑",
            _ => " |",
        },
        symbol,
        state.effects,
        width = state.stack.len() * 2,
    );

    Ok(())
}

fn flowtrace(tockilator: &mut Tockilator, file: &str)
  -> Result<(), Box<dyn Error>>
{

    let mut entry = false;

    tockilator.tracefile(file, |state| -> Result<(), Box<dyn Error>> {
        let f: &str = &format!("{:x}", state.pc);

        if entry {
            println!("{} {:width$} -> {}", "",
                state.cycle,
                match state.symbol {
                    Some(sym) => sym.demangled.borrow(),
                    None => f
                },
                width = state.stack.len() * 2);
        }

        if state.inst.op == rv_op::ret {
            println!("{} {:width$} <- {}", "",
                state.cycle,
                match state.symbol {
                    Some(sym) => sym.demangled.borrow(),
                    None => f
                },
                width = state.stack.len() * 2);
        }

        match state.inst.op {
            rv_op::jalr | rv_op::c_jalr | rv_op::jal | rv_op::c_jal => {
                entry = true;
            },
            _ => { entry = false; }
        }

        Ok(())
    })
}

fn main() -> Result<(), Box<dyn Error>> {
    let matches = App::new("tockilator")
        .arg(
            Arg::with_name("elf")
                .short("e")
                .value_name("FILE")
                .help("import debug info from the specified ELF file")
                .multiple(true)
                .number_of_values(1)
                .takes_value(true),
        )
        .arg(
            Arg::with_name("no-dwarf")
                .short("D")
                .help("Disable DWARF processing"),
        )
        .arg(
            Arg::with_name("flowtrace")
                .short("F")
                .help("shows only function flow trace")
        )
        .arg(Arg::with_name("tracefile").required(true).index(1))
        .get_matches();

    let mut tockilator = Tockilator::default();

    if let Some(elfs) = matches.values_of("elf") {
        for elf in elfs {
            tockilator.loadobj(
                elf,
                if matches.is_present("no-dwarf") {
                    TockilatorLoadobjOptions::None
                } else {
                    TockilatorLoadobjOptions::LoadDwarfOrDie
                },
            )?;
        }
    }

    let file = matches.value_of("tracefile").unwrap();

    if matches.is_present("flowtrace") {
        flowtrace(&mut tockilator, file)?;
    } else {
        tockilator.tracefile(file, dump)?;
    }

    Ok(())
}
