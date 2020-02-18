/*
 * Copyright 2020 Oxide Computer Company
 */

use std::error::Error;
use std::borrow::Borrow;
use std::collections::HashMap;

use clap::{App, Arg};
use disc_v::*;

mod tockilator;
use tockilator::*;

macro_rules! fatal {
    ($fmt:expr) => ({
        eprint!(concat!("tockilator: ", $fmt, "\n"));
        ::std::process::exit(1);
    });
    ($fmt:expr, $($arg:tt)*) => ({
        eprint!(concat!("tockilator: ", $fmt, "\n"), $($arg)*);
        ::std::process::exit(1);
    });
}

fn dump(state: &TockilatorState) -> Result<(), Box<dyn Error>> {
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
        "{time:15} {cycle:10} {pc:08x} {hexinst:8} {asm:width$}{flow}{sym}  {fx}",
        time = state.time,
        cycle = state.cycle,
        pc = state.pc,
        hexinst = format!("{:0width$x}", state.inst.inst, width = state.inst.len),
        asm = format!("{:10} {}", state.asm_op, state.asm_args.unwrap_or("")),
        flow = match state.inst.op {
            rv_op::jalr | rv_op::c_jalr | rv_op::jal | rv_op::c_jal => "->",
            rv_op::ret => "<-",
            rv_op::ecall => "↓↓",
            rv_op::mret => "↑↑",
            _ => " |",
        },
        sym = symbol,
        fx = state.effects,
        width = 30 + state.stack.len() * 2,
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

fn flamegraph(tockilator: &mut Tockilator, file: &str, cycles: u64)
  -> Result<(), Box<dyn Error>>
{
    let mut fired: u64 = 0;
    let mut stacks: HashMap<Vec<u32>, u64> = HashMap::new();

    tockilator.tracefile(file, |state| -> Result<(), Box<dyn Error>> {
        if fired == 0 {
            fired = state.cycle;
            return Ok(());
        }

        if state.cycle - fired < cycles {
            return Ok(());
        }

        fired = state.cycle;

        let mut s: Vec<u32> = state.stack.iter().cloned().collect();
        s.push(state.pc);

        let count = stacks.entry(s).or_insert(0);
        *count += 1;

        Ok(())
    })?;

    for (stack, count) in stacks {
        let mut first = true;

        for frame in stack {
            if first {
                first = false;
            } else {
                print!(";");
            }

            if let Some(sym) = tockilator.lookup(frame) {
                print!("{}", sym.demangled);
            } else {
                print!("0x{:x}", frame);
            }
        }

        println!(" {}", count);
    }

    Ok(())
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
        .arg(
            Arg::with_name("flamegraph")
                .short("f")
                .value_name("CYCLES")
                .help("generate flamegraph profiling at CYCLES granularity")
                .number_of_values(1)
                .takes_value(true)
                .conflicts_with("flowtrace")
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

    if let Some(cycles) = matches.value_of("flamegraph") {
        let interval = cycles.parse::<u64>()
            .map_err(|_| fatal!("invalid cycle count")).unwrap();

        flamegraph(&mut tockilator, file, interval)?;
    } else if matches.is_present("flowtrace") {
        flowtrace(&mut tockilator, file)?;
    } else {
        tockilator.tracefile(file, dump)?;
    }

    Ok(())
}
