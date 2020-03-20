/*
 * Copyright 2020 Oxide Computer Company
 */

use std::borrow::Borrow;
use std::error::Error;

use clap::{App, Arg};
use disc_v::*;

mod tockilator;
use tockilator::*;

fn dump(state: &TockilatorState) -> Result<(), Box<dyn Error>> {
    let mut symbol = format!("{:8x}", state.pc);

    if let Some(sym) = state.symbol {
        let offset = state.pc - sym.addr;
        if offset == 0 {
            symbol = format!("{}", sym.demangled)
        } else {
            if state.inlined.len() != 0 {
                symbol = format!(
                    "{}+0x{:x} ({})",
                    sym.demangled,
                    offset,
                    state.inlined.last().unwrap().name
                );
            } else {
                symbol = format!("{}+0x{:x}", sym.demangled, offset)
            }
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

fn dump_param(
    state: &TockilatorState,
    param: &TockilatorVariable,
    ident: usize,
) -> Result<(), Box<dyn Error>> {
    let result = state.evaluate(param.expr)?;

    print!(
        "{} {:ident$}   ( {}=",
        state.cycle,
        "",
        param.name,
        ident = ident
    );

    match result {
        None => {
            println!("<unknown>");
        }
        Some(vals) => {
            let mut sep = "";

            for v in vals {
                print!("{}0x{:x} ({})", sep, v, param.id);
                sep = ", ";
            }
            println!("");
        }
    }

    Ok(())
}

fn flowtrace(
    tockilator: &mut Tockilator,
    file: &str,
    matches: &clap::ArgMatches,
) -> Result<(), Box<dyn Error>> {
    let mut entry = true;
    let mut inlined: Vec<TockilatorGoff> = vec![];

    tockilator.tracefile(file, |state| -> Result<(), Box<dyn Error>> {
        let f: &str = &format!("{:x}", state.pc);
        let base = state.stack.iter().fold(0, |sum, &val| sum + val.1 + 2);
        let mut ident = base;
        let mut output = false;
        let sigil = 2;

        if entry {
            println!(
                "{} {:ident$}-> {}",
                state.cycle,
                "",
                match state.symbol {
                    Some(sym) => sym.demangled.borrow(),
                    None => f,
                },
                ident = ident,
            );

            for param in state.params.iter() {
                dump_param(state, param, ident)?;
            }

            output = true;
        }

        for i in 0..state.inlined.len() {
            if i < inlined.len() && inlined[i] == state.inlined[i].id {
                continue;
            }

            ident = base + (i * 2) + sigil;

            println!(
                "{} {:ident$} | {} ({})",
                state.cycle,
                "",
                state.inlined[i].name,
                state.inlined[i].id,
                ident = ident,
            );

            if let Some(params) = state.iparams.get(&state.inlined[i].id) {
                for param in params.iter() {
                    dump_param(state, param, ident)?;
                }
            }

            output = true;
        }

        while let Some(_top) = inlined.pop() {
            continue;
        }

        for inline in state.inlined {
            inlined.push(inline.id);
        }

        if state.inst.op == rv_op::ret {
            ident = base;

            println!(
                "{} {:ident$}<- {}",
                state.cycle,
                "",
                match state.symbol {
                    Some(sym) => sym.demangled.borrow(),
                    None => f,
                },
                ident = ident,
            );
            output = true;
        }

        macro_rules! regfmt {
            () => { "" };
            ($reg:tt) => {
                format!("{:2}:{:8x}", stringify!($reg),
                    state.regs[rv_ireg::$reg as usize])
            };
            ($reg:tt, $($rest:tt)*) => {
                format!("{:2}:{:8x} {}", stringify!($reg),
                    state.regs[rv_ireg::$reg as usize],
                    regfmt!($($rest)*))
            }
        }

        macro_rules! regline {
            () => { println!("{}", state.cycle) };
            ($($regs:tt)*) => {
                println!("{} {:ident$} {}",
                    state.cycle, "", regfmt!($($regs)*), ident = ident + sigil);
            }
        }

        if output && matches.is_present("allreg") {
            println!("{} {:ident$} pc:{:8x}",
                state.cycle, "", state.pc, ident = ident + sigil);
            regline!(ra, sp, gp, tp);
            regline!(t0, t1, t2, t3);
            regline!(t4, t5, t6);
            regline!(s0, s1, s2, s3);
            regline!(a0, a1, a2, a3);
            regline!(a4, a5, a6, a7);
            regline!();
        }

        match state.inst.op {
            rv_op::jalr | rv_op::c_jalr | rv_op::jal | rv_op::c_jal => {
                entry = true;
            }
            _ => {
                entry = false;
            }
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
                .help("shows only function flow trace"),
        )
        .arg(
            Arg::with_name("allreg")
                .short("a")
                .help("shows all registers"),
        )
        .arg(
            Arg::with_name("dryrun")
                .short("n")
                .help("do not process trace file"),
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

    if matches.is_present("dryrun") {
        return Ok(());
    }

    let file = matches.value_of("tracefile").unwrap();

    if matches.is_present("flowtrace") {
        flowtrace(&mut tockilator, file, &matches)?;
    } else {
        tockilator.tracefile(file, dump)?;
    }

    Ok(())
}
