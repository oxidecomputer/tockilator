/*
 * Copyright 2020 Oxide Computer Company
 */

use std::borrow::Borrow;
use std::error::Error;
use std::str::FromStr;

use clap::{App, Arg};
use disc_v::*;
use colored::Colorize;

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

#[derive(Debug)]
struct TockilatorConfig {
    noparams: bool,
    verbose: bool,
    allreg: bool,
    range: (Option<usize>, Option<usize>),
}

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

fn color(
    input: &str,
    object: Option<u32>,
    param: bool
) -> colored::ColoredString {
    match (object, param) {
        (Some(0), false) => input.red().bold(),
        (Some(0), true) => input.red(),
        (Some(1), false) => input.magenta().bold(),
        (Some(1), true) => input.blue(),
        (Some(2), false) => input.green().bold(),
        (Some(2), true) => input.cyan(),
        _ => input.normal(),
    }
}

fn dump_param(
    state: &TockilatorState,
    param: &TockilatorVariable,
    ident: usize,
) -> Result<(), Box<dyn Error>> {
    let result = state.evaluate(param)?;

    print!(
        "{} {:ident$}   ( {}=",
        state.cycle,
        "",
        color(param.name, Some(param.id.object), true),
        ident = ident
    );

    match result {
        None => {
            println!("<unknown>");
        }
        Some(vals) => {
            let mut sep = "";

            for v in vals {
                print!("{}", color(
                    &format!("{}0x{:x} ({})", sep, v, param.id),
                    Some(param.id.object), true
                ));
                sep = ", ";
            }
            println!("");
        }
    }

    Ok(())
}

fn dump_symbol<'a>(
    sym: &Option<&'a TockilatorSymbol<'a>>,
    fallback: &str,
) -> colored::ColoredString {
    let object = match sym {
        Some(sym) => match sym.goff {
            Some(goff) => Some(goff.object),
            None => None
        },
        None => None
    };

    color(match sym {
        Some(sym) => sym.demangled.borrow(),
        None => fallback,
    }, object, false)
}

fn flowtrace(
    tockilator: &mut Tockilator,
    file: &str,
    config: &TockilatorConfig,
) -> Result<(), Box<dyn Error>> {
    let mut entry = true;
    let mut inlined: Vec<TockilatorGoff> = vec![];

    tockilator.tracefile(file, |state| -> Result<(), Box<dyn Error>> {
        let f: &str = &format!("{:x}", state.pc);
        let base = state.stack.iter().fold(0, |sum, &val| sum + val.1 + 2);
        let mut ident = base;
        let mut output = false;
        let sigil = 2;
        let mut skip = false;

        if let (Some(start), Some(end)) = config.range {
            if state.cycle < start || state.cycle > end {
                skip = true
            }
        }

        if entry && !skip {
            println!(
                "{} {:ident$}-> {}",
                state.cycle,
                "",
                dump_symbol(&state.symbol, f),
                ident = ident,
            );

            if !config.noparams {
                for param in state.params.iter() {
                    dump_param(state, param, ident)?;
                }
            }

            output = true;
        }

        for i in 0..state.inlined.len() {
            if i < inlined.len() && inlined[i] == state.inlined[i].id {
                continue;
            }

            if skip {
                continue;
            }

            ident = base + (i * 2) + sigil;

            println!(
                "{} {:ident$} | {}",
                state.cycle,
                "",
                color(
                    &format!(
                        "{} ({})",
                        state.inlined[i].name,
                        state.inlined[i].id
                    ), Some(state.inlined[i].id.object), false
                ),
                ident = ident,
            );

            if let Some(params) = state.iparams.get(&state.inlined[i].id) {
                if !config.noparams {
                    for param in params.iter() {
                        dump_param(state, param, ident)?;
                    }
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

        match state.inst.op {
            rv_op::jalr | rv_op::c_jalr | rv_op::jal | rv_op::c_jal => {
                entry = true;
            }
            _ => {
                entry = false;
            }
        }

        if skip {
            return Ok(());
        }

        if state.inst.op == rv_op::ecall {
            ident = base;

            println!("{} {:ident$}=> {}",
                state.cycle,
                "",
                "SYSTEMCALL".white().bold(),
                ident = ident,
            );
            output = true;
        }

        if state.inst.op == rv_op::mret {
            ident = base;

            println!("{} {:ident$}<= {}",
                state.cycle,
                "",
                "SYSTEMCALL".white().bold(),
                ident = ident,
            );
            output = true;
        }


        if state.inst.op == rv_op::ret {
            ident = base;

            println!(
                "{} {:ident$}<- {}",
                state.cycle,
                "",
                dump_symbol(&state.symbol, f),
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

        if output && config.allreg {
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

        Ok(())
    })
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut range = (None, None);

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
            Arg::with_name("no-inline")
                .short("I")
                .help("Disable inline processing"),
        )
        .arg(
            Arg::with_name("no-params")
                .short("P")
                .help("Disable parameter processing"),
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
        .arg(
            Arg::with_name("cycles")
                .short("c")
                .number_of_values(1)
                .takes_value(true)
                .value_name("CYCLES")
        )        
        .arg(Arg::with_name("tracefile").required(true).index(1))
        .get_matches();

    let mut tockilator = Tockilator::default();

    if matches.is_present("no-inline") {
        tockilator.inline(false);
    } else {
        tockilator.inline(true);
    }

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

    if let Some(cycles) = matches.value_of("cycles") {
        let pair: Vec<&str> = cycles.split("-").collect();
        let start = usize::from_str(pair[0]).map_err(|_|
            fatal!("cycle must be an integer")
        ).unwrap();

        if pair.len() > 2 {
            fatal!("cycle range can only have two values");
        }

        if pair.len() > 1 {
            let end = usize::from_str(pair[1]).map_err(|_|
                fatal!("cycle range must be an integer")
            ).unwrap();

            if end < start {
                fatal!("ending cycle must be greater than starting cycle")
            }

            range = (Some(start), Some(end))
        } else {
            range = (Some(start), Some(start))
        }
    }

    let config = TockilatorConfig {
        range: range,
        verbose: false,
        allreg: matches.is_present("allreg"),
        noparams: matches.is_present("no-params"),
    };

    if matches.is_present("flowtrace") {
        flowtrace(&mut tockilator, file, &config)?;
    } else {
        tockilator.tracefile(file, dump)?;
    }

    Ok(())
}
