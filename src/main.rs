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
    params: bool,
    verbose: bool,
    allreg: bool,
    range: (Option<usize>, Option<usize>),
    unresolved: bool,
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
        (Some(2), false) => input.cyan(),
        (Some(2), true) => input.green(),
        _ => input.normal(),
    }
}

fn dump_id(
    id: &Option<TockilatorGoff>,
    config: &TockilatorConfig,
) -> String {
    match (id, config.verbose) {
        (Some(id), true) => { format!(" ({})", id) }
        _ => { "".to_string() }
    }
}

fn dump_param(
    state: &TockilatorState,
    config: &TockilatorConfig,
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
                    &format!(
                        "{}0x{:x}{}", sep, v,
                        dump_id(&Some(param.id), config)
                    ),
                    Some(param.id.object), true
                ));
                sep = ", ";
            }
            println!("");
        }
    }

    Ok(())
}

fn dump_syscall(
    state: &TockilatorState,
) -> String {
    let args = &state.regs[10..15];

    let driver = match args[1] {
        0x00000000 => "timer",
        0x00000001 => "console",
        0x00000002 => "leds",
        0x00000003 => "buttons",
        0x00000004 => "gpio",
        0x00000005 => "adc",
        0x00030000 => "simple_ble",
        0x00040001 => "rng",
        0x00060000 => "temperature",
        _ => "<unknown>"
    };

    let str = match args[0] {
        0 => "YIELD".to_string(),
        1 => format!(
                "SUBSCRIBE driver={} ({}) subdriver={} cb=0x{:x} data=0x{:x}",
                args[1], driver, args[2], args[3], args[4]
            ),
        2 => format!(
                "COMMAND driver={} ({}) subdriver={} arg0=0x{:x} arg1=0x{:x}",
                args[1], driver, args[2], args[3], args[4]
            ),
        3 => format!(
                "ALLOW driver={} ({}) subdriver={} addr=0x{:x} len={}",
                args[1], driver, args[2], args[3], args[4]
            ),
        4 => format!(
                "MEMOP operand={} ({}) arg0=0x{:x}",
                args[1],
                match args[1] {
                    0 => "brk",
                    1 => "sbrk",
                    2 => "process-memory-start",
                    3 => "process-memory-end",
                    4 => "process-flash-start",
                    5 => "process-flash-end",
                    6 => "grant-region-begin",
                    7 => "writeable-flash-regions",
                    8 => "writeable-region-start",
                    9 => "writeable-region-end",
                    10 => "update-stack-start",
                    11 => "update-heap-start",
                    _ => "<unknown>"
                },
                args[2]
            ),
        _ => "UNKNOWN".to_string(),
    };

    format!("SYSCALL {}", str)
}

fn dump_symbol<'a>(
    sym: &Option<&'a TockilatorSymbol<'a>>,
    fallback: &str,
    config: &TockilatorConfig,
) -> colored::ColoredString {
    let object = match sym {
        Some(sym) => match sym.goff {
            Some(goff) => Some(goff.object),
            None => None
        },
        None => None
    };

    match sym {
        Some(sym) => {
            let dem: &str = &sym.demangled.borrow();
            color(
                &format!("{}{}", dem, dump_id(&sym.goff, config)),
                object,
                false
            )
        },
        None => color(fallback, object, false)
    }
}

fn flowtrace(
    tockilator: &mut Tockilator,
    file: &str,
    config: &TockilatorConfig,
) -> Result<(), Box<dyn Error>> {
    let mut entry = true;
    let mut inlined: Vec<TockilatorGoff> = vec![];

    let mut mret = false;
    let mut ecall: Option<(u32, usize)> = None;
    let mut syscall: Option<String> = None;
    let mut stack: Vec<(u32, usize, bool)> = vec![];

    tockilator.tracefile(file, |state| -> Result<(), Box<dyn Error>> {
        let f: &str = &format!("{:x}", state.pc);
        let base = stack.iter().fold(0, |sum, &val| {
            match val.2 {
                false => sum,
                true => sum + val.1 + 2
            }
        });
        let mut ident = base;
        let mut output = false;
        let sigil = 2;

        let skip = match config.range {
            (Some(start), Some(end)) => {
                state.cycle < start || state.cycle > end
            },
            (Some(start), None) => {
                state.cycle < start
            },
            (None, Some(end)) => {
                state.cycle > end
            },
            (None, None) => {
                false
            }
        };

        if entry && !skip && (!state.symbol.is_none() || config.unresolved) {
            println!(
                "{} {:ident$}-> {}",
                state.cycle,
                "",
                dump_symbol(&state.symbol, f, config),
                ident = ident,
            );

            if config.params {
                for param in state.params.iter() {
                    dump_param(state, config, param, ident)?;
                }
            }

            output = true;
        }

        if mret && !skip && ecall.is_some() &&
          state.pc == ecall.unwrap().0 + 4 {
            println!("{} {:ident$} <= {}",
                state.cycle,
                "",
                syscall.as_ref().unwrap().white().bold(),
                ident = ecall.unwrap().1,
            );
            output = true;
        }

        mret = state.inst.op == rv_op::mret;

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
                        "{}{}",
                        state.inlined[i].name,
                        dump_id(&Some(state.inlined[i].id), config),
                    ), Some(state.inlined[i].id.object), false
                ),
                ident = ident,
            );

            if let Some(params) = state.iparams.get(&state.inlined[i].id) {
                if config.params {
                    for param in params.iter() {
                        dump_param(state, config, param, ident)?;
                    }
                }
            }

            output = true;
        }

        if !skip && state.inst.op == rv_op::ecall {
            syscall = Some(dump_syscall(&state));

            if state.inlined.len() > 0 {
                ident = base + (state.inlined.len() * 2) + sigil;
            } else {
                ident = base + 1;
            }

            println!("{} {:ident$} => {}",
                state.cycle,
                "",
                syscall.as_ref().unwrap().white().bold(),
                ident = ident,
            );
            output = true;
            ecall = Some((state.pc, ident));
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
                stack.push((
                    state.pc,
                    state.inlined.len(),
                    !skip && (!state.symbol.is_none() || config.unresolved)
                ));
            }
            rv_op::ret => {
                entry = false;
                stack.pop().or_else(|| panic!("underrun"));
            }
            _ => {
                entry = false;
            }
        }

        if skip {
            return Ok(());
        }

        if state.inst.op == rv_op::ret &&
          (!state.symbol.is_none() || config.unresolved) {
            ident = base;

            println!(
                "{} {:ident$}<- {}",
                state.cycle,
                "",
                dump_symbol(&state.symbol, f, config),
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
            Arg::with_name("cycles")
                .short("c")
                .number_of_values(1)
                .takes_value(true)
                .value_name("CYCLES")
        )
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
            Arg::with_name("dump")
                .short("d")
                .help("dump minimally decorated output"),
        )
        .arg(
            Arg::with_name("no-inline")
                .short("I")
                .help("Disable inline processing"),
        )
        .arg(
            Arg::with_name("params")
                .short("p")
                .help("Show parameters"),
        )
        .arg(
            Arg::with_name("registers")
                .short("r")
                .help("shows registers"),
        )
        .arg(
            Arg::with_name("dryrun")
                .short("n")
                .help("do not process trace file"),
        )
        .arg(
            Arg::with_name("unresolved")
                .short("u")
                .help("display unresolved functions"),
        )
        .arg(
            Arg::with_name("verbose")
                .short("v")
                .help("display verbose identifiers"),
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
            if pair[1].len() == 0 {
                range = (Some(start), None);
            } else {
                let end = usize::from_str(pair[1]).map_err(|_|
                    fatal!("cycle range must be an integer")
                ).unwrap();

                if end < start {
                    fatal!("ending cycle must be greater than starting cycle");
                }

                range = (Some(start), Some(end));
            }
        } else {
            range = (Some(start), Some(start));
        };
    }

    let config = TockilatorConfig {
        range: range,
        verbose: matches.is_present("verbose"),
        allreg: matches.is_present("registers"),
        params: matches.is_present("params"),
        unresolved: matches.is_present("unresolved"),
    };

    if matches.is_present("dump") {
        tockilator.tracefile(file, dump)?;
    } else {
        flowtrace(&mut tockilator, file, &config)?;
    }

    Ok(())
}
