use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader};

struct RVRegs {
    x: [u32; 32],
}

fn verilog_parse(line: &str) -> Option<(&str, &str, &str, &str, &str, &str)> {
    let mut fields = line.match_indices("\t");
    let time = fields.next()?.0;
    let cycle = fields.next()?.0;
    let pc = fields.next()?.0;
    let ibin = fields.next()?.0;
    let mut iasm = fields.next()?.0;
    fields.next().map(|operands| iasm = operands.0);

    Some((
        &line[..time].trim(),
        &line[time + 1..cycle].trim(),
        &line[cycle + 1..pc].trim(),
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
                return false;
            }
            if eff.find("=").is_some() {
                return true;
            }
            assert!(eff.find(":").is_some());
            false
        })
        .map(|eff| {
            println!("  eff={}", eff);
            let eq = eff.find("=").expect("missing =");
            (
                eff[1..eq].parse::<u32>().unwrap(),
                u32::from_str_radix(&eff[eq + 1 + 2..], 16).unwrap(),
            )
        })
        .collect()
}

fn main() -> std::io::Result<()> {
    let file = BufReader::new(File::open(env::args().nth(1).expect("specify input file"))?);
    let mut regs = RVRegs { x: [0; 32] };
    regs.x[1] = 7;
    for line in file.lines().skip(1) {
        match line {
            Ok(ll) => {
                process_line(&ll, &mut regs);
                ()
            }
            Err(_err) => println!("bad"),
        }
    }
    Ok(())
}

fn process_line(ll: &str, regs: &mut RVRegs) {
    println!("{}", ll);
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

    println!(
        "time={}, cycle={} pc={} ibin={} iasm={} effects={}",
        time, cycle, pc, ibin, iasm, effects,
    );

    for eff in parse_effects(effects) {
        regs.x[eff.0 as usize] = eff.1;
    }

    for (x, v) in regs.x.iter().enumerate() {
        println!("  %x{}: 0x{:x}", x, v);
    }
}
