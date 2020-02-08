# tockilator


```
cargo run -- \
    -e ../opentitan/build-bin/sw/device/sim-verilator/boot_rom/boot_rom.elf \
    -e ../tock/boards/opentitan/target/riscv32imc-unknown-none-elf/release/opentitan.elf \
    trace_core_00000000.log | less
```


Example output

```
...
         316566     158278 200001ae d83a     c.swsp     x14,48(x2)                    |_start_trap+0x26  x2:0x10000d20 x14:0x00000000 PA:0x10000d50 store:0x00000000 load:0x00000000
         316570     158280 200001b0 da3e     c.swsp     x15,52(x2)                    |_start_trap+0x28  x2:0x10000d20 x15:0x00000000 PA:0x10000d54 store:0x00000000 load:0x00000000
         316574     158282 200001b2 dc42     c.swsp     x16,56(x2)                    |_start_trap+0x2a  x2:0x10000d20 x16:0x10001dbc PA:0x10000d58 store:0x10001dbc load:0x00000000
         316578     158284 200001b4 de46     c.swsp     x17,60(x2)                    |_start_trap+0x2c  x2:0x10000d20 x17:0x10001ddf PA:0x10000d5c store:0x10001ddf load:0x00000000
         316582     158286 200001b6 649050ef jal        x1,20005ffe                  ->_start_trap+0x2e  x1=0x200001ba
         316584     158287 20005ffe 7179     c.addi16sp x2,-48                          |_start_trap_rust  x2:0x10000d20  x2=0x10000cf0
         316588     158289 20006000 d606     c.swsp     x1,44(x2)                       |_start_trap_rust+0x2  x2:0x10000cf0  x1:0x200001ba PA:0x10000d1c store:0x200001ba load:0x00000000
         316592     158291 20006002 d422     c.swsp     x8,40(x2)                       |_start_trap_rust+0x4  x2:0x10000cf0  x8:0x10000d70 PA:0x10000d18 store:0x10000d70 load:0x00000000
         316594     158292 20006004 1800     c.addi4spn x8,x2,48                        |_start_trap_rust+0x6  x8=0x10000d20
         316596     158293 20006006 34202573 csrrs      x10,mcause,x0                   |_start_trap_rust+0x8  x0:0x00000000 x10=0x8000000b
         316600     158295 2000600a 00003097 auipc      x1,0x3                          |_start_trap_rust+0xc  x1=0x2000900a
         316604     158297 2000600e 392080e7 jalr       x1,914(x1)                     ->_start_trap_rust+0x10  x1:0x2000900a  x1=0x20006012
         316606     158298 2000939c 1141     c.addi     x2,-16                            |<tock_registers::registers::LocalRegisterCopy<u32,rv32i::csr::mcause::mcause::Register> as rv32i::csr::mcause::McauseHelpers>::cause::h8f489736de9e1a35  x2:0x10000cf0  x2=0x10000ce0
         316610     158300 2000939e c606     c.swsp     x1,12(x2)                         |<tock_registers::registers::LocalRegisterCopy<u32,rv32i::csr::mcause::mcause::Register> as rv32i::csr::mcause::McauseHelpers>::cause::h8f489736de9e1a35+0x2  x2:0x10000ce0  x1:0x20006012 PA:0x10000cec store:0x20006012 load:0x00000000
         316614     158302 200093a0 c422     c.swsp     x8,8(x2)                          |<tock_registers::registers::LocalRegisterCopy<u32,rv32i::csr::mcause::mcause::Register> as rv32i::csr::mcause::McauseHelpers>::cause::h8f489736de9e1a35+0x4  x2:0x10000ce0  x8:0x10000d20 PA:0x10000ce8 store:0x10000d20 load:0x00000000
         316616     158303 200093a2 0800     c.addi4spn x8,x2,16                          |<tock_registers::registers::LocalRegisterCopy<u32,rv32i::csr::mcause::mcause::Register> as rv32i::csr::mcause::McauseHelpers>::cause::h8f489736de9e1a35+0x6  x8=0x10000cf0
         316618     158304 200093a4 800005b7 lui        x11,0x80000                       |<tock_registers::registers::LocalRegisterCopy<u32,rv32i::csr::mcause::mcause::Register> as rv32i::csr::mcause::McauseHelpers>::cause::h8f489736de9e1a35+0x8  x11=0x80000000
...
```
Those names are so long I almost wish I hadn't demangled them.