# Tockilator

## Deducing Tock execution flows from Ibex Verilator traces

[![Build Status](https://travis-ci.org/oxidecomputer/tockilator.svg?branch=master)](https://travis-ci.org/oxidecomputer/tockilator)

Tockilator is a Rust program that consumes the output from the tracer module of
an <a href="https://github.com/lowRISC/ibex">Ibex RISC-V core</a> running under
<a href="https://www.veripool.org/wiki/verilator">Verilator</a> along with an
ELF file (or ELF files) that correspond to the running software (e.g., boot
loader, operating system and application) and symbolically interprets the
instruction trace to provide a view of execution flow.

Here is an example of Tockilator output for 
<a href="https://github.com/tock/libtock-rs">libtock-rs</a>
running the <a href="https://github.com/tock/libtock-rs/blob/master/examples/hello_world.rs">hello_world</a> example:

```
~/tockilator/example/libtock-rs/hello_world$ ../../../target/release/tockilator -e ./hello_world.elf ./trace_core_00000000.log
147794         => SYSCALL MEMOP operand=0 (brk) arg0=0x10003004
165897         <= SYSCALL MEMOP operand=0 (brk) arg0=0x10003004
165900         => SYSCALL MEMOP operand=10 (update-stack-start) arg0=0x10003400
181174         <= SYSCALL MEMOP operand=10 (update-stack-start) arg0=0x10003400
181177         => SYSCALL MEMOP operand=11 (update-heap-start) arg0=0x10003004
195872         <= SYSCALL MEMOP operand=11 (update-heap-start) arg0=0x10003004
195879         -> rust_start
195894            | copy_nonoverlapping<u8> (GOFF 0x7ee8)
195898            -> memcpy
195913            <- memcpy
195921            | write<u8> (GOFF 0x8006)
195922            | add_usize (GOFF 0x7f69)
195922              | checked_add (GOFF 0x7f94)
195922                | overflowing_add (GOFF 0x7fb6)
195923            | lt (GOFF 0x7fdd)
195928            | write<u8> (GOFF 0x8006)
195929            | add_usize (GOFF 0x7f69)
195929              | checked_add (GOFF 0x7f94)
195929                | overflowing_add (GOFF 0x7fb6)
195930            | lt (GOFF 0x7fdd)
195935            | write<u8> (GOFF 0x8006)
195936            | add_usize (GOFF 0x7f69)
195936              | checked_add (GOFF 0x7f94)
195936                | overflowing_add (GOFF 0x7fb6)
195937            | lt (GOFF 0x7fdd)
195942            | write<u8> (GOFF 0x8006)
195943            | add_usize (GOFF 0x7f69)
195943              | checked_add (GOFF 0x7f94)
195943                | overflowing_add (GOFF 0x7fb6)
195944            | lt (GOFF 0x7fdd)
195947            | set_brk (GOFF 0x8072)
195947              | memop (GOFF 0x808b)
195949                => SYSCALL MEMOP operand=0 (brk) arg0=0x10003404
212924                <= SYSCALL MEMOP operand=0 (brk) arg0=0x10003404
212927           -> main
212936             -> start<core::result::Result<(), libtock::result::TockError>>
212953                | block_on<core::result::Result<(), libtock::result::TockError>,async_support::executor::GeneratorFuture<generator-0>> (GOFF 0x3a4a)
212953                  | poll<async_support::executor::GeneratorFuture<generator-0>> (GOFF 0x3a6c)
212953                    | poll<generator-0> (GOFF 0x3a8f)
212953                      | {{closure}} (GOFF 0x3ab7)
212953                        | retrieve_drivers (GOFF 0x3acc)
212960                        | create_console (GOFF 0x3adc)
212966                    -> memset
213489                    <- memset
213492                | block_on<core::result::Result<(), libtock::result::TockError>,async_support::executor::GeneratorFuture<generator-0>> (GOFF 0x3a4a)
213492                  | poll<async_support::executor::GeneratorFuture<generator-0>> (GOFF 0x3a6c)
213492                    | poll<generator-0> (GOFF 0x3a8f)
213492                      | {{closure}} (GOFF 0x3ab7)
213492                        | create_console (GOFF 0x3adc)
213493                        | write_fmt<libtock::console::Console> (GOFF 0x3afe)
213515                    -> write
213559                       | iter<core::fmt::ArgumentV1> (GOFF 0x2145)
213559                         | add<core::fmt::ArgumentV1> (GOFF 0x2156)
213559                           | offset<core::fmt::ArgumentV1> (GOFF 0x2167)
213582                       | zip<core::slice::Iter<core::fmt::ArgumentV1>,core::slice::Iter<&str>> (GOFF 0x2311)
213582                         | new<core::slice::Iter<core::fmt::ArgumentV1>,core::slice::Iter<&str>> (GOFF 0x2322)
213582                           | new<core::slice::Iter<core::fmt::ArgumentV1>,core::slice::Iter<&str>> (GOFF 0x2333)
213582                             | min<usize> (GOFF 0x2343)
213582                               | min<usize> (GOFF 0x2353)
213582                                 | min_by<usize,fn(&usize, &usize) -> core::cmp::Ordering> (GOFF 0x2364)
213588                       | next<core::slice::Iter<core::fmt::ArgumentV1>,core::slice::Iter<&str>> (GOFF 0x237a)
213588                         | next<core::slice::Iter<core::fmt::ArgumentV1>,core::slice::Iter<&str>> (GOFF 0x2387)
213607                      -> write_str<libtock::console::Console>
213666                         | copy_from_slice<u8> (GOFF 0x592b)
213666                           | copy_nonoverlapping<u8> (GOFF 0x594d)
213672                          -> memcpy
213870                          <- memcpy
213871                         | copy_from_slice<u8> (GOFF 0x592b)
213871                           | copy_nonoverlapping<u8> (GOFF 0x594d)
213872                         | allow (GOFF 0x575d)
213872                           | allow (GOFF 0x579a)
213877                             => SYSCALL ALLOW driver=1 subdriver=1 addr=0x10003370 len=17
231428                             <= SYSCALL ALLOW driver=1 subdriver=1 addr=0x10003370 len=17
231428                         | allow (GOFF 0x575d)
231428                           | allow (GOFF 0x579a)
231434                         | subscribe<libtock_core::callback::Identity0Consumer,closure-0> (GOFF 0x535c)
231434                           | subscribe_fn (GOFF 0x5387)
231434                             | subscribe (GOFF 0x53bb)
231439                               => SYSCALL SUBSCRIBE driver=1 subdriver=1 callback=0x20030b40 data=0x100032c8
247187                               <= SYSCALL SUBSCRIBE driver=1 subdriver=1 callback=0x20030b40 data=0x100032c8
247187                         | subscribe<libtock_core::callback::Identity0Consumer,closure-0> (GOFF 0x535c)
247187                           | subscribe_fn (GOFF 0x5387)
247187                             | subscribe (GOFF 0x53bb)
247188                         | command (GOFF 0x540f)
247188                           | command (GOFF 0x5443)
247193                             => SYSCALL COMMAND driver=1 subdriver=1 arg0=0x11 arg1=0x0
263288                             <= SYSCALL COMMAND driver=1 subdriver=1 arg0=0x11 arg1=0x0
263288                         | command (GOFF 0x540f)
263288                           | command (GOFF 0x5443)
263295                         | poll_with_tls_context<libtock::futures::WaitForValue<closure-0>> (GOFF 0x5559)
263295                           | poll<libtock::futures::WaitForValue<closure-0>> (GOFF 0x556e)
263295                             | poll<(),closure-0> (GOFF 0x559f)
263304                         | yieldk (GOFF 0x55cb)
263304                           | yieldk (GOFF 0x55db)
263305                             => SYSCALL YIELD
295020                         | consume<closure-0> (GOFF 0x5ca9)
295023                           | {{closure}} (GOFF 0x5ccf)
295023                             | set<bool> (GOFF 0x5cdf)
295023                               | replace<bool> (GOFF 0x5cfc)
295023                                 | replace<bool> (GOFF 0x5d23)
295023                                   | swap<bool> (GOFF 0x5d41)
295023                                     | swap_nonoverlapping_one<bool> (GOFF 0x5d59)
295023                                       | copy_nonoverlapping<bool> (GOFF 0x5d71)
295030                      <- libtock_core::syscalls::subscribe::c_callback::hf280be3ae7cea60e
295041                       | poll_with_tls_context<libtock::futures::WaitForValue<closure-0>> (GOFF 0x5559)
295041                         | poll<libtock::futures::WaitForValue<closure-0>> (GOFF 0x556e)
295041                           | poll<(),closure-0> (GOFF 0x559f)
295046                       | drop<libtock_core::callback::CallbackSubscription> (GOFF 0x55ed)
295046                         | real_drop_in_place<libtock_core::callback::CallbackSubscription> (GOFF 0x55fd)
295046                           | drop (GOFF 0x560e)
295046                             | subscribe (GOFF 0x561d)
295051                               => SYSCALL SUBSCRIBE driver=1 subdriver=1 callback=0x0 data=0x0
310752                               <= SYSCALL SUBSCRIBE driver=1 subdriver=1 callback=0x0 data=0x0
310752                  | drop<libtock_core::shared_memory::SharedMemory> (GOFF 0x5664)
310752                    | real_drop_in_place<libtock_core::shared_memory::SharedMemory> (GOFF 0x5679)
310752                      | drop (GOFF 0x568f)
310752                        | allow (GOFF 0x56a3)
310757                          => SYSCALL ALLOW driver=1 subdriver=1 addr=0x0 len=0
328162                          <= SYSCALL ALLOW driver=1 subdriver=1 addr=0x0 len=0
328162                  | index<u8,core::ops::range::RangeFrom<usize>> (GOFF 0x5854)
328162                    | index<u8> (GOFF 0x5876)
328162                      | index<u8> (GOFF 0x5899)
328162                        | get_unchecked<u8> (GOFF 0x58bc)
328162                          | add<u8> (GOFF 0x58df)
328162                            | offset<u8> (GOFF 0x5902)
328199               <- write_str
328232             <- write
328234              | block_on<core::result::Result<(), libtock::result::TockError>,async_support::executor::GeneratorFuture<generator-0>> (GOFF 0x3a4a)
328234                | poll<async_support::executor::GeneratorFuture<generator-0>> (GOFF 0x3a6c)
328234                  | poll<generator-0> (GOFF 0x3a8f)
328234                    | {{closure}} (GOFF 0x3ab7)
328234                      | write_fmt<libtock::console::Console> (GOFF 0x3afe)
328246           <- start<core::result::Result<(), libtock::result::TockError>>
328254         <- main
328255          | yieldk (GOFF 0x80be)
328255            | yieldk (GOFF 0x80ce)
328256              => SYSCALL YIELD
```

As an example of going to more depth, Tockilator can also be given particular
cycle ranges with the `-c` option, and be told to print parameters (such as
they can be determined) with the `-p` option -- and if provided multiple
ELF files as input, it can follow code flow across privilege boundaries:

```
~/tockilator/example/libtock-rs/hello_world$ ../../../target/release/tockilator -e ./hello_world.elf -e ./opentitan.elf -c 213607-214200 -p ./trace_core_00000000.log 
213607                            -> write_str<libtock::console::Console>
213666                               | copy_from_slice<u8> (GOFF 0x592b)
213666                                 | copy_nonoverlapping<u8> (GOFF 0x594d)
213672                                -> memcpy
213870                                <- memcpy
213871                               | copy_from_slice<u8> (GOFF 0x592b)
213871                                 | copy_nonoverlapping<u8> (GOFF 0x594d)
213872                               | allow (GOFF 0x575d)
213872                                 | allow (GOFF 0x579a)
213877                                   => SYSCALL ALLOW driver=1 subdriver=1 addr=0x10003370 len=17
213980                               | switch_to_process (GOFF 0x81f4 in object 1)
213980                                 ( state=0x10000c00 (GOFF 0x820b in object 1)
214045                               -> from
214045                                  ( csr_val=0x8 (GOFF 0x6fea0 in object 1)
214054                                 -> from
214054                                    ( val=0x8 (GOFF 0x6fe2d in object 1)
214065                                    | from_reason (GOFF 0x6fe3c in object 1)
214093                                 <- from
214101                               <- from
214102                               | switch_to_process (GOFF 0x81f4 in object 1)
214102                                 ( state=0x10000d38 (GOFF 0x820b in object 1)
214127                                 | arguments_to_syscall (GOFF 0x8222 in object 1)
214127                                   ( r0=0x1 (GOFF 0x8238 in object 1)
214127                                   ( r1=0x1 (GOFF 0x8241 in object 1)
214127                                   ( r2=0x10003370 (GOFF 0x824a in object 1)
214127                                   ( r3=0x11 (GOFF 0x8253 in object 1)
214142                               | set<*const u8> (GOFF 0x8293 in object 1)
214142                                 ( self=0x10004884 (GOFF 0x82a4 in object 1)
214142                                 | replace<*const u8> (GOFF 0x82ad in object 1)
214142                                   ( self=0x10004884 (GOFF 0x82be in object 1)
214142                                   | replace<*const u8> (GOFF 0x82c7 in object 1)
214142                                     ( dest=0x10004884 (GOFF 0x82d8 in object 1)
214142                                     | swap<*const u8> (GOFF 0x82e6 in object 1)
214142                                       ( x=0x10004884 (GOFF 0x82f7 in object 1)
214142                                       | swap_nonoverlapping_one<*const u8> (GOFF 0x8305 in object 1)
214142                                         ( x=0x10004884 (GOFF 0x8316 in object 1)
214142                                         | copy_nonoverlapping<*const u8> (GOFF 0x8324 in object 1)
214142                                           ( count=0x1 (GOFF 0x8343 in object 1)
214142                                           ( dst=0x10004884 (GOFF 0x833a in object 1)
214150                              -> memcpy
```

## Getting Verilator

For an example of how to generate Verilator output for an Ibex core,
see (for example) the documentation for 
<a href="https://docs.opentitan.org/doc/ug/getting_started_verilator/">configuring Verilator for OpenTitan</a>.  

## ELF binaries

For anything written in Rust (or presumably, C++), it is important that
<a href="xxx">DWARF information</a> be included in the ELF binary, as
Tockilator will use this information to find
(and display) inlined functions and parameter information.
When building release builds in Rust, this information is not generated by
default; to generate it, add the "debug = true" to the "

Adding this information will result in a much larger binary, but it is stored in
unloadable sections:  it does not at all affect what is loaded into the 
embedded system.

```
cargo run -- \
    -e ../opentitan/build-bin/sw/device/sim-verilator/boot_rom/boot_rom.elf \
    -e ../tock/boards/opentitan/target/riscv32imc-unknown-none-elf/release/opentitan.elf \
    trace_core_00000000.log | less
```


Example output
