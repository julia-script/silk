# Verification

The designated native runner passed all four Linux x86-64/ARM64 debug/optimized lanes with LLVM
22.1.8, the pinned GNU compiler/container images and verified UAPI/header hashes. `results.json`
records object/source hashes and actual naked instructions. Each lane verifies both textual LLVM
IR and bitcode, emits and disassembles a target object, and compiles independent C fixtures.

The fixtures prove fixed/tied registers across seven operands, raw pointer input and result lanes,
C-observed memory writes and reads, getpid using the UAPI syscall number compared with libc, and
terminal assembly caught by a C SIGILL/SIGTRAP handler. A separate executable uses the naked symbol
as its actual ELF entry and passes the incoming loader stack to an independent C probe; the probe
checks argc/argv and exits 23. Disassembly requires the authored stack-forwarding instruction first
and excludes compiler prologues, spills, probes or entry wrappers. This is a machine contract test,
not the downstream no-libc startup/allocator/runtime implementation. LTO is rejected explicitly.

The first debug object exposed a compiler-created branch before the naked fragment. Naked bodies
now emit one LLVM block directly. Another fixture exposed elimination of a terminal side-effecting
assembly operation when its LLVM call claimed memory(none). Observable effects now occupy LLVM's
inaccessible-memory lane, as in the pinned Rust lowering; data memory effects remain independent.
The runner catches the expected trap signal and exits 74, avoiding expensive container core dumps.
Textual IR verification also exposed an invalid `other:` memory spelling: the LLVM renderer now
expresses default memory effects and target-state exclusions consistently with bitcode decoding.

Assembly metadata never receives runtime layout or emitted static storage. Source analysis tests
cover literal and lane validation, fixed/tied conflicts, clobbers, malformed templates, static and
unsupported target admission, machine body restrictions and unavailable instrumentation/unwind.
MIR independently validates normalized contracts, terminal region shape and naked bodies.
Unit type arguments now parse and resolve in call generic lists, enabling the unit result form.

The first integrated full gate exposed an initialization cycle in CLI builds: source declaration
validation imported MIR planning, which reached instance-key initialization before declaration
collection completed. A single-file CLI build reproduced the failure. Source contracts now have
no MIR runtime dependency; `NativeAssemblyPlanning` owns retained-operation and profile validation.
The rebuilt compiler passes the original CLI reproducer. Repository checks continue; no full check
or release result is claimed yet.
