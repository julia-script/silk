# Native process-input conformance

Build the compiler, then run `node packages/compiler/conformance/native-host-input/run.mjs`
with the same SILK_SUPPLY tool, target, SDK/sysroot, GCC and container variables as
the native-process runner. Missing tools, header mismatches and missing GNU
execution images fail the run. LLVM 22.1.8 and LTO rejection are checked explicitly.

Each debug/optimized lane compiles the Silk snapshot implementation and an
independent C caller. The C translation checks int/size_t widths, the getcwd
signature and ERANGE. It passes non-UTF-8 argument/environment bytes, empty and
absent values, duplicate environment names and malformed entries. After source
capture it mutates the original buffers; lookups must still return the captured
bytes. Prefix tests check full lengths, short buffers and untouched tails. Cwd
lookup checks owned bytes against the prefix-copy operation.

The libc environment-only capture checks that the snapshot has zero arguments and
contains the selected variable. Darwin's independent C declaration checks the
`_NSGetEnviron` accessor; GNU reads the `environ` object. The fault receiver supplies
a deterministic environment vector so every snapshot allocation participates in
the refusal sweep. The fixture moves the captured entry snapshot into OsHostInput
and checks argument count and an owned argument through the portable service.

The source object may retain the existing allocator boundary (malloc/free), but
must have no generated host-input or entry/report runtime dependency. Object inspection
checks architecture and foreign relocations. Reports retain tool queries, headers,
source/artifact identities, C compilation, object inspection, disassembly and
actual execution on the selected architecture. The fault caller interposes malloc/free
and refuses every allocation in the successful path, checking that all acquired
allocations are released exactly once. It also forces cwd growth, host failure,
exhaustion of the growth bound and a contradictory success without a NUL terminator.
Required platform CI runs this
probe and uploads `.scratch/native-host-input`.

This probe does not yet prove startup wiring, concurrent/reentrant invocation, or hosted reporting.
Those remain part of JUL-130 admission and migration.
