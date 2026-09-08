# Source-owned native process conformance

Run after building the compiler with the standard platform-supply environment:

```sh
node packages/compiler/conformance/native-process/run.mjs
SILK_PROCESS_FAULTS=true node packages/compiler/conformance/native-process/run.mjs
```

Required variables are `SILK_SUPPLY_TARGET`, `SILK_SUPPLY_CLANG`, `SILK_SUPPLY_AR`,
`SILK_SUPPLY_LINKER`, `SILK_SUPPLY_READOBJ`, and `SILK_SUPPLY_ROOT`. GNU also requires
`SILK_SUPPLY_GCC` and `SILK_SUPPLY_IMAGE`. `SILK_SUPPLY_OUTPUT` optionally changes
the default `.scratch/native-process` output directory. CI supplies all three
admitted targets and runs both commands. Every command tests debug and optimized
objects against LLVM 22.1.8 and the header hashes in
`openspec/changes/complete-native-runtime-migration/process-supplies.json`.

The real receiver starts two native threads together. Each invokes the compiled
source provider, checking separate captures, a missing executable versus exit
127, signal status, stdin EOF, explicit environment and cwd, non-UTF-8 byte
arguments, and simultaneous 128 KiB stdout/stderr. These execute real selected
platform calls. The C receiver does not implement the process provider.

The fault receiver substitutes foreign operations while calling the same
compiled source provider. It sweeps every foreign-call failure ordinal in the
successful parent lifecycle and every source allocation ordinal. It checks no
live descriptor or native handle remains, every acquired child is reaped once,
and secondary cleanup failures preserve the original error. It supplies EINTR,
low-numbered descriptors, stopped/continued wait statuses, and GNU fragmented,
truncated and malformed startup notices. Its GNU fork substitution returns a
parent PID: child-side dup/exec/notice-write fault injection is not covered by
that ordinal sweep.

The GNU-only `SILK_PROCESS_CHILD_FAULTS=true` mode runs real forked children.
Its receiver injects failures into notice-fd duplication, CLOEXEC setup, all three
standard-fd duplications, cwd change and exec. Startup notices are delivered in
one-byte writes, with additional EINTR, zero-progress and partial-then-failure cases.
Shared receiver observations prove the injection was reached; the parent checks
unchanged descriptor flags and no remaining child after each call. The parent may
kill an owned failed child after receiving its complete notice, so the check permits
that race and verifies exit 127 whenever the child reaches its own terminal call.

A completely unwritten error notice produces EOF, which cannot distinguish failed
startup from successful exec; the outcome then carries exit 127. A partial notice
is rejected with the provider's malformed-protocol code. This transport limitation
is tested explicitly. The GNU child mode uses process-child-supplies.json for the
additional independent receiver headers. Required CI runs it on both GNU targets.

Reports retain tool versions, pinned headers, profiles, physical link inputs,
independent C translation, object symbols/relocations, disassembly and execution
output. Full reports can be large. The compact committed process-conformance
record identifies local evidence without copying full machine paths into the
language reference. CI execution must be observed separately from local results.
