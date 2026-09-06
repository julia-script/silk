# Native pointer boundary verification

## Implemented contract

The parser, type representation, semantic surfaces, MIR verifier and LLVM lowering retain
pointee, access, nullability, single/many extent, alignment and address space. Ordinary
`silk.pointer` wrappers expose the initial checked-null and unsafe access subset. `silk.output`
provides Copy-only owning uninitialized and initialized states over RawBuffer and Slot.
The compiler knows only sealed Intrinsic operations; it does not recognize these library actors.

Target primitive descriptions drive scalar storage and existing external record/array layout.
Independent optimized C execution exposed a pre-existing missing narrow-integer C extension:
Darwin ARM64 and GNU x86-64 now carry signext/zeroext through semantic classification to LLVM
foreign declarations, direct calls and export thunks; GNU ARM64 retains its AAPCS64 convention.

## Independent native evidence

The committed [conformance report](conformance-report.json) records six successful runs:
debug and release on aarch64-apple-darwin, aarch64-unknown-linux-gnu and
x86_64-unknown-linux-gnu. Every run compiled separate C and Silk objects, linked them,
inspected the object architecture/symbols/relocations and executed the resulting program.
The fixtures cover scalar arguments in both directions, narrow scalar results, an existing
function pointer passed back through C, nested nullable pointers, separate buffer lengths,
C-observed writes, unaligned accesses, external record/array layout and foreign output storage.

Darwin used Clang and LLD 22.1.8 with the explicit macOS 15.5 SDK and deployment target 11.0.0.
GNU ARM64 and x86-64 ran in the pinned Debian/GCC/glibc images; the report records image ids,
compiler/linker versions, fixture hashes and object hashes. The runner verifies the package
versions and header hashes in [supplies.json](supplies.json) before exercising each lane.

The missing-supply check selected `/nonexistent/jul123/clang` and failed with ENOENT and exit 1.
No lane was reported as skipped or passed. Explicit LTO configuration is rejected and recorded
in the report. The runner uses Effect filesystem, configuration and scoped subprocess services.

Clang reports the pre-existing warning `ignoring debug info with an invalid version (0)` for
Silk debug bitcode. Both debug and optimized machine-code boundary cases execute successfully;
this evidence does not claim that debug symbols are usable. The baseline compile-unit builder
also omits the LLVM Debug Info Version module flag. Explicit Darwin deployment selection causes
Clang's target-triple override warning; the object and link use the same selected deployment.

## Analysis and structural checks

Focused tests passed for type identity/substitution/weakening, nested pointer invariance,
qualifier parsing and spans, serialization, scalar/layout facts, C ABI extensions, intrinsic
inventory, output ownership and MIR pointer formation. One analysis snapshot rejects implicit
nullability, access, alignment, extent and slice conversions with SEM0129 and exact spans.
Every public output operation explicitly refines its type parameter with `T: Copy`, as existing
container operations do; a nominal bound alone does not establish this call-site restriction. A
regression snapshot rejects move-only values at all five operations with SEM0083 and exact spans.
Output analysis distinguishes pre-initialization reads, unchanged state after foreign calls,
missing unsafe acknowledgement, and extraction after move. MIR checks establish that Slot.address
forms an address without reading or writing the output value. The shared native corpus includes
owned-output-storage and the migrated pointer-slice write case; both focused executions passed.

Strict OpenSpec validation and the 64-module documentation policy check passed. Generated
standard-library source, catalog, toolchain integrity and documentation artifacts were refreshed.

## Workspace gates

- `pnpm typecheck`: passed, 18 tasks.
- `pnpm format:check`: passed.
- `pnpm lint`: passed after replacing the new conformance runner's raw Node boundaries with Effect services.
- `pnpm test`: passed, 22 workspace tasks; 2,315 compiler tests across 211 files and all 319 serial native acceptance tests.
- `pnpm check`: passed, including all 17 script tests.
- `pnpm release:candidate`: passed, all 10 tests.

The first full compiler run passed 2,313 tests and failed the standard-library namespace inventory
for the new output module. Its manifest now names Uninitialized and exposes Initialized as an
alias for tooling discovery. This failure was introduced by the change. The repeated full run
passed every workspace and native acceptance test.

Initial integration failures from the changed intrinsic inventory and pointer symbol spellings
were introduced by this change and corrected together with their fixtures. The independent
Darwin optimized fixture found the narrow C extension bug described above; all six conformance
runs pass after that fix.

## CI evidence

[GitHub CI for implementation e8bef47a](https://github.com/julia-script/silk/actions/runs/34016745741)
passed all four compiler shards, all three native acceptance shards, macOS native-OS, browser,
validation and all ten release-candidate tests. The committed conformance report additionally
covers the designated pinned C/Silk boundary on all three native targets and both optimization modes.

[The JUL-121 CI run](https://github.com/julia-script/silk/actions/runs/34013255120) also passed.
Its validation job initially reached the existing 180-second aggregate documentation-test timeout;
the main-branch baseline took 171 seconds. An unchanged rerun passed, without increasing a timeout
or skipping a test.

The final output-bound review first reproduced acceptance of move-only values by the new wrapper.
Adding explicit `T: Copy` bounds at its five operations corrected that introduced gap, using the
same parameter refinement as existing container operations. The new regression and the valid
output-state tests pass. The earlier local native run was deliberately stopped so the complete
workspace gates could restart against that fix; no interrupted run is counted as a pass.

## Delivery

`gh stack` stack #360 contains draft [JUL-121 PR #357](https://github.com/julia-script/silk/pull/357)
below draft [JUL-123 PR #359](https://github.com/julia-script/silk/pull/359). JUL-123 is based on
`julia/jul-121-static-selection`; both PR descriptions include validation evidence. The final
verification-only commit updates this record and the completed task list without changing code.
