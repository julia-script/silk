# Synchronous native callback evidence

## Authorities pinned before implementation

`supplies.json` was written before callback fixture implementation. Its SHA-256 entries identify the
actual retrieved bytes, and its `reuses` field points to the unchanged JUL-124 ABI, compiler,
linker, SDK, C++ runtime and GNU image pins. New stdlib and pthread header hashes are checked by the
existing native harness in addition to those prior supplies.

POSIX qsort specifies comparator ordering and access restrictions, but does not explicitly promise
same-thread execution. The selected glibc 2.36 `msort.c`/`qsort.c` and Apple Libc revision
`71bbe350ab79eef58113991d817ccc6165061a64` qsort/heapsort implementations call comparators directly
within their sorting call stacks. This source evidence supports the binding author's unsafe
invocation assertion. It does not identify the installed Apple binary with that source release, or
the Debian-patched libc package with unpatched upstream source. The executable fixture separately
observes the installed implementations with pthread identity and a live dynamic-extent marker.
The finite run cannot prove every future comparator invocation or establish arbitrary raw-pointer
provenance.

Zig's pinned C ABI fixture informs exact mixed scalar/pointer signatures. Rust's pinned
`abort_unwinding_calls.rs` explicitly introduces termination where a forbidden unwind would cross
a boundary; `ffi_unwind_calls.rs` supplies the corresponding ABI distinction. Silk deliberately
uses its existing fatal personality during exception search, which prevents an outer C++ catch
from intercepting the exception. Neither prior implementation defines Silk's loan semantics.

## Independent fixture and execution cost

The existing `pnpm --filter @silklang/compiler test:foreign-contracts` harness now compiles
`callbacks.c` as a separate C11 object and links it with the Silk object and the C++ caller. A runtime
C address crosses C → Silk → C with signed 8-bit, unsigned 16-bit, signed 64-bit, float, double, and
mutable-pointer parameters and a 64-bit return. The first native target reenters Silk using a second
C address, checking independent inner and outer storage. A real libc qsort invokes a Silk comparator
and checks thread identity and enclosing dynamic extent. A separate process supplies a throwing C++
address to the Silk indirect call and must terminate before its enclosing catch.

These extend one existing compilation per target/profile, sharing the produced object and success
process. Only the distinguishable indirect-throw boundary adds a process per target/profile. The
harness is opt-in; incremental default-suite execution cost is zero. Existing direct-contract,
no-return and direct-throw assertions remain. Historical JUL-124 results are unchanged.

`callbacks.c` passes standalone `clang -std=c11 -Wall -Wextra -Werror -pthread -c` and the harness
passes `node --check`. Full execution passed with `pnpm --filter @silklang/compiler test:foreign-contracts`: all six Darwin ARM64, GNU ARM64 and GNU x86-64 debug/release rows are recorded in `results.json`. Both direct and indirect C++ throw processes terminated with signal exits 132/133 before their outer catches; the success process confirmed mixed scalar/pointer calls, same-thread dynamic extent, nested independent storage and libc sorting. The harness verified emitted LLVM IR/bitcode plus object guard/personality and unwind sections.

## Source and interface verification

The existing CAbi test file shares one valid analysis snapshot for lifetime-polymorphic export
conversion, genuine indirect MIR, independent reference access, semantic interface identity and
one release IR lowering. Its negative table distinguishes unsupported invocation promises, unsafe
acknowledgement, incompatible callback access and conflicting loans. Malformed semantic interfaces
and noncanonical ABI ordinal encodings are rejected. The existing pointer formatter case preserves
explicit lifetime binders and foreign clauses through normalized syntax and idempotent formatting.

The existing qsort native corpus case now uses one compilation and process to verify normal and
typed-failure wrapper cleanup. Both paths release their guard exactly once. This does not promise
cleanup after a fatal boundary violation.

Independent general review found and verified fixes for semantic/ABI transport admission and
canonical ordinal spelling. A separate test-economics review approved the test shape after
removing one repeated formatter parse. Equivalent focused measurements against isolated base
`03ec67f665ea8848f173a8b834ed025f9a7346b0` were:

| Existing test file/selection | Base test time | Change test time | Increment |
| ---------------------------- | -------------- | ---------------- | --------- |
| CAbi (median of three)       | 166 ms         | 207 ms           | 41 ms     |
| qsort native corpus          | 853 ms         | 2330 ms          | 1477 ms   |
| SyntaxFormatter              | 88 ms          | 114 ms           | 26 ms     |

Commands: `pnpm exec vitest run test/CAbi.test.ts`,
`SILK_NATIVE_CORPUS_CASE=foreign-libc-qsort-callback pnpm exec vitest run test/DriverNativeAcceptance.test.ts -t 'runs the native corpus'`,
and `pnpm exec vitest run test/SyntaxFormatter.test.ts`, run from each compiler package directory.
The estimated added default-suite test execution is 1.550 seconds; import/worker timing noise is
not treated as a performance claim. The opt-in native conformance additions add no default-suite
processes or target combinations.

The existing native-boundary fixture's `c_invoke` declaration now explicitly lists its callback
invocation promise. `pnpm --filter @silklang/compiler test:native-boundary` passed all six native
target/profile rows against the final rebuilt compiler. LLVM emitted nonfatal debug-info-version
and Darwin target-triple override warnings; every independent C executable completed successfully.
Historical native-boundary evidence files were not rewritten.

The existing export-thunk backend assertion now checks guarded invoke, landingpad and trap. Its
redundant second compilation and bitcode comparison were removed; the existing determinism canary
retains that global claim. Equivalent selection: `pnpm exec vitest run test/Backend.test.ts -t 'defines one C thunk'`. The independent test reviewer approved this migration.

## Repository gates

Final checks passed in the required order: `pnpm typecheck`, `pnpm format:check`, `pnpm lint`,
`pnpm test`, `pnpm check`, and `pnpm release:candidate`. The full compiler run passed 2,345
non-native tests and all 321 native acceptance tests. All 22 repository test/build tasks passed;
release-candidate validation passed all 10 tests.

Initial change-related failures were corrected before the final run: generated toolchain identity
needed regeneration, new expressions needed complete pipeline/lifetime handling, source fixtures
needed valid syntax, canonical contract expectations needed migration, three nested ternaries
violated lint, and the existing export-thunk assertion expected a plain call instead of guarded
invoke. No required check remains failed or skipped.
