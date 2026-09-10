# Verification

The pinned LLVM 22.1.8 / Darwin SDK 15.5 / GNU toolchain supplies are recorded in `supplies.json`.
`packages/compiler/conformance/artifact-roots/run.mjs` passed all twelve designated lanes:
Darwin ARM64 and GNU/Linux ARM64/x86-64, debug/optimized, custom source runtime/no runtime.
Each lane validates all three semantic library/object forms, verifies LLVM bitcode, emits a real
object, inspects private retention and absence of unrelated public functions, creates an archive
and shared module, and independently compiles/executes a C consumer against each. Results are in
`results.json`. LTO is explicitly rejected. This does not claim downstream archive extraction of
an unreferenced member, physical supply discovery, or replacement of hosted startup policy.

Before rebasing, repository typecheck, format, lint, all 2,336 compiler tests and all 321 shared
native acceptance cases passed. After integration with JUL-135 on origin/main `0ee2ed40`, the entire
stack passed typecheck, format:check, lint, test, check and release:candidate in order. This final
integrated run covers JUL-125; no standalone JUL-125 check/release run is claimed. The final compiler
suite contains 2,343 tests, and all 321 native acceptance cases actually executed. All twelve
artifact-root conformance lanes passed again with results identical to the committed record.

The first full run exposed a language-server timeout after a manifest switched to an application
that was not open. Selection attempted to publish the new application before loading it. A minimal
ProjectAnalysis regression reproduced the lost-root defect; selection now loads the explicit
application independently of open document roots. The regression and original stdio navigation test
both pass. The CLI diagnostic expectation was updated for the generalized invocation contract.

A u64-only/empty-library backend probe exposed an assumption that selected source always includes
i32. LLVM now constructs its internal control ABI type independently of source layout. The existing
empty-library test now proves actual LLVM emission. The invalid generic retention fixture was
changed to avoid an unrelated ownership error before the configured-role check.

Focused compiler tests prove custom application binding, private retention, all three requirement
scopes including foreign data imports, duplicate collapse, all-origin conflicts, admitted binding
choices, runtime/loader request identity, compiler/stage identity and ordered physical inputs.

Final integrated verification on source head `a239100e` (above origin/main `0ee2ed40`) passed all six
required repository gates. The final stack includes 2,343 compiler tests, 321 actually executed
native acceptance cases, 159 LSP tests, 89 CLI tests, 17 repository policy checks and 10 packed
release-candidate checks. See `../native-assembly-entry-contracts/verification.md` for integration
fixes, logs and the two unrelated baseline OpenSpec delta-validation failures. Submitted through
`gh stack` as draft PR #364; no merge is claimed.
