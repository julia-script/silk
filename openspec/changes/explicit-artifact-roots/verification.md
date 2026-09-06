# Verification

The pinned LLVM 22.1.8 / Darwin SDK 15.5 / GNU toolchain supplies are recorded in `supplies.json`.
`packages/compiler/conformance/artifact-roots/run.mjs` passed all twelve designated lanes:
Darwin ARM64 and GNU/Linux ARM64/x86-64, debug/optimized, custom source runtime/no runtime.
Each lane validates all three semantic library/object forms, verifies LLVM bitcode, emits a real
object, inspects private retention and absence of unrelated public functions, creates an archive
and shared module, and independently compiles/executes a C consumer against each. Results are in
`results.json`. LTO is explicitly rejected. This does not claim downstream archive extraction of
an unreferenced member, physical supply discovery, or replacement of hosted startup policy.

Repository typecheck, format and lint passed. All 2,336 compiler tests passed; the shared native
acceptance and aggregate check/release sequence is still in progress.

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
