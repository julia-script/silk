# Verification

The pinned LLVM 22.1.8 / Darwin SDK 15.5 / GNU toolchain supplies are recorded in `supplies.json`.
`packages/compiler/conformance/artifact-roots/run.mjs` passed all twelve designated lanes:
Darwin ARM64 and GNU/Linux ARM64/x86-64, debug/optimized, custom source runtime/no runtime.
Each lane validates all three semantic library/object forms, verifies LLVM bitcode, emits a real
object, inspects private retention and absence of unrelated public functions, creates an archive
and shared module, and independently compiles/executes a C consumer against each. Results are in
`results.json`. LTO is explicitly rejected. This does not claim downstream archive extraction of
an unreferenced member, physical supply discovery, or replacement of hosted startup policy.

Repository typecheck and format checks passed. The lint pass found four unnecessary JavaScript
quote escapes in the new conformance runner; these were corrected. The full test/check/release
sequence is in progress and is not yet claimed complete.

Focused compiler tests prove custom application binding, private retention, all three requirement
scopes including foreign data imports, duplicate collapse, all-origin conflicts, admitted binding
choices, runtime/loader request identity, compiler/stage identity and ordered physical inputs.
