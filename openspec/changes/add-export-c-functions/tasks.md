## 1. Rename and syntax

- [ ] 1.1 Rename `ModuleSummary.Export` to `PublicDeclaration` (interface, `_tag`, and the `exports` field to `publicDeclarations`) across `ModuleSummary.ts`, `WorkspaceInventory.ts`, `AutoImport.ts`, the compiler `index.ts`, LSP `WorkspaceCatalog.ts` and `Document.ts`, and their tests, and verify typecheck and those test files pass unchanged.
- [ ] 1.2 Add `export` to the complete-identifier keyword table as `ExportKeyword`, confirm no `.silk` source uses `export` as an identifier, and verify a lexer test distinguishes `export` from `exported`.
- [ ] 1.3 Parse `[pub] [static] [unsafe] export <text> [effect] fn name(params) [-> type] [as <text>] { body }` in the function-declaration path with an export marker node in the `extern` slot, sharing the `as` tail parser, requiring the body, and retaining `unsafe`, `static`, `effect`, type parameters, rows, and `where` for semantic rejection, and verify parser fixtures cover the renamed public form, a missing body with intact recovery, and modifier order.
- [ ] 1.4 Render the export marker and `as` tail in `SyntaxFormatter` before the ordinary block layout and verify an idempotent formatting test with irregular spacing.

## 2. Declaration facts and admission

- [ ] 2.1 Extend `DeclarationFact` with `foreignExport?: { abi: 'C'; symbol }`, collect it in `DeclarationCollection`, and verify the fact is present with the resolved symbol for both the named and renamed forms.
- [ ] 2.2 In `DeclarationCompletion`, reuse the foreign admission, restriction, and symbol helpers for exported headers and additionally reject `unsafe` and `static`, and verify one test per rejection (`string` result, `effect`, type parameter, failure row, `unsafe`, invalid symbol, `as "main"`) publishes no callable and no export.
- [ ] 2.3 Verify an exported function is callable from Silk as an ordinary function with no acknowledgement and that `pub` and `export` are independent (private export compiles; public non-export has no symbol).

## 3. Discovery and planning

- [ ] 3.1 Add export roots to `Instances.discover` when `target.kind === 'Native'`, after the entry in canonical module then declaration order, record `exports: [{ symbol, signature, key, declaration }]` on `Discovery`, and verify an uncalled export is discovered after `main`, that a `wasm32-unknown-unknown` target adds no roots, and that two fresh-process discoveries are byte-identical.
- [ ] 3.2 Copy `discovery.exports` onto `Mir.Module` in `Lower.lowerProgram`, add `foreignExports` to the cached emission header in `Driver`, and verify a cache-hit decode reproduces the export inventory.
- [ ] 3.3 Add a `ForeignPlanning.check` helper over `Mir.Module` that builds the closure-wide symbol map over imports and exports and reports the conflicting-foreign-symbol diagnostic relating both declarations for duplicate exports and export/import coincidences, and rejects every export with the foreign-function-target-unavailable diagnostic when the target kind is WebAssembly; call it from `Realization.discoverAndLower` and `Analysis.codegen` beside the availability gate; and verify the duplicate, coincidence, accepted-distinct, direct-Wasm, and LLVM-wasm32 cases, plus that an evaluator run of a program with an export reports nothing.
- [ ] 3.4 In the same helper, reject an export whose MIR function has `suspension` defined with a classification other than `Synchronous`, naming the suspending call, and verify a test with a suspending body reports it and a synchronous body passes.

## 4. Native backend

- [ ] 4.1 Declare one thunk per export record in `NativeDeclare` (calling convention 0, external linkage, LLVM types from the C signature) and build its body as one `callDirect` to the implementation handle plus a return, asserting the implementation is not suspendable, and verify an IR text test shows `define i32 @silk_test_double_v1(i32)` with calling convention property `0` and one call to the distinct implementation symbol.
- [ ] 4.2 Add `foreignExports` to `Backend.ArtifactBase` sorted by symbol (empty for Wasm) and verify a backend test lists the exports with signatures and that bitcode with exports is byte-identical across fresh processes.

## 5. Native acceptance

- [ ] 5.1 Add a C fixture with `int32_t silk_test_roundtrip(int32_t)` calling `silk_test_double_v1` from Silk, compile it with `compileCObject`, and verify a Silk program that exports `silk_test_double_v1` and calls the fixture returns the value computed through both boundaries on the host target.
- [ ] 5.2 Verify an export covering every admitted scalar type is called correctly from C by a fixture that exercises each and returns a checksum.
- [ ] 5.3 Add the programs to the native acceptance corpus.

## 6. Tooling and documentation

- [ ] 6.1 Add `ExportKeyword` to the TextMate and CodeMirror keyword tables and verify the highlighting fixtures.
- [ ] 6.2 Show the export symbol in LSP hover and document structure and in docgen output, and verify the LSP acceptance fixture and a generated reference include it.
- [ ] 6.3 Add the export marker to the syntax inspector and verify the inspector fixture renders it.
- [ ] 6.4 Extend the "Foreign functions" reference section with FFI entries for exports (thunk seam, roots, collision rules, Wasm rejection), update the glossary and `alpha-status.md`, and verify the docs snippet tests compile the new examples.
- [ ] 6.5 Regenerate the diagnostic index and stdlib tables, verify the staleness checks pass, and run the full gate with `node scripts/turbo.mjs run test`.
