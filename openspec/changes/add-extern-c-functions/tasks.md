## 1. Lexer, parser, formatter

- [x] 1.1 Add `extern` to the complete-identifier keyword table as `ExternKeyword`, confirm no `.silk` source in the repository uses `extern` as an identifier, and verify a lexer test distinguishes `extern` from `external`.
- [x] 1.2 Add the `ForeignFunctionDeclaration` node kind and parse `[pub] [static] [unsafe] extern <text> [effect] fn name(params) [-> type] [as <text>]` from the declaration-start, `pub`-following, and top-level-following tables, retaining `static`, `effect`, a type-parameter list, rows, `where`, or a block body for semantic rejection, and verify parser fixtures cover the renamed public form, the retained body, a missing ABI literal with intact recovery of the next declaration, and modifier order.
- [x] 1.3 Render the declaration in `SyntaxFormatter` as one line with the existing parameter-list layout, an omitted result kept omitted, and no body, and verify an idempotent formatting test with irregular spacing and an attached comment.

## 2. C ABI admission and declaration facts

- [x] 2.1 Add `CAbi.ts` with target-independent `admit(type)` over the V1 spelling set (`()` result, exact-width integers, `isize`, `usize`, `f32`, `f64`) returning `NotAdmitted` for every other variant, and target-aware `classify(type, target)` / `signature(...)` with a canonical key, and verify a table test covering every admitted scalar, `bool`, `char`, `string`, a reference, a slice, a struct, and a type parameter, plus `usize` classifying to 32 bits on wasm32 and 64 bits on native targets.
- [x] 2.2 Extend `DeclarationFacts.DeclarationFact` with `foreign?: { abi: 'C'; symbol }`, collect it in `DeclarationCollection` with `unsafe` from the qualifier, and verify a collision with a same-named function reports the existing collision diagnostic.
- [x] 2.3 Skip foreign facts in `Elaboration.elaborateModule` body analysis so a missing `Block` is not an error (`Residualization` is never reached for a bodiless runtime-phase fact), and verify a module with only a foreign declaration and a `main` elaborates and realizes without a thrown `RangeError`.
- [x] 2.4 In `DeclarationCollection` (where the source literals are available), reject a non-`"C"` ABI, a missing `unsafe`, a retained `static`, `effect`, type-parameter list, failure row, requirement row, `where` clause, or body, each with a new diagnostic at the offending span, and verify one test per rejection publishes no callable.
- [x] 2.5 Run `admit` over every foreign parameter and result in completion, reporting the foreign-type-not-admitted diagnostic at each rejected type, and verify `bad(flag: bool) -> char` reports two diagnostics and an all-scalar declaration reports none.
- [x] 2.6 Validate the symbol spelling (`[A-Za-z_][A-Za-z0-9_]*`) and reject reserved symbols from the exported list in `ForeignSymbol.ts` (`main`, `silk_main`, OS and coroutine runtime symbols, standard-stream and host-argument symbols, `^silk_suspend_`, `^silk_.*__`), and verify tests for an invalid spelling, `as "main"`, and `as "silk_main"`.
- [x] 2.7 Reject forming a first-class callable, section, or stored callable from a foreign function in expression analysis with a new diagnostic, and verify `let f = abs` and passing `abs` as a callable argument both report it while a direct call inside `unsafe` does not.
- [x] 2.8 Verify the unsafe path end to end: a call without acknowledgement reports the existing unsafe-acknowledgement diagnostic, and `unsafe { abs(x) }` and `unsafe abs(x)` both resolve.

## 3. Reachability, availability, and module surface

- [x] 3.1 Collect `ReachableForeignCall { symbol, signature, declaration, declarationSpan, callSpan }` in `ExecutableOrigin` beside intrinsic calls, classifying the signature for the selected target, carry the inventory through `Instances.Discovery`, and verify an uncalled foreign declaration yields an empty inventory while a called one yields one entry with the target-classified signature.
- [x] 3.2 Copy the inventory onto `Mir.Module` in `Lower.lowerProgram` beside `intrinsics` and read it at every availability site (`Realization.discoverAndLower`, `Analysis.codegen`, `Backend.emit`, `BootstrapEvaluation`), and verify each site rejects a reachable foreign call under its non-native surface.
- [x] 3.3 Reject a reachable foreign call when the execution target is `Evaluator` or `Wasm` or when `Target.kind !== 'Native'` with a new stable availability diagnostic naming the symbol and surface or target, and reject conflicting classified signatures under one symbol across the closure with a diagnostic relating both declarations, and verify tests for evaluator rejection, direct-Wasm rejection, LLVM `wasm32-unknown-unknown` rejection, unreachable-declaration acceptance under Wasm, a `static if` arm that hides the call, agreeing redeclarations across two modules, and a conflicting pair.
- [x] 3.4 Encode public foreign functions in `ModuleSurface` with unsafe contract, ABI, symbol, and Silk parameter and result types, and verify an encode/decode round-trip, that an `as` rename invalidates a direct importer, and that a trivia-only edit leaves the surface equal.
- [x] 3.5 Verify cross-module use: a selected import and a namespace-qualified call of a public foreign function both resolve, and a private one reports the visibility diagnostic.

## 4. MIR

- [x] 4.1 Add the `ForeignCall` operation to `Mir.ts` with symbol, ABI, signature, arguments, destination, type, and provenance; lower a call whose target fact is foreign to it in `LowerExpression`; and verify a MIR test shows one operation with the expected signature and locals.
- [x] 4.2 Verify arity, argument-class, and destination-class agreement in `MirVerification` as structural violations and add the operation and the module inventory to `MirEncoding`, and verify an arity-mismatch fixture reports one violation and two fresh-process encodings of the same program are byte-identical.
- [x] 4.3 Make `BootstrapEvaluation` and `WasmBackend` throw `RangeError` on a `ForeignCall` (availability rejects it first), and verify the evaluator test suite still passes with the operation present in the union.

## 5. Native backend

- [x] 5.1 In `NativeProgram`, declare each reachable foreign symbol once with `FunctionActor.declare` (calling convention 0) using the LLVM types the C signature selects, map an incompatible-redeclaration `LlvmError` to a `BackendError` naming the symbol, and verify an IR text test shows exactly one `declare i32 @silk_test_add(i32, i32)` for two call sites, calling convention property `0`, and `i64` for a `usize` parameter on a 64-bit target.
- [x] 5.2 Add a `ForeignCall` arm to the `NativeOperation` dispatch beside `OsCall` that reads the argument lanes, emits `callDirect`, reloads the address-taken roots, and stores the single result lane or nothing for `void`, and verify the LLVM module verifies and the IR contains a direct `call` to the declaration.
- [x] 5.3 Add `foreignImports` to `Backend.ArtifactBase` sorted by symbol, populate it from the LLVM backend (empty for Wasm), and verify a backend test lists `abs` and `silk_test_add` with signatures and nothing for a program without foreign calls.

## 6. Toolchain, driver, manifest

- [x] 6.1 Extract `compileCObject(toolchain, scope, target, name, sourceText)` from `compileShim` (same pinned `-c -x c` command and shim cache) and rebuild `compileShim` on it, and verify `NativeToolchain.test.ts` still passes and the planned command is unchanged.
- [x] 6.2 Add `nativeObjects` and `nativeLibraries` to `Driver.CompileRequest`, wrap each object path as a `PathArtifact` for the selected target, pass `[object, shim, ...nativeObjects]` and `['m', ...nativeLibraries]` to the existing `ClangLinker.link`, and verify a plan test shows the object after the shim and `-lc` after the objects and before `-o`.
- [x] 6.3 Extend `artifactCacheKey` with each native object's bytes and the ordered library list, and verify a driver test relinks when a supplied object's bytes change and hits the cache when nothing changes.
- [x] 6.4 Parse `build.native-libraries` in `Project.ts` with the validation rules (non-empty, no separators, whitespace, NUL, or leading `-`), forward it from `BuildBatch` for native targets and ignore it for Wasm, and verify manifest tests for the two-library case and the disguised-flag rejection.

## 7. Native acceptance

- [x] 7.1 Add a C fixture under the compiler test tree defining scalar functions for every admitted type (`int8_t`…`uint64_t`, `size_t`, `ssize_t`-shaped, `float`, `double`, a `void` function that records a call through a second query function), compile it with `compileCObject`, link it through `nativeObjects`, and verify a Silk program calling each function returns the C-computed exit status on the host target.
- [x] 7.2 Verify the libc smoke case: `unsafe extern "C" fn abs(value: i32) -> i32` without any fixture links and returns the expected value; and verify the undefined-symbol case returns a typed link failure retaining the linker output and no executable.
- [x] 7.3 Add the fixture programs to the native acceptance corpus so they run under the existing parallel and native-acceptance gates.

## 8. Tooling

- [x] 8.1 Add `ExternKeyword` to the TextMate and CodeMirror keyword tables (the exhaustive record forces it) and verify the highlighting fixtures.
- [x] 8.2 Add the declaration to LSP document structure and hover, showing the unsafe signature and the native symbol, and verify the LSP acceptance fixture lists a `pub unsafe extern "C"` symbol and hovers it.
- [x] 8.3 Emit foreign functions in docgen module references and the documentation JSON with their native symbol, and verify a generated reference for a module with one includes it.
- [x] 8.4 Add the node kind to the syntax inspector and verify the inspector fixture renders it.

## 9. Documentation and acceptance

- [x] 9.1 Add a "Foreign functions" section to `unsafe-intrinsics-and-targets.md` with FFI-numbered entries for the declaration and identities, mandatory unsafety, the admitted type subset, restrictions and symbol rules, direct linkage, and native-only availability; add a glossary entry; narrow the `alpha-status.md` FFI boundary; point RUNTIME-003 at `extern "C"`; and verify the docs snippet tests compile every new example.
- [x] 9.2 Regenerate the diagnostic index and stdlib source tables and verify the staleness checks pass.
- [x] 9.3 Run the full gate with `node scripts/turbo.mjs run test` and verify it passes.
