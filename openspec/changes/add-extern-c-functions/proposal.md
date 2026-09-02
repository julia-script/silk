## Why

Silk can only reach native code through compiler-sealed intrinsics, each of which needs a new
compiler primitive, a generated C runtime fragment, and an evaluator adapter. The compiler already
links `silk_os_*_v1` symbols through an ordinary C ABI, and the LLVM wrapper already models linkage,
visibility, and calling conventions; what is missing is a source-level way to name a native symbol.
`extern "C"` gives ordinary Silk source that ability under the rule the reference already commits to:
foreign calls are unsafe, target-specific, and outside the compatibility guarantee. This change is
the first of a sequence (foreign exports, raw pointers, C-layout records, library artifacts); it
proves the parser → facts → MIR → LLVM → linker path with the scalar subset only, so the pointer
representation can be decided without blocking it.

## What Changes

- Add the declaration `[pub] unsafe extern "C" fn name(params) -> result [as "symbol"]`. It has
  no body. `extern` becomes a complete-identifier keyword. The `unsafe` qualifier is mandatory: a
  foreign declaration without it is rejected. `pub` keeps its ordinary meaning and says nothing
  about native linkage.
- A foreign function has three separate identities: its Silk name, its native symbol (`as "..."`
  or the Silk name), and its ABI. Only the ABI string `"C"` is accepted. The symbol is the logical
  native name; the compiler never asks for target decoration such as a Darwin `_` prefix.
- A foreign signature admits only the V1 C-compatible subset: `()` as result, `i8`–`u64`,
  `isize`/`usize` at the selected target's width, and `f32`/`f64`, all by value. `bool`, `char`,
  `string`, references, slices, arrays, structs, unions, enums, callables, and generic parameters
  are rejected with a diagnostic at the offending type. Foreign-ABI admission is its own relation,
  not ordinary type compatibility.
- A foreign function may not declare type parameters, a failure row, a requirement row, a `where`
  clause, `effect`, `static`, or a body. Its value may be called only; forming a first-class
  callable from it is rejected in this change.
- Calling a foreign function lowers to a new explicit MIR operation carrying the symbol, the ABI,
  and the classified C signature. The LLVM backend declares each reachable symbol once with the C
  calling convention and external linkage and emits a direct call. There is no runtime lookup,
  cache, or indirect call.
- Foreign functions are available on native LLVM targets only. A reachable foreign call under the
  evaluator or the direct WebAssembly backend is a compile-time availability diagnostic in the
  TARGET-003 shape; an unreachable declaration costs nothing.
- Two foreign declarations of one symbol with different C signatures in one executable closure are
  rejected, as is a symbol that collides with the compiler's own runtime symbols.
- The driver request accepts additional native object files and library names; the project
  manifest's `[build]` table gains an optional `native-libraries` list that reaches the link
  command as `-l` arguments. Arbitrary linker flags stay out of source and manifest.
- Emitted native artifacts record the reachable foreign symbols and their C signatures.

## Non-goals

- **No pointers.** `*const T`/`*mut T` are the next change; pointer-taking libc functions cannot be
  declared yet.
- **No exports.** `export "C"` and C-callable thunks are a separate change.
- **No evaluator or Wasm implementation.** A symbol-keyed host-function table for the evaluator
  and Wasm import binding are later work; this change only leaves the seam (the MIR operation is
  keyed by symbol and signature).
- **No library artifacts, headers, ABI manifests, variadics, weak or data symbols, or dynamic
  loading.**
- **No migration of OS intrinsics.** `Intrinsic.os*` and the generated C runtime are untouched.

## Capabilities

### New Capabilities

- `bootstrap-foreign-functions`: the `extern "C"` declaration, its three identities, mandatory
  unsafety, the V1 C-ABI type admission relation, declaration restrictions, symbol validity and
  collision rules, direct-call linkage semantics, and native-only availability.

### Modified Capabilities

- `bootstrap-lexer`: `extern` joins the complete-identifier keyword vocabulary.
- `bootstrap-syntax`: the foreign function declaration parses losslessly and recovers locally.
- `silk-source-formatting`: the formatter prints a foreign declaration in canonical form.
- `bootstrap-declaration-index`: foreign function headers join the canonical index with symbol and
  ABI facts and no body.
- `bootstrap-module-semantic-surface`: public foreign functions are encoded with symbol, ABI, and
  signature so a symbol change invalidates dependents.
- `bootstrap-mir`: foreign calls are an explicit, verified, deterministically encoded operation.
- `bootstrap-backend`: native emission declares reachable foreign symbols with the C calling
  convention and records them on the artifact; direct Wasm receives no foreign ABI.
- `bootstrap-native-toolchain`: the linker accepts request-supplied native objects and approved
  library names beyond the program object and shim, and the artifact cache key covers them.
- `silk-project-manifest`: the optional build defaults gain a `native-libraries` list.

## Impact

- **Lexer and parser.** New `ExternKeyword`, new `ForeignFunctionDeclaration` node kind, entries in
  the declaration-start, `pub`-following, and top-level-following tables in `Parser/Declaration.ts`
  and `Parser/Grammar.ts`.
- **Declaration pipeline.** `DeclarationCollection` collects the header with `foreign: { abi,
  symbol }`; `DeclarationCompletion` admits the signature by spelling and reports admission,
  restriction, and symbol diagnostics; body analysis and residualization skip foreign headers. Name
  and call resolution, the callable contract, and unsafe acknowledgement are reused unchanged.
- **New module.** `CAbi.ts`: the target-aware classifier from `Type.Type` to a C ABI type and the
  signature identity used by MIR, verification, and the backend.
- **Reachability and availability.** `ExecutableOrigin` collects reachable foreign calls beside
  intrinsic calls and classifies their C signatures for the selected target; `Instances`,
  `Mir.Module`, and `IntrinsicAvailability` carry the foreign inventory to every availability site
  with a native-only check (target kind `Native`, not merely the LLVM backend) and a new stable
  diagnostic code.
- **MIR and backends.** `Mir.ForeignCall`, verification, encoding, `LowerExpression` lowering, a
  foreign inventory on `Mir.Module`; `NativeProgram` declares symbols; a `ForeignCall` arm in the
  `NativeOperation` dispatch emits the call the way `OsCall` is emitted today; `BootstrapEvaluation`
  and `WasmBackend` treat the operation as unreachable after availability.
- **Driver and toolchain.** `CompileRequest.nativeObjects` / `nativeLibraries` passed through to
  the already list-shaped `ClangLinker.link` and `ToolchainPlan.linkCommand` (today `Driver.ts`
  hard-codes `[object, shim]` and `['m']`), the artifact cache key covers them, `Project.ts` parses
  `native-libraries`, `BuildBatch` forwards it. `compileShim`'s C compile step is extracted
  so tests can compile a C fixture object through the pinned Clang.
- **Artifact.** `Backend.ArtifactBase.foreignImports`.
- **Tooling.** Formatter, syntax inspector, LSP structure and hover, TextMate and CodeMirror keyword
  tables, docgen, following the `type` declaration precedent.
- **Documentation.** New "Foreign functions" section in `unsafe-intrinsics-and-targets.md`, glossary
  entry, `alpha-status.md` boundary narrowed, RUNTIME-003 pointed at the real syntax.
