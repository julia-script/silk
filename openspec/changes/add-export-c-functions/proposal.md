## Why

`add-extern-c-functions` lets Silk call C; nothing lets C call Silk except the compiler's private
`silk_main` entry. Every Silk function is emitted under a compiler-versioned symbol with a
lane-flattened internal ABI that must stay private. `export "C"` gives source an explicit way to
publish one C-callable symbol behind a generated thunk, so the internal ABI never becomes a public
contract by accident. Doing it now, before pointers and library artifacts, proves the bidirectional
ABI on the scalar subset with a single-executable round trip and settles the closure-root and
symbol-collision rules that library artifacts will build on.

## What Changes

- Add the declaration `[pub] export "C" fn name(params) -> result [as "symbol"] { body }`.
  `export` becomes a complete-identifier keyword. Only the ABI `"C"` is accepted. The native symbol
  is the `as` string or the Silk name and obeys the same validity and reservation rules as an
  `extern` symbol. `pub` keeps its ordinary Silk meaning and is independent of native export.
- An exported function's signature is admitted by the same V1 C-ABI relation as a foreign function:
  `()` result, exact-width integers, target-width `isize`/`usize`, `f32`/`f64`, all by value. It
  may not declare type parameters, a `where` clause, a failure row, a requirement row, `effect`,
  `static`, or `unsafe`, and its body may not suspend. Its Silk callers call it as an ordinary
  function.
- Every `export "C"` declaration in the loaded module closure is an additional instance-discovery
  root on native targets, so an export that no Silk code calls is still compiled. A native
  executable still requires the ordinary `main` entry; exports do not replace it.
- Native emission keeps the ordinary internal implementation and adds one thunk per export: an
  external function under the C calling convention and the export's symbol that forwards its
  scalar arguments to the implementation and returns its result. The internal symbol is never the
  exported one.
- Two exports of one symbol, or an export and an `extern` of one symbol, in one executable closure
  are rejected. An export symbol that collides with the compiler's runtime symbols is rejected.
- On WebAssembly targets an `export "C"` declaration in the closure is an availability
  diagnostic; the evaluator runs with native discovery and reports nothing for exports because they
  are not observable there.
- Native artifacts record the exported symbols with their C signatures beside the foreign imports.
- `ModuleSummary.Export` is renamed to `PublicDeclaration` so "export" means native export only.

## Non-goals

- **No library artifacts.** Exports live in executables in this change; shared and static library
  output, hidden visibility for internals, and structured link inputs are the next change.
- **No pointers, records, callbacks, or data symbols.**
- **No generated C headers or ABI manifests.** The artifact inventory is the seed for them.
- **No `unsafe export`.** Unsafety is a caller-side Silk contract; C callers cannot acknowledge it.

## Capabilities

### New Capabilities

_None._

### Modified Capabilities

- `bootstrap-foreign-functions`: adds the `export "C"` declaration, its identities, signature
  admission and restrictions, thunk linkage semantics, root semantics, collision rules with
  imports, and native-only availability.
- `bootstrap-lexer`: `export` joins the complete-identifier keyword vocabulary.
- `bootstrap-syntax`: the exported function declaration parses losslessly and recovers locally.
- `silk-source-formatting`: the formatter prints an exported declaration in canonical form.
- `bootstrap-declaration-index`: exported function headers carry the ABI and symbol facts and are
  admitted at the header level.
- `bootstrap-instances`: exported functions in the loaded closure are discovery roots on native
  targets.
- `bootstrap-backend`: native emission produces one C-calling-convention thunk per export and
  records exports on the artifact; direct Wasm rejects them.

## Impact

- **Lexer and parser.** New `ExportKeyword`; the function-declaration path accepts an
  `export <literal>` prefix and an `as <literal>` tail before the body; the `as` tail parser is
  shared with `extern`.
- **Declaration pipeline.** `DeclarationFact.foreignExport?: { abi, symbol }`; completion reuses
  the `CAbi` classifier, restriction checks, and symbol validation from the extern change. The
  suspension restriction is checked where suspension classification is available, after MIR.
- **Discovery.** `Instances.discover` gains export roots collected from every loaded module's
  declaration facts when the target kind is `Native`; `Discovery` records them, `Lower` copies them
  onto `Mir.Module`, and the cached emission header carries the export inventory.
- **Backend.** `NativeDeclare` declares the thunk beside the implementation; a small emitter builds
  the forwarding body; `Backend.ArtifactBase.foreignExports`. `WasmBackend` planning reports the
  availability diagnostic.
- **Rename.** `ModuleSummary.Export` → `PublicDeclaration`, `exports` → `publicDeclarations`, and
  every consumer: compiler `WorkspaceInventory`, `AutoImport`, `index.ts`; LSP `WorkspaceCatalog`
  and `Document`; their tests.
- **Tests.** A C fixture that calls back into an exported Silk function from a Silk-called C
  function, proving the C ABI in both directions inside one executable.
- **Tooling and docs.** Formatter, inspector, LSP, highlighting, docgen; FFI reference entries for
  exports; glossary.
