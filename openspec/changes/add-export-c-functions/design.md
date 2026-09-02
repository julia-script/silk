## Context

See `add-extern-c-functions`: `DeclarationFact.foreign`, the `CAbi` admission and classification,
symbol validity and reservation (including `main`), and the foreign inventory on `Mir.Module`
exist after that change. Every Silk function is emitted under `symbolFor(fn)`
(`silk_<module>_<name>__<instance>`, or `silk_main` for the entry) with lane-flattened parameters
and, when suspendable, a `$suspend_step` symbol and three hidden parameters. Discovery starts from
the single entry in `Instances.discover(rootModule, results, index, target, resolution)`, which
has every loaded module's elaboration results and the declaration index but no execution-surface
input; `Target.kind` is `'Native' | 'WebAssembly'`. `Realization.discoverAndLower` runs the same
discovery for the evaluator and the backends; the evaluator selects `'Evaluator'` only in
`BootstrapEvaluation`. Availability gates run in two places: `Realization.discoverAndLower` (when a
backend is requested) and `Analysis.codegen`, which emits from its own MIR. `Backend.emit` receives
only `Mir.Module`, and `Driver.decodeCachedEmission` rebuilds an artifact from a cached header plus
the program. `SuspensionMir.finalize` leaves `suspension` undefined on a function with no regions.
The LLVM builder can define a body for a declared function (`Function.buildBody`, `Block.make`,
`Value.argument`, `callDirect`, `returnValue`/`returnVoid`). The textual renderer prints calling
convention 0 without a marker. `ModuleSummary.Export` names public Silk declarations and is consumed
by `WorkspaceInventory`, `AutoImport`, the compiler barrel, and the LSP `WorkspaceCatalog` and
`Document`.

## Goals / Non-Goals

**Goals:**

- One seam between the public C symbol and the private implementation, kept even when the thunk is
  a trivial forward, so later representation changes never leak into exported ABI.
- Reuse the extern change's admission, restrictions, symbol rules, and diagnostics; add only the
  root and thunk mechanics.

**Non-Goals:**

- Library artifacts, symbol visibility of internals, headers, manifests.
- Exports on Wasm targets (rejected) or observable in the evaluator (silently inert).

## Decisions

### An export is a `FunctionDeclaration` fact with a `foreignExport` field

`DeclarationFact.foreignExport?: { readonly abi: 'C'; readonly symbol: string }`. Collection sets it
from the parsed marker; completion runs the same admission and restriction checks as for `foreign`
(sharing the helper) plus rejections of `unsafe` and `static`. Everything after completion treats
the function as ordinary: call resolution, ownership, MIR, and the internal symbol are untouched.
This mirrors the extern decision and keeps "one function fact" as the model.

### Exports are extra discovery roots keyed on `Target.kind`

`Instances.discover` appends every `foreignExport` fact from every loaded module as a root after
the entry, in canonical module then declaration order, when `target.kind === 'Native'`, and records
`exports: ReadonlyArray<{ symbol, signature, key: InstanceKey, declaration }>` on `Discovery`.
Roots are monomorphic by restriction, so specialization needs no arguments. The evaluator runs the
same native discovery and therefore inherits the roots harmlessly: nothing reads `exports` there,
and the extra instances are ordinary functions. For a WebAssembly target the list is empty and the
availability helper reports the diagnostic for each export in the closure.

The alternative, adding an execution-surface parameter to `discover` so the evaluator can drop the
roots, would touch every discovery caller for no observable benefit. Making exports the entry for a
library artifact kind is the library change's job; executables keep `main` so the driver, shim, and
termination contract are unchanged here.

### Exports travel on `Mir.Module` and the cached emission header

`Lower.lowerProgram` copies `discovery.exports` onto `Mir.Module` beside `intrinsics` and the
foreign inventory, which is the only input `Backend.emit` receives. The LLVM backend reads it to
declare thunks and to populate `foreignExports` on the artifact. The cached emission header gains
`foreignExports` so `decodeCachedEmission` reproduces the artifact on a cache hit.

### Planning checks live in one helper called from both gate sites

A `ForeignPlanning.check(program, target, surface)` helper performs, over `Mir.Module`: the
closure-wide symbol map over imports and exports (duplicate export symbols and export/import
coincidences report the conflicting-foreign-symbol diagnostic relating both declarations), the
Wasm-target export rejection, and the suspension check below. `Realization.discoverAndLower` and
`Analysis.codegen` both call it before backend construction, next to the existing availability
gate. Reserved-symbol and spelling rules stay the extern change's header checks.

### Suspension is checked after MIR, against the optional classification

For each export record the helper requires `fn.suspension === undefined ||
fn.suspension.classification === 'Synchronous'` on the export's MIR function, exactly the
predicate `NativeDeclare` uses to decide suspendability; otherwise it reports the export-suspension
diagnostic at the declaration naming the suspending call and constructs no artifact. Checking
earlier would require re-deriving classification in completion.

### The thunk is declared beside the implementation and forwards scalar lanes

`NativeDeclare.functions` continues to declare every MIR function as today. A new step declares one
thunk per export record with `FunctionActor.declare(symbol, cType)` (calling convention 0, external
linkage) and builds a body of one `callDirect` to the implementation handle with the thunk's
arguments and one `returnValue` (or `returnVoid`). The scalar subset guarantees one lane per
parameter and at most one result lane, so no lane packing is needed. The thunk emitter asserts the
implementation is not suspendable, which the suspension check already guarantees.

### `ModuleSummary.Export` becomes `PublicDeclaration`

Pure rename of the interface, its `_tag`, and the `exports` field, done first so no code has two
meanings of "export" while this change lands. Consumers: `ModuleSummary.ts`,
`WorkspaceInventory.ts`, `AutoImport.ts`, the compiler `index.ts`, LSP `WorkspaceCatalog.ts` and
`Document.ts`, and their tests.

### Parsing shares the marker slot and the `as` tail with `extern`

The function-declaration path accepts `export <literal>` in the same slot as `extern`, after
`unsafe` and before `effect` (`[pub] [static] [unsafe] export "C" [effect] fn`), and reuses the
`as <literal>` tail parser. The body is required; a missing body is a parser diagnostic because,
unlike `extern`, there is no valid bodiless form.

## Risks / Trade-offs

- [Thunk duplicates the implementation's frame for a trivial forward] → LLVM inlines it at `-O2`;
  at `-O0` the extra call is the accepted cost of the seam.
- [Export roots increase closure size for programs that do not use them] → Only modules that
  declare `export "C"` add roots; ordinary programs are unaffected.
- [The evaluator specializes export roots it never runs] → Bounded by the number of exports; they
  are ordinary synchronous scalar functions.
- [Checking suspension after MIR delays the diagnostic] → It is still a compile-time planning
  diagnostic before any artifact; the message names the suspending call.
- [Rename touches several packages] → Mechanical; the typechecker enumerates every site.
