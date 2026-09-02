## Context

The compiler already crosses a C ABI in one place: `NativeProgram` walks every function's `OsCall`
and `OsOpen` operations, derives a `silk_os_*_v1` symbol per intrinsic, declares it once with
`FunctionActor.declare` (calling convention 0, external linkage), and the `OsCall` arm of the
`NativeOperation` dispatch (in `NativeMemoryOperation`) emits `callDirect` and then reloads every
address-taken root, exactly as `NativeCall.callSynchronous` does after a Silk call. The shim is
compiled from generated C, and `Driver.compile` links `[object, shim]` with a hard-coded `['m']`
through `ClangLinker.link` and `ToolchainPlan.linkCommand`, which already take arbitrary object and
library lists. The LLVM package models linkage, visibility, and calling conventions on
`Global.Options` / `Function.Options` and rejects an incompatible redeclaration of one symbol; the
textual renderer prints convention 0 without a marker.

Function headers are `DeclarationFacts.DeclarationFact` with `unsafe`, `phase`, `functionKind`,
parameter and return facts, and a `bodyTemplate` retained only for bodies containing static work.
`CallableContract.unsafe` already drives the lexical acknowledgement diagnostics (UNSAFE-002..008).
The declaration index is target-independent: `NameResolution.analyze` and
`DeclarationCompletion.complete` see no target, and the semantic surface must exclude target
layout. The target enters at `Analysis.realize`. `Elaboration.elaborateModule` runs
`analyzeFunctionBody` over every declaration and throws when a declaration has no `Block`;
`Residualization` does the same. `ExecutableOrigin` collects `ReachableIntrinsicCall`s from runtime
HIR; `IntrinsicAvailability.select` is called from four sites (`Realization`, `Analysis.codegen`,
`Backend.emit`, `BootstrapEvaluation`), three of which read `Mir.Module.intrinsics`. `LlvmBackend`
also emits `wasm32-unknown-unknown`, and `IntrinsicAvailability.backendTarget` maps it to `LLVM`.

Silk keeps scalar types distinct even at equal width, and `char` is a Unicode scalar. The Address
calling lane exists (`CallingShape.AddressScalar`), but no source-level pointer type does.

## Goals / Non-Goals

**Goals:**

- Reuse the function-declaration pipeline end to end; a foreign function is a function header
  with a native body, not a new declaration entity.
- Keep the Silk internal calling ABI private: nothing in this change makes lane flattening or
  `$suspend_step` visible to C.
- Leave a symbol-keyed seam for the evaluator host table and Wasm imports without building them.

**Non-Goals:**

- Pointers, exports, records, library artifacts, headers, manifests, dynamic loading.
- Any change to `OsCall`/`OsOpen` or the generated C runtime.
- A general `NativeLinkInput` model; the request grows two plain lists.

## Decisions

### A foreign function is a `FunctionDeclaration` fact with a `foreign` field

`DeclarationFact` gains `foreign?: { readonly abi: 'C'; readonly symbol: string }`. Collection sets
`unsafe` from the qualifier and reports the missing-qualifier diagnostic when absent. Name
resolution, call resolution, `CallableContract`, unsafe acknowledgement, module surface encoding,
LSP structure, hover, and docgen see an ordinary unsafe function header. Body analysis in
`Elaboration.elaborateModule` and `Residualization` skips a fact with `foreign` set instead of
demanding a `Block`; `Instances` already tolerates a callee without HIR.

The alternative was the report's separate `HirForeignFunction` entity with its own id space. It
would need its own name-resolution arm, call-resolution arm, surface encoding, and tooling paths for
a value that behaves as a function everywhere except at lowering. The one place foreignness matters
before lowering, first-class use, is a single check in expression analysis: a `FunctionItem` or
`CallableSection` whose target fact is foreign reports the not-first-class diagnostic.

### Admission is target-independent at completion; classification is target-aware at realization

`CAbi.ts` has two entry points:

```
admit(type): Admitted | NotAdmitted        // spelling set: (), i8..u64, isize, usize, f32, f64
classify(type, target): CAbiType           // Void | Integer{bits, signed} | Float{bits}
signature(fact, target): CAbiSignature     // parameters + result, canonical key
```

`DeclarationCompletion` runs `admit` over every parameter and result of a foreign header and
reports one diagnostic per rejected type at that type's span; a rejected header publishes no
callable, so call sites get the ordinary unknown-callee behavior. `admit` is a closed switch over
`Type.Builtin` scalars and returns `NotAdmitted` for every other `Type` variant; it does not
consult `TypeCompatibility`, because `u32` and `i32` are distinct C classes and `char` is rejected
even though it is 32 bits wide. `classify` resolves `isize`/`usize` through the target's pointer
width and runs where the target is known: in `ExecutableOrigin` when the foreign inventory is
built, and in `NativeProgram`. The surface encodes symbol, ABI, and Silk contract types only.

### `ForeignCall` is a new MIR operation, not a `Call` variant

```ts
{ _tag: 'ForeignCall', destination, symbol, abi: 'C', signature: CAbiSignature,
  arguments, type, provenance }
```

`Call` targets a `CanonicalId` and is specialized, inlined, and instance-keyed; a foreign call has
no instance and must never be. The dedicated operation makes verification (`arguments.length ===
signature.parameters.length`, each argument local's type classifies to the parameter class),
encoding, and backend dispatch obvious. It mirrors `OsCall` exactly, which is the intended
migration target for the OS runtime later. `BootstrapEvaluation` and `WasmBackend` throw a
`RangeError` on it, as `WasmBackend` does for `OsCall` today, because availability rejects it first.

### The foreign inventory lives on `Mir.Module` and reaches every availability site

`ExecutableOrigin` collects `ReachableForeignCall { symbol, signature, declaration: CanonicalId,
declarationSpan, callSpan }` beside intrinsic calls (a `Call` HIR expression whose target fact is
foreign). `Instances.Discovery` carries it, `Lower.lowerProgram` copies it onto `Mir.Module`
beside `intrinsics`, and `IntrinsicAvailability.select` (or a sibling `ForeignAvailability`) reads
it at all four call sites. A reachable entry is rejected when the execution target is `Evaluator`
or `Wasm`, or when `Target.kind !== 'Native'` (which catches LLVM emission of
`wasm32-unknown-unknown`), with a new stable code. Conflicting signatures under one symbol across
the closure report a second new code relating both declarations through the carried declaration
ids. Both are reported before backend construction, so the Wasm backend and the evaluator never see
the operation. Unreachable declarations cost nothing, matching TARGET-002.

The alternative, an `Evaluator` host-function table keyed by symbol, was deferred by decision. The
symbol-and-signature key on the operation and the inventory is the seam it will attach to.

### Symbol validity and reservation are header diagnostics; the backend is the safety net

A symbol must match `[A-Za-z_][A-Za-z0-9_]*`. It is rejected when it equals `main`, `silk_main`,
any `OsRuntime.symbols` or `CoroutineRuntime.symbols` entry, `silk_standard_stream_write_v1`,
`silk_host_argc_v1`, `silk_host_argv_v1`, or matches `^silk_.*__` (the generated
`silk_<module>_<name>__<instance>` shape) or `^silk_suspend_`. The reserved set is one exported
list next to `osRuntimeSymbol`. `malloc`, `free`, and `memcmp` are not reserved: the backend
declares them itself with fixed signatures, so a user declaration must agree, and when
`FunctionActor.declare` reports an incompatible redeclaration `NativeProgram` maps that
`LlvmError` to a `BackendError` naming the symbol rather than letting it surface as an
invalid-module failure.

### Native emission extends the `osRuntimes` pattern

`NativeProgram` folds every `ForeignCall` into a `Map<symbol, { handle, signature }>`, declaring
each symbol once with the LLVM function type derived from the C signature (`iN`, target-width
integer, `float`/`double`, `void`). The `NativeOperation` dispatch gains a `ForeignCall` arm beside
`OsCall`: arguments are single scalar lanes already, so it reads each local's lane, emits
`callDirect`, reloads the address-taken roots as `OsCall` and `callSynchronous` do, and stores the
single result lane (or nothing for `void`). No thunk is needed for the scalar subset; the internal
ABI stays private because the declaration is never reachable through a Silk function symbol. The
artifact gains `foreignImports: ReadonlyArray<{ symbol, signature }>` sorted by symbol.

### Link inputs are two lists on the request; the manifest supplies one of them

`CompileRequest` gains `nativeObjects?: ReadonlyArray<string>` and `nativeLibraries?:
ReadonlyArray<string>`. `Driver.compile` wraps each object path as a `PathArtifact` for the selected
target (the linker requires matching targets), links `[object, shim, ...nativeObjects]` with
`['m', ...nativeLibraries]`, and `artifactCacheKey` takes each object's bytes and the library list.
`Project.ts` parses `build.native-libraries` with the validation the manifest spec states;
`BuildBatch` forwards it. The structured `NativeLinkInput` union from the report is not needed until
archives, search paths, and frameworks arrive with library artifacts.

`compileShim`'s "write C source, run pinned Clang `-c -x c`" step is extracted as
`compileCObject(toolchain, scope, target, name, sourceText)` so the test suite compiles its C
fixture through the same pinned command and cache.

### Parsing follows the function declaration

`ForeignFunctionDeclaration` is parsed by the function-declaration path; the modifier order is
`[pub] [static] [unsafe] extern "C" [effect] fn`, so `extern <literal>` sits after `unsafe` and
before `effect`, with an optional `as <literal>` after the return type. The parser retains, rather
than rejects, `static`, `effect`, a following type-parameter list, rows, `where`, or block body so
the diagnostics are semantic and navigable, following the `type Pair<T>` precedent. `extern` joins
the complete-identifier keyword table and every declaration-start table; the exhaustive `keywords`
record in `editor-support` forces the highlighting update.

### Diagnostics

New semantic codes: unsupported foreign ABI, foreign function requires unsafe, foreign type not
admitted, foreign declaration restriction, foreign function not first-class, invalid foreign symbol,
reserved foreign symbol, conflicting foreign signature, foreign function target unavailable. Unsafe
acknowledgement, collision, visibility, and unknown-callee reuse their existing codes.

## Risks / Trade-offs

- [`abs`-style libc symbols behave differently per platform] → The acceptance corpus uses a C
  fixture compiled by the pinned Clang for every ABI shape and uses libc only for one smoke case
  (`abs`), which is identical on every supported target.
- [Reserving `extern` breaks an identifier] → Verified no `.silk` source in the repository uses
  `extern`; Silk is unreleased.
- [Two-module redeclaration agreement depends on target-aware classification] → Signatures are
  compared by classified C key at realization, so `usize` and `u64` agree on 64-bit targets and
  disagree on wasm32, where the call is unavailable anyway.
- [Foreign inventory changes the artifact cache key surface] → The inventory is derived from the
  MIR that is already hashed; only link inputs are new key material.
- [Future pointer change reopens the classifier] → It is a closed switch by design; the pointer
  change adds one arm and one scenario.
- [Tooling surfaces lag] → Listed as tasks with the `type` declaration precedent for each.
