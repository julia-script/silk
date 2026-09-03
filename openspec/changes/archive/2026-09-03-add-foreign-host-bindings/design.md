## Context

See `proposal.md` for motivation. MIR already carries one canonical reachable-foreign inventory and
each `ForeignCall` operation carries the symbol plus classified C signature. Availability currently
rejects evaluator and direct-Wasm consumers before either can use that information. The evaluator is
synchronous and deterministic; its other host boundaries are explicit per-run providers returning
tagged results. The Wasm builder already supports typed function imports.

## Goals / Non-Goals

**Goals:**

- Make one immutable symbol-keyed host table the evaluator's only foreign-call authority.
- Reject all missing or signature-mismatched bindings before the first evaluation operation.
- Preserve the typed-result convention at the host boundary and validate successful results before
  they enter a MIR local.
- Emit direct Wasm calls through one canonical imported handle per reachable symbol.
- Keep native linking and LLVM-wasm availability policy independent from evaluator/Wasm hosting.

**Non-Goals:**

- Loading native libraries or resolving process symbols inside the evaluator.
- Emulating libc or selecting default host functions in compiler code.
- Supporting callbacks, foreign data symbols, variadic functions, or non-C ABIs.
- Adding a foreign-call binding model to LLVM's wasm32 output.

## Decisions

### A public `ForeignHost` actor owns the evaluator boundary

`ForeignHost` defines the canonical public signature spelling, binding, tagged invocation result,
immutable table constructor, lookup, signature comparison, and the versioned Wasm module name. A
binding's handler consumes evaluator `Value` objects and returns either `Returned` with a value (or
no value for `void`) or `Failed` with a message. This preserves exact integer bits and evaluator
pointer identities without lossy JavaScript-number conversion.

The table is supplied on `BootstrapEvaluation.Options` and copied by its constructor, so evaluation
never reads an ambient registry or mutable compiler singleton. A fixed libc table was rejected
because it would silently couple language semantics to the compiler host and make tests dependent on
the process platform.

### Admission compares canonical C-class signatures before execution

The existing C-ABI classifier remains the authority. Evaluator admission converts each reachable
signature to the public canonical class spelling and compares it with the binding for the same
symbol. It returns the first canonical missing or mismatched symbol as a dedicated
`ForeignHostUnavailable` blocked reason. Conflicting source declarations remain planning
diagnostics. Performing this pass before entry execution avoids partial effects followed by a late
missing-binding failure.

Successful host results are checked against the operation's C result class before being written;
`void` constructs the evaluator's canonical unit value. A tagged host failure or invalid result
becomes `ForeignHostCallFailed` with symbol and call provenance. Unexpected JavaScript throws remain
defects, matching the repository's boundary convention.

### Direct Wasm imports share the symbol/signature inventory

Wasm emission sorts and deduplicates `program.foreignCalls`, declares each import from
`silk:runtime/foreign@v1` under the symbol field, and stores the resulting handle in the emission
context. `ForeignCall` lowers its argument lanes, calls that handle, and stores the optional result
lane. C integer widths through 32 bits and pointers map to `i32`; 64-bit integers map to `i64`;
floats map to their matching Wasm type; `void` maps to no result.

The artifact's existing `foreignImports` inventory becomes populated for direct Wasm and its
`hostImports` includes the actual module/field pair. Reusing those existing inventories was chosen
over another Wasm-only metadata type so drivers and inspectors have one backend-neutral contract.

### Availability becomes consumer-specific

`ForeignAvailability` continues to diagnose conflicting reachable declarations and rejects only
execution surfaces that still have no binding model: LLVM emission for a non-native target.
Evaluator host admission lives beside the evaluator options because availability depends on a
per-run table; direct Wasm availability is guaranteed by import emission. The old evaluator-only
`ForeignTargetUnavailable` blocked variant is removed rather than retained as a compatibility path.

## Risks / Trade-offs

- [Handlers receive evaluator values rather than JavaScript primitives] -> Keep representation exact
  now; a separate ergonomic adapter can be designed without changing the binding contract.
- [Pointer-valued handlers can observe evaluator-owned addresses] -> Preserve opaque pointer value
  objects and validate result shape; memory-access APIs remain out of scope.
- [An imported symbol could collide with a private debug name] -> Wasm imports use the symbol as the
  field identity and a compiler-prefixed local debug name, with builder name checks covering locals.
- [Adding imports changes previously import-free direct Wasm modules] -> Only reachable foreign calls
  add imports; programs without them remain byte-for-byte on the existing path.

## Migration Plan

Implement the host actor and evaluator admission/execution first, then Wasm import declaration and
call lowering, then update presentation, docs, and focused parity tests. Archive the OpenSpec delta
after all checks and exact committed-diff reviews pass. Rollback is the single issue-scoped commit;
there is no persisted data or compatibility shim.
