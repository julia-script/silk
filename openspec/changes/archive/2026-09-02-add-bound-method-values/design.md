## Context

See proposal.md. After `add-method-call-syntax`, `analyzeMethodCall` resolves `value.member(args)`
through `resolveMethodCandidate` and synthesizes the receiver as argument zero
(`synthesizeReceiver`). A bare `value.member` reaches `analyzeProjection`, whose unknown-field
branch asks `receiverMethodOf` only to choose the `SEM0199` diagnostic.

Sections are already positional: `CallableSectionExpressionFact` and `Hir.CallableSection` carry
`remainingParameters` and per-capture `parameterOrdinal`; MIR normalization, layout, ownership,
cleanup, verification, and forwarding all key off those ordinals. Two layers still assume a
trailing suffix: construction (`finishCallableSection`, `analyzeSectionContract`,
`sectionCallableType` compute `captureStart = count - arguments`) and operand assembly at
application (`BootstrapEvaluation` sets arguments at ordinals `0..k` and lets captures overwrite;
`WasmBackend`, `NativeCallOperation`, and `NativeExecutionOperation` emit `[...arguments,
...captures]`).

## Goals / Non-Goals

**Goals:**

- One representation: a bound method value is `CallableSection` with one capture at ordinal zero.
- One operand-ordering rule shared by MIR normalization, the evaluator, and both compiled backends.
- Ownership of the receiver comes from the existing `synthesizeReceiver` and capture machinery.

**Non-Goals:**

- Binding interface operations on generic receivers (no interface-operation callable target exists).
- Extending a temporary receiver's lifetime to the callable's region.
- Any change to completion, which already presents receiver-bound members after `value.`.

## Decisions

### The bound value is produced inside `analyzeProjection`

The unknown-field branch of `analyzeProjection` calls `resolveMethodCandidate`; an `Inherent`
candidate returns a section built from `synthesizeReceiver(subject, parameter zero)` as the single
argument, `NoReceiver` reports `SEM0198`, and everything else keeps the unknown-field diagnostic.
`receiverMethodOf` and `SEM0199` are deleted. Alternative: a separate `analyzeBoundMethod` before
`analyzeProjection` in the projection arm. Rejected: it would analyze the subject twice on
fall-through and duplicate its diagnostics.

### Sections name their captured ordinals

`finishCallableSection`, `analyzeSectionContract`, and `sectionCallableType` take the captured
parameter ordinals aligned with the argument facts (default: the trailing suffix). Remaining
parameters are the ordinals not captured, in order; a section may have zero remaining parameters,
which only the bound path produces. `executableSites` registers `FieldProjectionExpression` nodes
after every call site so bound values get distinct site ordinals without renumbering existing sites.

### One operand-ordering rule

`Mir.applyOperands(captures, arguments)` places each capture's lanes at its parameter ordinal and
fills the remaining ordinals with the supplied arguments in order. `MirNormalization.parametersFor`,
`BootstrapEvaluation` (`invokeStoredCallable`, `ApplyCallable`), `WasmBackend`,
`NativeCallOperation`, and `NativeExecutionOperation.applyCallable` use it for declaration targets.
Builtin targets keep their existing shape: a builtin section is always trailing.

### A temporary receiver is not borrowed into a section

`synthesizeReceiver` is reused unchanged, so an rvalue receiver of a `&Self` or `&mut Self` member
would form a `TemporaryRoot` borrow. The bound path rejects that with the ordinary
borrow-operand diagnostic before constructing the section; a `Self` receiver consumes the rvalue.

### Hover needs no new code

`SemanticOccurrence` collects a section's `reference` with its `path`; a path without a qualifier
on a receiver member yields the `Method` role, and `hoverSubjectAt` already substitutes the
receiver type read from the projection at the member token.

## Risks / Trade-offs

- [Every projection becomes a site entry] → ordinals are identity only; appending projections after
  call sites keeps existing site ordinals and goldens stable.
- [Zero-parameter sections reach layout and backends for the first time] → covered by the
  evaluator test and the native differential corpus program.
