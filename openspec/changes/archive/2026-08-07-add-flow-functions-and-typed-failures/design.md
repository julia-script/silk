## Context

The compiler already carries nominal types, normalized structural unions, affine ownership,
target-selected aggregate calling shapes, a structured MIR DAG, and three agreeing execution
engines. Calls are currently eager and every function returns only a success value or traps.
Wayfinder issues 03 and 08 settle the semantic distinction between ordinary `fn` and lazy `flow fn`,
the `!` row, one-layer `run`, owned abortive failures, and explicit non-unwinding lowering. The next
two roadmap changes own requirement rows/provision and scope wrappers; this change must leave clear
extension points for both without implementing either.

## Goals / Non-Goals

**Goals:**

- Establish an executable, target-aware success-or-failure ABI shared by analysis, MIR, evaluation,
  LLVM, and Wasm.
- Prove laziness, exact propagation, recovery, cleanup, and trap separation with a statically shaped
  flow recipe that can later be wrapped by provision and scopes.
- Keep failure tags, payload packing, and control transfer compiler-owned and deterministic.
- Preserve unavailable facts and byte stability for programs that contain no flow declarations.

**Non-Goals:**

- General function/closure types, source-spelled flow types, dynamically selected flow values,
  escaping flow values, or nested flow results.
- Private contract inference, recursive flow construction, suspension, trampolines, or contract-row
  polymorphism. This slice requires every flow declaration to state its complete row; omitting `!`
  means the explicit empty row.
- The remaining Flow composition family. `Flow.catch<E>` is the single compiler-known operation
  admitted because it makes the new failure branch executable and testable.
- Requirement rows, providers, roles, scopes, allocator operations, interruption, or host adapters.

## Decisions

### 1. Represent flow values as closed compiler-known recipes

Semantic `Flow` identity contains a success type, normalized failure row, and one recipe shape. A
recipe is either a direct call to a known `flow fn` with its analyzed argument captures or an exact
catch wrapper around another recipe plus a known handler `flow fn`. A recipe may be held in an
immutable local and run later, but cannot cross a parameter, result, aggregate, array, union, or
module boundary in this slice.

This gives construction real lazy semantics while keeping the representation erasable: no heap
closure, vtable, interpreter instruction object, or public ABI is introduced. Capability provision
and `Scope.scoped` can add more wrapper recipe variants later. Making every flow an opaque runtime
closure now was rejected because source flow/function types and a general owned-environment ABI are
not settled or required by allocation.

### 2. Use explicit declared nominal failure rows

A declaration is `Ordinary` or `Flow`. A flow header resolves the source-ordered `!` members through
the ordinary type namespace, rejects non-nominal members, and publishes a canonical sorted unique
row. The source row remains inspectable. Public, private, generic, and recursive flow declarations
all state their complete row in this first slice; a missing row is explicitly empty.

This is stricter than Wayfinder's future private non-recursive inference allowance, but does not
change its semantics: explicit rows are always valid and are the prerequisite needed by independent
header collection. Inference is deferred until the compiler has general function-contract values
and recursive contract fixed points. Treating body order as inference was rejected because imports
and mutual recursion would make headers traversal-dependent.

### 3. Make failure origin a terminating statement

`fail move expression` is a `FailStatement` whose expression must produce one owned nominal value.
It is valid only in a flow body and terminates that control path with success type `Never`. Existing
block analysis already reasons about return, break, and continue; fail joins that closed transfer
set. A body may continue syntactically after a conditional fail, but statements after an
unconditional fail are unreachable.

Reusing `return Error { ... }` was rejected because it erases the success/failure distinction.
Encoding failure as a trap was rejected because handlers and cleanup could not observe it.

### 4. Make `run` a prefix expression with row checking

`run recipe` consumes or borrows the recipe according to its captures, executes exactly one layer,
and has the recipe's success type. Within a flow body its residual failure row is unioned into the
body's actual row and checked against the declared row. Within an ordinary function the residual
row must be empty. Only a direct flow call, an immutable binding holding a recipe, or a
`Flow.catch<E>` recipe is accepted; arbitrary runtime expressions are rejected explicitly.

Construction captures arguments left to right but emits no body-entry or body-operation event.
Moved affine captures make the recipe take-only; Copy captures permit repeated runs; borrowed
captures retain their existing loan lifetime and access mode. A mutable flow binding is rejected in
this slice because replacing recipe identity would require general flow values.

### 5. Special-case one exact static catch operation

`Flow.catch<E>(protected, handler)` and `protected |> Flow.catch<E>(handler)` are equivalent
spellings recognized as one compiler-owned qualified actor call. `E` must be exactly one protected
row member. `handler` must resolve directly to a `flow fn` with one owned parameter of type `E` and
the same success type. The wrapper row is canonical
`(protected − E) ∪ handler.failures`. On success the handler is not entered; on failure `E` the
handler receives ownership; other members propagate unchanged.

The handler is a declaration reference rather than a general function value. This intentionally
narrow operation can later become an ordinary higher-order Flow actor when function-contract types
land without changing source spelling or row algebra. A dedicated catch block syntax and throwable
class matching were rejected because Wayfinder prefers qualified actor operations and exact nominal
identity.

### 6. Extend HIR with recipes and explicit transfers

HIR adds `FlowConstruct`, `FlowCatch`, and `Run` expressions plus `Fail` statements. Each recipe
carries its canonical success/failure contract, declaration/type arguments, capture expressions,
handler identity where applicable, capture access, and provenance. `Run` records the residual row
and the containing function's propagation decision. HIR verification checks acyclicity, declared
targets, capture mappings, handler shape, row algebra, and fail membership.

Instance discovery walks recipes structurally and discovers protected bodies and handlers. Instance
keys retain existing concrete type arguments and contract-row identity, but never runtime values,
tags, or outcomes.

### 7. Plan one tagged outcome shape per reachable flow instance

The layout plan adds lazy outcome entries only for reachable flow instances. Tag zero is success;
failure members receive tags starting at one in canonical nominal order. The payload area uses the
existing compiler-owned flattened calling-shape vocabulary and contains enough canonical slots for
the success shape or any failure payload shape. Each variant publishes an exact mapping into those
slots. Native targets use the selected compiler scalar lanes; Wasm uses its existing target lanes.

This mirrors structural-union planning but does not pretend the outcome is a source union: success
may be scalar, arrays and aggregates retain their logical identities, and failures are abortive.
Backend-private result packing without a layout entry was rejected because LLVM, Wasm, and the
evaluator could choose incompatible tags or lanes.

### 8. Keep MIR a DAG with structured run dispatch

A flow `MirFunction` retains its ordinary success result type plus its normalized failure row and
selected outcome shape. Its terminal outcomes are `ReturnSuccess`, `ReturnFailure`, and `Trap`.
Ordinary functions retain the existing plain `Return` representation to preserve non-flow byte
stability.

`RunFlow` is a structured MIR operation. It contains the static recipe, destination success local,
selected outcome shape, exact catch layers, success continuation, and failure continuations. A
propagating continuation first emits compiler-planned cleanup for lexical regions exited, then
returns the same owned member. A matching catch continuation binds the payload to the handler
parameter and executes the handler flow. Structural child references remain acyclic; backends may
linearize them into private CFG branches.

MIR verification recomputes no layout. It validates target/function/handler instances, tags and
variant mappings, local types, residual rows, exact payload identities, catch subtraction, cleanup
before propagation, and absence of failure terminals in ordinary functions.

### 9. Share the outcome contract across all engines

The evaluator represents a flow invocation as `Success(Value)` or `Failure(member, payload)` and
records deterministic call/binding events plus explicit `FlowSuccess` and `FlowFailure` outcomes.
Construction remains visible in HIR and produces no body event; the evaluator never catches a trap.

LLVM returns the selected private outcome aggregate from flow functions and branches on its tag at
each run. Wasm returns the same flattened tag/payload lanes through multi-value results and uses
structured `if`/`block` dispatch. Both reuse existing aggregate lane copy and payload mapping code.
No platform unwinder participates. A flow-free program does not construct outcome types, helper
declarations, or changed symbols.

### 10. Gate the slice with one canonical fixture

The acceptance fixture declares one nominal error payload, one generic flow that either succeeds,
fails with the caught member, or traps, and one handler flow. It proves delayed body entry, exact
catch, unmatched propagation in an internal flow, empty residual row at ordinary `main`, affine
cleanup before propagation, generic instance reuse, native/Wasm/evaluator parity, and fresh-process
artifact determinism. `/labs` exposes pipelined executable recovery and stopped-residual variants
through the unified inspector; there is no standalone flow inspector.

## Risks / Trade-offs

- [Risk] Static recipes are mistaken for the final general Flow representation. → Name the
  restriction in facts and diagnostics, reject escape explicitly, and keep recipe variants private
  so later source-spelled contract types can replace the storage boundary.
- [Risk] Outcome payload packing duplicates union machinery and diverges. → Reuse the same canonical
  lane-slot mapping helpers while retaining a distinct logical outcome identity.
- [Risk] Catch becomes a compiler forever-special form. → Preserve the accepted
  `Flow.catch<E>(flow, handler)` actor spelling and isolate special handling to resolution/lowering;
  later higher-order contracts can desugar the same surface normally.
- [Risk] Cleanup is emitted after payload transfer or twice. → Make propagation a structured MIR
  continuation whose verifier requires cleanup before the single failure terminal; run success and
  catch branches own disjoint obligations.
- [Risk] Adding outcome types perturbs existing artifacts. → Create layout/backend outcome state
  lazily and keep ordinary `fn` MIR and signatures unchanged; retain flow-free golden tests.
- [Trade-off] Private non-recursive failure inference and the rest of the Flow actor family remain
  unavailable. → This keeps the allocation prerequisite small and independently executable; later
  work can add inference and higher-order combinators without changing failure semantics.

## Migration Plan

Add syntax and header facts first, then semantic recipes and ownership, HIR/instances, outcome
layout, structured MIR, evaluator, LLVM, Wasm, acceptance fixtures, and unified Labs projections.
Existing programs require no migration because `flow`, `run`, and `fail` were previously ordinary
identifiers. If three-engine outcome parity or cleanup verification cannot be achieved, remove the
new syntax and representations together; the already archived `Usize` work remains independent.
