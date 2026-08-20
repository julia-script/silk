# Audit ownership, borrowing, captures, and callable application

Type: audit
Status: resolved
Blocked by: 01

## Question

For the confirmed function, call, ownership, borrowing, capture, callable, pipeline, cleanup,
allocation, Effect-lifecycle, and returned-view rules, what does the current compiler implement,
partially implement, contradict, or omit, and which coherent implementation batches would
reconcile the differences?

## Scope

- all 44 rules in `docs/language/ownership-and-borrowing.md`;
- `FUNC-001–002`, `CALL-001–002`, `RETURN-001`, `CALLABLE-001–003`, and `PIPE-001` in
  `docs/language/functions-callables-and-control-flow.md`;
- the ownership consequences of branch, match, and loop control, while leaving their general
  syntax and value semantics to ticket 04.

Repeated rule identifiers are page-qualified in the tables below. This audit classifies 53
reference rules exactly once.

## Evidence to inspect

- `docs/language/ownership-and-borrowing.md`
- the function, call, callable, return, and pipeline sections of
  `docs/language/functions-callables-and-control-flow.md`
- ownership, elaboration, declaration, MIR, lowering, evaluation, layout, backend, focused test,
  diagnostic-catalog, and current OpenSpec evidence

## Answer

### Audit result

Silk already has a substantial affine-ownership implementation. Whole-value moves, path-sensitive
liveness, call-scoped loans, shared and exclusive reborrows, match and loop ownership, deterministic
cleanup, restricted Drop hooks, owned allocations, callable and Effect capture modes, stored
environment cleanup, and returned slice provenance all have working compiler paths and focused
tests. Of 53 scoped rules, 36 are implemented, 11 are partial, and 6 are contradicted. No rule is
unknown or wholly absent.

The mismatches collect around six seams rather than indicating a generally unfinished borrow
checker:

1. source `Copy` is not an opt-in sealed property: ordinary ownership still treats every nominal
   as move-only while some cleanup code separately computes structural Copy;
2. borrow formation is still tied to restricted named/whole-array operands, so hidden temporary
   owners and ordinary local borrowed bindings are rejected;
3. callable sections encode only one omitted leading parameter and deliberately reject deeper
   trailing partial application;
4. callable capture loans require explicit drop after their final invocation, while Effect loans
   already use last-run lifetime shortening;
5. represented callable and Effect fields have milestone-specific move-only and extraction rules
   instead of deriving ordinary aggregate ownership from their realized fields; and
6. the otherwise mature lifecycle edge still inherits the general return-contract hole, old
   `OutOfMemoryError` spelling, and unfinished suspension/terminal-report reconciliation.

The focused suite passed all 212 tests across 21 files. One Wasm heap-flatness test exceeded its
default five-second timeout in the combined run and passed when rerun alone with a 20-second test
timeout; this is test-cost evidence, not a semantic failure.

### Evidence anchors

- Ownership facts, moves, paths, capture loans, and cleanup:
  [`Ownership.ts`](../../../packages/compiler/src/Ownership.ts),
  [`Ownership.test.ts`](../../../packages/compiler/test/Ownership.test.ts), and
  [`bootstrap-ownership`](../../../openspec/specs/bootstrap-ownership/spec.md).
- Borrow formation, returned provenance, lowering, and MIR loan verification:
  [`Elaboration.ts`](../../../packages/compiler/src/Elaboration.ts),
  [`DeclarationIndex.ts`](../../../packages/compiler/src/DeclarationIndex.ts),
  [`Lower.ts`](../../../packages/compiler/src/Lower.ts),
  [`Mir.ts`](../../../packages/compiler/src/Mir.ts),
  [`RuntimeSliceSemantics.test.ts`](../../../packages/compiler/test/RuntimeSliceSemantics.test.ts),
  and [`RuntimeSliceOwnership.test.ts`](../../../packages/compiler/test/RuntimeSliceOwnership.test.ts).
- Callable contracts, sections, application, and pipelines:
  [`Elaboration.ts`](../../../packages/compiler/src/Elaboration.ts),
  [`bootstrap-callable-values`](../../../openspec/specs/bootstrap-callable-values/spec.md),
  [`IndirectCallAcceptance.test.ts`](../../../packages/compiler/test/IndirectCallAcceptance.test.ts),
  and [`OperatorPipeline.test.ts`](../../../packages/compiler/test/OperatorPipeline.test.ts).
- Represented callable and Effect storage:
  [`CallableFieldRealization.ts`](../../../packages/compiler/src/CallableFieldRealization.ts),
  [`bootstrap-nominal-callable-storage`](../../../openspec/specs/bootstrap-nominal-callable-storage/spec.md),
  [`bootstrap-nominal-effect-storage`](../../../openspec/specs/bootstrap-nominal-effect-storage/spec.md),
  [`StoredCallableOwnership.test.ts`](../../../packages/compiler/test/StoredCallableOwnership.test.ts),
  and [`StoredEffectOwnership.test.ts`](../../../packages/compiler/test/StoredEffectOwnership.test.ts).
- Drop, allocation, and cross-engine cleanup:
  [`DropHookExecution.test.ts`](../../../packages/compiler/test/DropHookExecution.test.ts),
  [`OwnedAllocation.test.ts`](../../../packages/compiler/test/OwnedAllocation.test.ts),
  [`StoredCallableCleanup.test.ts`](../../../packages/compiler/test/StoredCallableCleanup.test.ts),
  and [`StoredEffectEngineParity.test.ts`](../../../packages/compiler/test/StoredEffectEngineParity.test.ts).

### Functions, calls, callable values, and pipelines

These identifiers refer to `functions-callables-and-control-flow.md`.

| Rule | Semantics | Diagnostics | Current evidence and boundary |
| --- | --- | --- | --- |
| FUNC-001 | Partial | Missing | Explicit parameters, omitted `()` results, and declared contracts work. General incompatible returns can still become unavailable HIR without a source diagnostic, and Effect channels retain ticket 01's old row model. |
| FUNC-002 | Implemented | Partial | Ordinary calls execute eagerly and effect calls construct lazily. Invalid nested-Effect/result uses inherit the missing return mismatch rather than changing execution timing. |
| CALL-001 | Implemented | Aligned | Elaboration, evaluation, and pipeline provenance preserve callable-once then arguments left-to-right; terminal earlier arguments prevent later runtime evaluation. |
| CALL-002 | Partial | Contradicted | Full positional calls, type checks, no implicit conversions, and over-application work. A valid deeper trailing suffix is rejected as `SEM0079`, so the confirmed section exception is missing. |
| RETURN-001 | Partial | Missing | Compatible returns and unit fallthrough work. The parser still requires a syntactically trailing return in some semantically terminal bodies, and incompatible returned expressions lack the stable general semantic diagnostic identified in ticket 01. |
| CALLABLE-001 | Implemented | Aligned | Named functions are first-class, statically specialized callable values; incompatible signatures, invocation modes, and identity erasure are checked before lowering. |
| CALLABLE-002 | Contradicted | Contradicted | `callArityDiagnostic` and the current OpenSpec intentionally allow only the unary-section case `K = N - 1`; confirmed arbitrary nonempty trailing suffixes are rejected with `SEM0079`. |
| CALLABLE-003 | Implemented | Aligned | Shared, exclusive, and take invocation modes describe environment access, admit only safe weakening, and are enforced for direct and represented callables. |
| PIPE-001 | Implemented | Aligned | Pipeline facts explicitly record `LeftThenCallable`; the right side is an ordinary unary callable and ownership uses the same application path. |

### Owned values, calls, and structured ownership

These and the remaining tables refer to `ownership-and-borrowing.md`.

| Rule | Semantics | Diagnostics | Current evidence and boundary |
| --- | --- | --- | --- |
| OWN-001 | Contradicted | Missing | `Ownership.categoryOf` classifies every nominal as move-only; there is no accepted opt-in `impl Copy` ownership surface. Separately, `DeclarationIndex.copyType` structurally classifies all-Copy structs for Drop validation, creating two incompatible Copy notions. |
| OWN-002 | Implemented | Aligned | Copy scalar reads preserve their source; affine by-value use requires explicit `move`, while projections, borrow operands, and assignment targets retain their distinct access modes. |
| OWN-003 | Implemented | Aligned | Explicit move consumes Copy and affine bindings alike, transfers the obligation, and later use receives `OWN0001` related to the move. |
| OWN-004 | Implemented | Aligned | Non-Copy field and element extraction is rejected as `OWN0002`; Copy leaves remain readable and whole aggregates remain transferable. |
| OWN-005 | Implemented | Aligned | Writes require a live mutable root, reject immutable or loan-conflicting access, clean the displaced complete value, and leave the root initialized. |
| OWN-006 | Implemented | Aligned | Overlapping assignment sources and destinations are detected and rejected with `OWN0004` before the replacement can consume its own root. |
| OWN-007 | Implemented | Aligned | Fixed-array ownership and cleanup recurse through the element type; indexed affine extraction remains a partial move. |
| OWN-008 | Contradicted | Contradicted | Whole-union movement and active-payload cleanup work, but ordinary ownership classifies every union as move-only instead of deriving Copy when every alternative is Copy. Valid Copy-union reads therefore require a transfer they should not need. |
| CALL-001 | Implemented | Aligned | Owned, shared, and exclusive parameter types select transfer or loan behavior; affine owned arguments require explicit move. |
| CALL-002 | Implemented | Aligned | Owned results transfer to the caller, moved sources are omitted from callee cleanup, and borrowed results enter the separate returned-view path. |
| FLOW-001 | Implemented | Aligned | Liveness is path-sensitive: a move on a returning path does not poison a continuing path, while a join reached with any missing owner rejects the later use. |
| MATCH-001 | Implemented | Aligned | Copy, move, shared, and exclusive match access are explicit and checked against scrutinee ownership and mutability. |
| MATCH-002 | Implemented | Aligned | Borrowed pattern fields remain arm loans; consuming fields become selected-arm owners; guarded consuming bindings remain provisional. |
| LOOP-001 | Implemented | Aligned | Loop headers use a deterministic ownership fixed point, require repeating paths to restore owners and loans, and clean only exited lexical regions. |
| GENERIC-001 | Partial | Missing | Generic whole-value move and cleanup specialization work without assuming Copy. The sealed `T: Copy` constraint and source conformance surface do not exist, so valid generic duplication cannot be stated. |

### Borrowing and returned views

| Rule | Semantics | Diagnostics | Current evidence and boundary |
| --- | --- | --- | --- |
| BORROW-001 | Implemented | Aligned | Shared loans allow reads, coexist with shared loans, and block mutation, movement, cleanup, and exclusive access for their live range. |
| BORROW-002 | Implemented | Aligned | Exclusive loans require a mutable root, permit read/write through the reference, and exclude every independent root access. |
| BORROW-003 | Implemented | Aligned | Borrow formation preserves the root owner and moving through borrowed storage is rejected rather than transferring ownership. |
| BORROW-004 | Implemented | Aligned | Argument loans begin during ordered argument evaluation and cover later arguments plus the complete ordinary callee execution. |
| BORROW-005 | Implemented | Aligned | Reborrowing preserves or weakens access, records parent suspension, and restores parent access after the child loan ends. |
| BORROW-006 | Contradicted | Contradicted | The confirmed rule permits hidden owners for borrowable temporaries and stable indexed results. Current elaboration rejects temporary and indexed operands with `SEM0056`, effectively requiring a restricted source-level root. |
| BORROW-007 | Implemented | Partial | Reference and slice projections preserve shared/exclusive access and mutation rules. Moving an affine field through a reference still uses aggregate `OWN0002` rather than a consistently borrowed-storage diagnostic. |
| BORROW-008 | Contradicted | Contradicted | Borrow parameters and compiler-retained returned views exist, but a direct local borrow binding is intentionally rejected with `SEM0055`; the confirmed ordinary lexical borrowed binding is unavailable. |
| BORROW-009 | Implemented | Aligned | Slices carry runtime length independently from their static type; engines check runtime indices and trap before invalid replacement evaluation. |
| VIEW-001 | Partial | Partial | Exactly-one-source provenance and `SEM0091`/`SEM0092` are implemented for slice returns. Other lifetime-bearing view types have not yet been reconciled through the same general rule. |
| VIEW-002 | Partial | Contradicted | Named caller-owned sources retain loans through the returned view's last use. A temporary argument cannot gain the confirmed hidden caller owner because BORROW-006 rejects it first. |

### Pipelines, captures, Effects, and represented storage

| Rule | Semantics | Diagnostics | Current evidence and boundary |
| --- | --- | --- | --- |
| PIPE-001 | Implemented | Aligned | The left value is checked exactly once and supplied under the leading parameter's ordinary move or borrow contract. |
| CAPTURE-001 | Implemented | Aligned | Section and Effect construction acquire snapshots, loans, and moved owners immediately without executing delayed bodies. |
| CAPTURE-002 | Implemented | Aligned | Captures use Copy, shared, exclusive, or take access and become ordinary hidden-environment fields with corresponding mode and cleanup. |
| CALLABLE-001 | Contradicted | Contradicted | Unary leading sections work, but a section awaiting two or more leading parameters is explicitly rejected and cannot be staged under the confirmed rule. |
| CALLABLE-002 | Implemented | Aligned | Capture access derives shared, exclusive, or take invocation and safe mode substitution; repeated take invocation is rejected. |
| CALLABLE-003 | Partial | Contradicted | Capture loans end on callable drop or consuming invocation. Unlike Effect loans, a reusable callable's loan is not shortened after its last invocation, so source must explicitly drop it before conflicting owner access. |
| EFFECT-OWN-001 | Implemented | Aligned | Effect construction records Copy/shared/exclusive/take capture access without running, and transfer or loan restrictions begin at construction. |
| EFFECT-OWN-002 | Implemented | Aligned | Effect access derives from environment use; shared/exclusive/take runs and stored aggregate receiver requirements are enforced. |
| COMPOSE-001 | Implemented | Aligned | Ordinary source composition retains nested callable and Effect environments and derives the strongest access and cleanup obligation instead of granting combinators compiler privilege. |
| STORAGE-001 | Partial | Contradicted | Stored callables and Effects preserve captures, loans, access, and exact cleanup. Current milestones nevertheless force every representation-bearing nominal move-only and use special extraction diagnostic `OWN0013`, instead of deriving ordinary aggregate Copy/affine behavior and `OWN0002`. |

### Cleanup, allocation, Effect lifetime, and traps

| Rule | Semantics | Diagnostics | Current evidence and boundary |
| --- | --- | --- | --- |
| CLEANUP-001 | Implemented | Aligned | Target-neutral cleanup obligations follow moves, skip consumed sources, recurse through aggregates and environments, and execute once. |
| CLEANUP-002 | Implemented | Aligned | Fallthrough, return, break, continue, and typed failure end loans before reverse-region cleanup; Drop hooks precede declaration-order fields and only active union payloads clean. |
| DROP-001 | Implemented | Aligned | Explicit drop consumes complete Copy or affine owners immediately, omits later cleanup, and rejects partial projection drop or live-loan conflict. |
| DROP-002 | Partial | Partial | Restricted automatic infallible Drop hooks and their cross-engine order are implemented. Their exclusion from Copy types currently consults structural `copyType`, not the accepted opt-in marker, so an all-Copy affine nominal is incorrectly barred. |
| CLOSE-001 | Implemented | Aligned | Fallible finalization is already an ordinary consuming function or Effect with ordinary failure, requirement, ownership, and cleanup behavior; no hidden cleanup channel exists. |
| ALLOC-001 | Partial | Partial | Allocation carries private one-owner reclaim authority, detaches from the allocator loan, and cleans exactly once across engines. The public failure is still named `OutOfMemoryError`, not the confirmed `OutOfMemoryError`, and service/failure syntax inherits ticket 01. |
| EFFECT-LIFE-001 | Partial | Partial | Unrun, successful, failing, retrying, and stored Effects clean per-run and retained environment state correctly. Explicit suspension has working ownership facts and cases, but its accepted coroutine-storage reconciliation remains ticket 06 rather than a completed all-target guarantee. |
| TRAP-001 | Implemented | Partial | Traps remain outside typed failure and make no source cleanup guarantee. Stable terminal status and presentation behavior belong to ticket 05 and are not yet fully reflected in current execution boundaries. |

### Authoritative-looking artifacts that encode superseded rules

| Superseded model | Current artifact evidence |
| --- | --- |
| Every nominal is move-only; some other subsystems infer structural Copy | `Ownership.categoryOf`, `DeclarationIndex.copyType`, and the “nominal structs are move-only” ownership requirement |
| Borrows require named whole-array roots and cannot be local values | `bootstrap-ownership` still requires standalone and temporary borrows to remain rejected, plus `SEM0055`/`SEM0056` tests |
| Only a unary leading section exists | `bootstrap-callable-values`, `callArityDiagnostic`, `SEM0079`, and HIR's single `omittedParameter: 0` shape |
| Callable capture loans require explicit drop after use | current ownership last-use planning and callable capture tests, contrasted with Effect last-run shortening |
| Representation-bearing nominals are always move-only and field extraction is special | both nominal storage specs, `OWN0013`, and represented-storage ownership tests |
| Allocation exhaustion uses the unsuffixed error name | compiler standard library and allocation tests |

### Ordered implementation handoffs

1. **Define one sealed Copy property.** Implement opt-in `impl Copy`, validate every field and the
   absence of cleanup, route ownership, union/array derivation, Drop exclusion, generic constraints,
   layout, and diagnostics through that single property, and delete structural/always-move-only
   disagreement.
2. **Generalize owners, places, and lexical borrow values.** Give temporary storage and stable
   subplaces compiler-owned identities, admit local borrow bindings, carry provenance through
   projections and calls, and keep the existing non-escape and last-use rules. This jointly closes
   BORROW-006, BORROW-008, and VIEW-002 instead of adding syntax exceptions.
3. **Generalize trailing sections.** Replace the unary-only section shape with an ordered omitted
   leading-parameter list, allow every `0 < K < N` suffix and staged application, update ownership,
   representation, evaluation, lowering, and engines, and retire `SEM0079` plus its OpenSpec rule.
4. **Unify delayed-value loan endings.** Apply Effect's last-run lifetime calculation to reusable
   callable last invocation while preserving explicit drop, consuming invocation, escape, and
   aggregate-storage lifetimes.
5. **Remove executable-field ownership privilege.** After Copy is authoritative, derive stored
   callable and Effect aggregate ownership from realized fields, use ordinary partial-move rules,
   retain access-specific `OWN0014`/`OWN0015`, and retire `OWN0013` and the milestone-wide move-only
   rule without weakening representation fences.
6. **Complete semantic returns and lifecycle edges through existing handoffs.** Ticket 01 owns the
   general return-contract diagnostic and ordinary failure/service model; ticket 05 owns terminal
   trap reporting; ticket 06 owns all-target suspension storage and cleanup. Rename
   `OutOfMemoryError` atomically with the ordinary-failure change rather than creating an ownership-only
   compatibility alias.

### Next frontier

The next frontier is
[03 — values, generics, representations, and operators](03-values-generics-representations-and-operators.md).
That audit should reuse the single-Copy-property and representation-privilege findings instead of
classifying their downstream type/layout symptoms as independent rules.
