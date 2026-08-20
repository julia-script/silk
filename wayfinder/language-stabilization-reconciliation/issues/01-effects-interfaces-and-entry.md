# Audit Effects, failures, services, interfaces, and entry boundaries

Type: audit
Status: resolved
Blocked by: —

## Question

For the confirmed Effect execution, Effect contract, typed failure, service, interface,
conformance, discard, and entry rules, what does the current compiler implement, partially
implement, contradict, or omit, and which coherent implementation batches would reconcile the
differences?

## Scope

- `EFF-001–013`
- `FAIL-001–007`
- `SERV-001–010`
- `INTF-001–006`
- `IMPL-001–009`
- `ENTRY-001–005`
- `STMT-001`

For every rule, inspect the semantic path, lower/evaluate/backend consequences where relevant,
focused tests, current OpenSpec requirements, diagnostic ownership, and explicit mismatch notes in
the language reference. Classification must distinguish semantics from diagnostic quality.

## Evidence to inspect

- `docs/language/effects-and-execution.md`
- `docs/language/effect-contracts.md`
- `docs/language/typed-failures.md`
- `docs/language/requirements-and-services.md`
- the interface and conformance sections of
  `docs/language/generics-interfaces-and-specialization.md`
- `docs/language/program-entry.md`
- `docs/language/statements-and-discarding.md`
- relevant compiler actors, evaluator and backend paths, focused tests, diagnostic catalog, main
  OpenSpec requirements, and GitHub issue 226 only as non-authoritative evidence

## Deliverable

Append an answer containing:

1. a per-rule semantic and diagnostic classification;
2. concise evidence links and the important counterexample for every non-implemented result;
3. cross-cutting causes rather than one issue per symptom;
4. ordered implementation handoffs small enough to audit and complete independently; and
5. the next frontier ticket.

## Answer

### Audit result

The current compiler has a substantial, working Effect and static-conformance core, but it exposes
an older language model at several shared seams. The important result is therefore not “interfaces
are broken” or “Effects are unfinished” in general. Seven cross-cutting causes explain almost every
mismatch:

1. failure channels still use a distinct row kind and nominal-only members;
2. incompatible return expressions previously became unavailable HIR without a semantic
   diagnostic; the return-contract soundness handoff has resolved this seam;
3. interfaces repeat their provider while services use partly separate witness machinery;
4. requirement selection and difference use access-bearing `@Role` members instead of
   access-independent service-role keys;
5. entry discovery still requires `Report`, accepts only ordinary `i32`, and coalesces visibility
   with typing failures;
6. compatible Effect construction sites cannot yet form one finite composite representation; and
7. stable logical failure traces and causal reports are incomplete.

GitHub issue 226 was evidence for cause 2, not a new interface rule. The return-contract soundness
handoff now diagnoses both `Effect<i32>`-where-`i32`-is-required and
`i32`-where-`Effect<i32>`-is-required at the source boundary. Its interface-dispatched reproduction
is retained as a regression case.

The focused current-behavior suite passed: 15 test files and 278 tests covering elaboration, Effect
runtime and entry, provider selection, selective catch, acquired providers, generic rows, interface
bounds and witnesses, and conditional conformance. These passing tests prove the classifications
below; they do not make superseded behavior authoritative.

### Evidence anchors

- Effect construction, application, return checking, provision, and selective catch:
  [`Elaboration.ts`](../../../packages/compiler/src/Elaboration.ts),
  [`Lower.ts`](../../../packages/compiler/src/Lower.ts), and
  [`Elaboration.test.ts`](../../../packages/compiler/test/Elaboration.test.ts).
- Effect runtime, nesting, flattening, failure, and cross-engine composition:
  [`EffectRuntime.test.ts`](../../../packages/compiler/test/EffectRuntime.test.ts) and
  [`SelectiveCatch.test.ts`](../../../packages/compiler/test/SelectiveCatch.test.ts).
- Row kinds, normalization, difference, and inference:
  [`Type.ts`](../../../packages/compiler/src/Type.ts),
  [`DeclarationIndex.ts`](../../../packages/compiler/src/DeclarationIndex.ts), and
  [`TypeGenerics.test.ts`](../../../packages/compiler/test/TypeGenerics.test.ts).
- Services, provision, and provider selection:
  [`effects.silk`](../../../packages/compiler/stdlib/silk/effects.silk),
  [`ProviderSelection.ts`](../../../packages/compiler/src/ProviderSelection.ts),
  [`ProviderSelection.test.ts`](../../../packages/compiler/test/ProviderSelection.test.ts), and
  [`ProvideWithAcceptance.test.ts`](../../../packages/compiler/test/ProvideWithAcceptance.test.ts).
- Interfaces and conformances:
  [`Parser.ts`](../../../packages/compiler/src/Parser.ts),
  [`DeclarationIndex.ts`](../../../packages/compiler/src/DeclarationIndex.ts),
  [`UserInterfaceWitness.test.ts`](../../../packages/compiler/test/UserInterfaceWitness.test.ts),
  [`InterfaceBounds.test.ts`](../../../packages/compiler/test/InterfaceBounds.test.ts), and
  [`ConditionalConformanceRejection.test.ts`](../../../packages/compiler/test/ConditionalConformanceRejection.test.ts).
- Entry discovery and execution:
  [`Instances.ts`](../../../packages/compiler/src/Instances.ts),
  [`Report.ts`](../../../packages/compiler-cli/src/Report.ts), and
  [`EffectEntry.test.ts`](../../../packages/compiler/test/EffectEntry.test.ts).
- Diagnostics and expression statements:
  [`Diagnostic.ts`](../../../packages/compiler/src/Diagnostic.ts),
  [`Elaboration.ts`](../../../packages/compiler/src/Elaboration.ts), and
  [`statements-and-discarding.md`](../../../docs/language/statements-and-discarding.md).

### Effect execution and contracts

| Rule | Semantics | Diagnostics | Current evidence and boundary |
| --- | --- | --- | --- |
| EFF-001 | Implemented | Aligned | Effect calls produce lazy `EffectConstruct` facts; execution is reached only through `run`. Runtime tests distinguish construction, execution, and dropping an unrun Effect. |
| EFF-002 | Implemented | Aligned | Every reachable explicit return and fallthrough path is checked against the resolved contract. `SEM0129`/`SEM0130` stop invalid bodies before target-dependent realization; issue 226 is a dispatched regression. |
| EFF-003 | Implemented | Aligned | Lowering executes one Effect contract layer. Nested flattening is an ordinary separate library composition tested across evaluator, LLVM, and Wasm. |
| EFF-004 | Implemented | Aligned | Nested Effect success values and explicit double execution work. A nested value used as the declared scalar result receives `SEM0129`; no implicit flattening occurs. |
| EFF-005 | Implemented | Aligned | Ordinary functions return deferred Effect values and may perform eager work before constructing an `effect {}` body. Stored-Effect runtime tests cover passing, storing, capture, and later execution. |
| EFF-006 | Implemented | Aligned | Ordinary `run` rejects residual failures with `SEM0066` and requirements with `SEM0071`; provision and recovery can close the channels first. |
| EFF-007 | Contradicted | Contradicted | Success and requirements exist, but failure `E` is still a separate row kind instead of an ordinary type. Diagnostics enforce the superseded kind distinction. |
| EFF-008 | Partial | Partial | Effect-function declarations construct one declared Effect layer and enforce its success contract. Their failure binder still inherits EFF-007's superseded failure-row model. |
| EFF-009 | Implemented | Partial | Failure and requirement subsumption is used by function and witness compatibility. Messages and source forms still expose the old failure kind and requirement selector model. |
| EFF-010 | Implemented | Aligned | An omitted effect-function result resolves to `()`; ordinary-function omission remains separate. |
| EFF-011 | Implemented | Aligned | Omitted failure and requirement channels resolve to `never` and the empty requirement row. |
| EFF-012 | Contradicted | Contradicted | Generic `?R` rows work, but generic failures require `!E` and value-level `Row<!E>`. Ordinary generic failure type `E` is rejected. |
| EFF-013 | Contradicted | Contradicted | `SEM0069` intentionally rejects compatible Effects from distinct construction sites. Current representation specs and tests encode the opposite of the confirmed rule. |

### Typed failures

| Rule | Semantics | Diagnostics | Current evidence and boundary |
| --- | --- | --- | --- |
| FAIL-001 | Contradicted | Contradicted | Failure members are restricted to concrete nominal types; built-in or structural detached values receive `SEM0061`. Borrow-backed nominal payload rejection with `SEM0073` is already useful. |
| FAIL-002 | Implemented | Aligned | `fail` is effect-body-only, transfers its payload under ordinary ownership, and terminates the path as `never`; invalid escape and use receive existing semantic/ownership diagnostics. |
| FAIL-003 | Implemented | Aligned | For admitted current failure members, `run` propagates exactly the declared payload and `SEM0066` guards an ordinary boundary. General eligibility remains FAIL-001's mismatch. |
| FAIL-004 | Contradicted | Contradicted | `catch` and `catchAll` currently require one shared success type `A`; they cannot normalize protected and handler successes into `A | B`. |
| FAIL-005 | Contradicted | Contradicted | Selective catch supports one nominal member using `S in E`, `!E`, and `Row<!E>`. It does not accept an ordinary selected type or selected union and cannot express the confirmed total partition directly. |
| FAIL-006 | Partial | Partial | Cleanup order and an evaluator failure trace exist. Stable logical traces across optimized engines and causal `while handling` context are not implemented. |
| FAIL-007 | Implemented | Partial | Arithmetic and bounds traps remain outside typed handlers and cleanup. Stable trap reporting and process-status policy are not fully assigned. |

### Services and requirement provision

| Rule | Semantics | Diagnostics | Current evidence and boundary |
| --- | --- | --- | --- |
| SERV-001 | Partial | Contradicted | Mapped actor functions are complete and statically checked. General inline bodies are parsed only as the special one-hook form and otherwise rejected with `SEM0083`. |
| SERV-002 | Contradicted | Missing | Requirement resolution accepts any nominal capability, including structs, rather than checking declaration-level service eligibility. |
| SERV-003 | Contradicted | Contradicted | A service cannot participate as an ordinary interface bound, and witness construction contains service-specific compatibility and operation paths plus special `Report` handling. |
| SERV-004 | Partial | Partial | Canonical service-role identity, `DefaultRole`, and strongest-access normalization work, but source and diagnostics use `@Role`; confirmed `at Role` is not the accepted form. |
| SERV-005 | Partial | Partial | Generic requirement rows remain symbolic, normalize, infer, and specialize correctly under the old concrete member spelling and selector rules. |
| SERV-006 | Implemented | Partial | Stored shared/exclusive requirement access is separate from shared/exclusive/owned provider capture. Rendered role syntax remains stale. |
| SERV-007 | Partial | Partial | Unique inference, no-match, ambiguity, conformance, and access checks exist. Explicit selection names an access-bearing row member rather than the service-role key. |
| SERV-008 | Contradicted | Contradicted | `Without` compares exact stored access, so a shared selector does not remove the exclusive entry for the same service-role key. Main specs explicitly require this superseded behavior. |
| SERV-009 | Partial | Partial | Provision is lazy, lexical, ownership-aware, and one-layer. It inherits the wrong selector, difference, and role syntax. |
| SERV-010 | Partial | Partial | Fresh per-run acquisition and scoped cleanup exist as `Effect.provideWith`; the confirmed API name `provideEffect` and key-based selector are absent. |

### Interfaces and conformances

| Rule | Semantics | Diagnostics | Current evidence and boundary |
| --- | --- | --- | --- |
| INTF-001 | Contradicted | Missing | Interfaces declare the provider as an explicit type argument; `Self` is not the implicit contextual provider binding. |
| INTF-002 | Partial | Partial | Interfaces accept bodyless ordinary/effect operation contracts and reject most invalid members. They still use failure-row binders and a parser path shared mechanically, not semantically, with services. |
| INTF-003 | Contradicted | Contradicted | Bounds mix hidden-provider shorthand with applications that repeat the provider. The confirmed uniform `T: Interface<Arguments>` application is unavailable. |
| INTF-004 | Not implemented | Missing | The parser/declaration model retains at most one bound per parameter; `+` conjunction has no source representation. |
| INTF-005 | Partial | Partial | Literal operand ownership, result compatibility, row subsumption, and resolved return-contract diagnostics work for mapped witnesses. Old provider/failure forms still violate the complete confirmed surface. |
| INTF-006 | Partial | Partial | Unique bound operations select static witnesses and ambiguity is detected. Application identity still includes the explicit provider, and selected operator names retain legacy privilege. |
| IMPL-001 | Contradicted | Contradicted | `impl Interface<Provider, Arguments> for Provider` repeats the provider; implicit `Self` and general inline bodies are unavailable. |
| IMPL-002 | Partial | Partial | Mapped completeness, duplicates, and unknown members are checked. Inline completeness exists only for the special hook path. |
| IMPL-003 | Partial | Partial | Function kind, literal operands, results, and row subsumption are compared. The comparison still adapts explicit providers and service-specific contracts and inherits the old failure kind. |
| IMPL-004 | Not implemented | Missing | Conformances are indexed program-wide with no provider-defining-module locality diagnostic or focused test. Mapped targets resolve in the provider actor, but a foreign module may declare the conformance fact. |
| IMPL-005 | Implemented | Partial | There is no independent conformance import or supported visibility modifier, and witness availability follows the endpoint declarations. Unsupported modifier recovery has no dedicated conformance explanation. |
| IMPL-006 | Partial | Partial | Bounded generic `impl` declarations and delayed concrete proof work. They retain duplicated providers and cannot express `+` conjunction from INTF-004. |
| IMPL-007 | Implemented | Aligned | Generic heads are alpha-normalized and duplicate or potentially overlapping applications are rejected deterministically before proof selection. |
| IMPL-008 | Partial | Aligned | Structural descent and nontermination rejection work, but their goal shapes still include the superseded provider argument. |
| IMPL-009 | Partial | Partial | Concrete proof chains and static witness targets are deterministic. Full conformance depends on removing provider duplication and service-specific witness branches without weakening proof. |

### Entry and discarded values

| Rule | Semantics | Diagnostics | Current evidence and boundary |
| --- | --- | --- | --- |
| ENTRY-001 | Implemented | Contradicted | Entry discovery checks `Public`, but private `main` is folded into `UntypedEntry` and reported as an unresolved return type. |
| ENTRY-002 | Implemented | Aligned | A valid effect entry is constructed and executed once; success/failure behavior is covered across evaluator, LLVM, direct Wasm, and native shim tests. |
| ENTRY-003 | Contradicted | Contradicted | Unhandled failure closure exists only after every nominal failure conforms to compiler-sealed `Report`. Failure-member ordinals and incomplete reports also differ from the confirmed status/report rules. |
| ENTRY-004 | Implemented | Partial | Any residual requirement makes the effect entry unavailable. The CLI reports only a generic unresolved-capability reason rather than listing the complete row. |
| ENTRY-005 | Contradicted | Contradicted | Ordinary entry discovery accepts only `i32`; explicit `()` is rejected and the CLI explicitly states the old `i32`-only rule. |
| STMT-001 | Implemented | Aligned | `SEM0087` rejects ignored non-unit/non-`never` results and recommends binding, returning, or explicit `drop`; dropping an Effect does not execute it. |

### Authoritative-looking artifacts that encode superseded rules

These artifacts are reconciliation targets. Until revised through an audited OpenSpec change, an
agent can easily mistake them for current language authority:

| Superseded model | Current artifact evidence |
| --- | --- |
| `!E` as a separate kind, nominal-only failure members, exact row difference | [`bootstrap-type-generics`](../../../openspec/specs/bootstrap-type-generics/spec.md), [`bootstrap-flow-functions`](../../../openspec/specs/bootstrap-flow-functions/spec.md), and `effects.silk` |
| Explicit/repeated interface provider and service-specific conformance behavior | [`bootstrap-complete-interface-contracts`](../../../openspec/specs/bootstrap-complete-interface-contracts/spec.md), [`bootstrap-service-declarations`](../../../openspec/specs/bootstrap-service-declarations/spec.md), conditional-conformance specs, parser, and declaration index |
| Access-bearing `@Role` selectors and subtraction | `bootstrap-type-generics`, `bootstrap-flow-functions`, provider-selection tests, and standard-library signatures |
| Required `Report`, ordinary `i32`-only entry, and member-ordinal failure tags | [`bootstrap-instances`](../../../openspec/specs/bootstrap-instances/spec.md) and [`bootstrap-entry-termination`](../../../openspec/specs/bootstrap-entry-termination/spec.md) |
| Construction identity makes compatible Effect joins invalid | [`bootstrap-flow-functions`](../../../openspec/specs/bootstrap-flow-functions/spec.md), representation-parameter specs, and `SEM0069` tests |

### Ordered implementation handoffs

The audit recommends these independent handoffs. Each should become or revise an SLP/OpenSpec
change, receive a planning audit, and be implemented before the next dependent handoff begins.

1. **Resolved — reject invalid return contracts before lowering.** The compiler now reports the
   general return mismatch and missing-return diagnostics, prevents invalid bodies from reaching
   target-dependent realization, and covers ordinary and interface-dispatched issue 226 shapes.
2. **Make failures ordinary types and generalize recovery.** Replace `!E` binders and `Row<!E>`,
   admit every detached owned failure type, preserve union difference over ordinary `E`, generalize
   `catch<S>` to selected unions and `A | B` success, and update the standard library and specs
   atomically.
3. **Unify interface and service identity around implicit `Self`.** Remove provider duplication,
   make service declaration eligibility the only special check, and route service bounds,
   conformances, operation contracts, and static calls through the ordinary interface machinery.
4. **Complete conformance source and coherence boundaries.** Admit mixed inline/mapped operations,
   add bound conjunction, enforce provider-module locality, preserve endpoint-only visibility, and
   retain overlap/termination/static-proof behavior.
5. **Normalize requirement keys and provision APIs.** Introduce `at`, key-based selectors and
   subtraction, access compatibility as a separate provider relation, and rename `provideWith` to
   `provideEffect` with no compatibility alias.
6. **Align entry discovery and terminal failures.** Accept explicit ordinary `()`, diagnose private
   `main` accurately, remove `Report`, list unresolved requirements, and implement the confirmed
   status and report minimum.
7. **Admit finite compatible Effect joins.** Replace `SEM0069` construction-identity rejection
   with a finite allocation-free composite realization across evaluator, LLVM, and Wasm.
8. **Complete failure observability.** Preserve stable optimized logical traces and causal recovery
   context after the semantic and entry models no longer depend on superseded failure machinery.

The existing suspension OpenSpec remains parked and valid. Nothing in this audit invalidates its
exact-channel or one-layer assumptions, but implementing it before handoffs 1–5 would force a large
rewrite across seams already scheduled to change.

### Next frontier

The next frontier is
[02 — ownership, borrowing, captures, and callable application](02-ownership-borrowing-and-callables.md).
That audit should reuse the return-soundness finding rather than reclassifying invalid MIR as an
ownership rule.
