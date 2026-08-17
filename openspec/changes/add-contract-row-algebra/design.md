## Context

See `proposal.md` for motivation and the delta specs for observable behavior. `Type.Effect` and
erased row arguments currently store normalized known members beside forwarded row parameters. That
shape represents union but cannot retain symbolic difference. Generic inference chooses the first
row decomposition that fits, while `analyzeEffectBindRequirement` independently parses its call,
derives provider access and role, searches the requirement row, and reconstructs the result Effect.

Latest main centralizes intrinsic inventory in `Intrinsic.ts`, but the displayed
`Selected | Rest -> Rest` signature is text-only: `EffectRule.BindRequirement` has no semantic
callable contract. The special elaborator is therefore still a second typing implementation.
Current generic permissiveness also treats parameter types as possible requirement matches, which
lets some open bodies through without carrying a proof that specialization can validate.

Contract rows and constraint evidence remain compiler-erased. Generic HIR may be symbolic, but an
instance's rows and evidence must be concrete before dependency discovery, witness reachability,
layout, row-dependent ownership specialization, or lowering. Standard-library combinators remain
ordinary Silk; only sealed `Intrinsic` operations may retain target-neutral runtime privilege.

## Goals / Non-Goals

**Goals:**

- Define failure-set and access-labelled requirement-row algebra precisely.
- Keep `Without` useful for whole-row forward computation without introducing general set-equation
  inference.
- Check generic bodies once by treating their declared constraints as assumptions and forward
  symbolic evidence safely to specialization.
- Give source declarations and intrinsics one callable contract, call normalization path, solver,
  capture-access calculation, and diagnostic model.
- Remove manual row subtraction from requirement binding and exact-member failure handling.
- Make symbolic rows and evidence unrepresentable to row-dependent instance consumers.

**Non-Goals:**

- Expose `Without` over ordinary value types or introduce open value-union parameters.
- Infer unknown row operands by solving arbitrary inverse union/difference equations.
- Introduce access-kinded or role-kinded generic parameters when ordinary types and selected row
  arguments can express the same contract.
- Add runtime row dictionaries, provider lookup, type descriptors, capability names, or role names.
- Preserve the old intrinsic role-selector syntax, flat row serialization, or operation-specific
  typing paths for compatibility.
- Change runtime Effect scheduling, cleanup order, or backend ABI.

## Decisions

### 1. Use deterministic keyed rows with domain-specific algebra

Introduce a pure `FiniteRow` actor parameterized by a policy that supplies a canonical key,
ordering, collision merge, stored-member equality, and presentation. It owns deterministic
construction, union, exact membership/subset, intersection, difference, and keys without importing
`Type` or declaration services.

The domains are:

- structural value unions: nominal key and identity merge, used only for closed internal rows;
- failure rows: canonical nominal key and identity merge;
- requirement rows: capability-role key and access-join collision merge.

Requirement rows contain only `Shared` and `Exclusive` labels. `Take` describes an Effect value or
provider-handle capture; it is not a stored service demand. Requirement union is a pointwise join:

| Left | Right | Stored result |
|---|---|---|
| Shared | Shared | Shared |
| Shared | Exclusive | Exclusive |
| Exclusive | Shared | Exclusive |
| Exclusive | Exclusive | Exclusive |

Checked membership/subset and raw difference compare the normalized stored label exactly:

| Stored left | Selected right | Checked membership | `Without` result for that key |
|---|---|---|---|
| Shared | Shared | present | removed |
| Shared | Exclusive | access mismatch | Shared remains |
| Exclusive | Shared | access mismatch | Exclusive remains |
| Exclusive | Exclusive | present | removed |

Intersection also uses exact normalized stored-member equality rather than the access join used by
union. For requirement rows, `Shared ∩ Shared` is `Shared`, `Exclusive ∩ Exclusive` is `Exclusive`,
and either mixed-access intersection is empty. This exact intersection is also suitable for
combining provider-selection candidate sets.

This separates row identity from provider compatibility. A stronger provider handle may satisfy a
weaker stored demand, but selection returns the exact stored member before subtraction. Constraints
alone never discharge an Effect; the operation body must bind or handle each selected member.

Alternatives considered:

- Key-only difference would let `&Logger` erase `&mut Logger` and hide an exclusive demand.
- Treating access as part of the normalization key would retain both shared and exclusive demands
  instead of joining them.
- Reusing a plain set policy for requirements would leave access collision behavior implicit.

### 2. Make symbolic `Without` forward-only

Introduce `RowAlgebra` over the kind-preserving expression:

```text
Row<Member, RowParameter, MemberParameter>
  = Concrete(FiniteRow<Member>)
  | RowParameter(RowParameter)
  | Singleton(MemberTerm<Member, MemberParameter>)
  | Union(Row, Row)
  | Without(Row, Row)

MemberTerm<Member, MemberParameter>
  = ConcreteMember(Member)
  | SymbolicMember(domainMemberShape<MemberParameter>)

NormalizedRow = {
  expression: Row,
  memberWellFormed: OrderedMap<MemberWellFormedKey, OrderedSet<SourceOrigin>>
}
```

`RowAlgebra` owns construction, substitution, domain-sound simplification, definitional equality,
keys, presentation, and concretization. Substitution always renormalizes concrete members before a
consumer observes them. A concrete singleton canonicalizes to `Concrete`; `Singleton` exists to
lift an open domain member shape without misclassifying it as a row parameter. Examples are the
failure member `S` in catch and a requirement member shape such as `&mut P@Audit`, whose access and
resolved role are fixed while its capability type remains parameterized. Role-kinded parameters
remain out of scope. Substitution of a `SymbolicMember` has three outcomes:

```text
MemberSubstitution
  = Residual(rewrittenSymbolicMember)
  | Concrete(member)
  | InvalidSingleton(reason)
```

Generic-to-generic row substitution produces `Residual` with rewritten parameter identities and
composed member substitutions; domain validation is deferred only for the portions that remain
open. The constraint/specialization layer separately substitutes and rebinds `Assumed` evidence,
so `RowAlgebra` does not depend on constraint evidence. A closed substitution must produce exactly
one valid member or `InvalidSingleton`.
Definitional keys, occurs
checks, serialization, presentation, and parameter discovery distinguish row parameters from every
parameter referenced inside a symbolic member shape. A generic declaration may retain either form,
but every complete application must concretize both before row-dependent instance consumers.

Every symbolic member constructor also contributes a canonical `MemberWellFormed(domain, term)`
key plus its source origin to the normalized row value. Union, `Without`, substitution, and ACI
normalization merge equal keys by unioning origins in canonical `(sourceId, start, end)` order, and
every open simplification preserves the entry even when it removes the corresponding visible
expression. Consequently reductions such as `Without<Singleton(S), Singleton(S)>` may yield an
empty expression but still reject a later `S = never` or multi-member substitution. Definitional
keys and serialized semantic surfaces include only residual obligation keys, never origins;
substitution, occurs checks, and parameter discovery traverse both the keys and their separate
provenance even when the visible expression is empty. One invalid-specialization diagnostic is
emitted per obligation key: its responsible explicit argument or application span is primary when
available, otherwise the canonical first origin is primary, and all remaining origins are ordered
secondary spans. Concretization discharges all obligations before exposing a finite row.

Open expressions use definitional rather than general extensional equality. Union is normalized
associatively, commutatively, and idempotently. Empty operands, identical subtraction operands, and
other reductions are applied only when sound for the row domain and when all erased symbolic-member
well-formedness obligations are retained. In particular, failure-set
cancellation laws are not blindly applied to access-labelled requirement rows, because access join
can invalidate them. Different open shapes that are merely extensionally equivalent may retain
different definitional keys; after substitution closes them, concrete normalization supplies
extensional equality and the instance key.

`Without` never supplies evidence for its operands. A row parameter or lifted member parameter
below it must already be bound by an explicit generic argument, an exact whole-row occurrence in a
parameter/argument pair, a supplied value-argument occurrence, or provider selection's unique
candidate rule. Membership and subset constraints only check an independently bound left operand;
they never enumerate their source row to infer it. Equations such as
`Without<R, Problem> = Other` do not infer `R`. This stratification keeps solving finite and gives
provision the intended order: infer whole `R`, select `S`, then compute `Without<R, S>`.

Alternatives considered:

- General finite-set unification has non-principal and unbounded inverse solutions.
- A single structural canonical form for all open extensional equivalences requires substantially
  heavier Boolean-algebra machinery and is unnecessary when output computation is stratified.

### 3. Use contextual row syntax and source-spellable constraints

`Without<R, S>` is a dedicated contextual syntax node only where a failure or requirement row is
expected. It cannot be shadowed by a nominal declaration. Generic argument lists accept row
expressions when the corresponding binder is a row kind. Calls extend the existing contiguous
value-type prefix rule to accept a contiguous explicit generic prefix containing value,
failure-row, and requirement-row binder kinds, then infer only the suffix from supplied value
arguments and constraints. This replaces the current rejection of explicit row-kind call
arguments.

One `where` clause contains comma-separated constraints:

```silk
where S in R, &mut P provides S from R
```

`where`, `in`, `provides`, and `from` are contextual within that grammar and remain valid
identifiers elsewhere. `Without` operands are delimited by `<...>`, so union has its ordinary row
precedence within each operand. Empty normalized rows use the existing omitted-channel
presentation. Formatters put short constraints on one line and break a list after `where` using the
ordinary continuation indentation.

`S in R` is kind-directed. When `S` is an ordinary value-type binder and `R` is a failure row, it
is singleton membership: an open generic `S` is represented as a lifted member parameter with
assumed evidence, and every complete application must substitute it to exactly one concrete nominal
failure type occurring in `R`. `never`, structural or multi-member unions, references, and
non-failure values are rejected by the common constraint solver. When the left operand is itself a
failure-row or requirement-row expression, `S in R` means exact normalized subset under that
domain policy. Both membership and subset are checking-only and never infer their left operand by
enumerating `R`. This distinction is represented in constraint data and is not a catch-specific rule. Provider
selection has three source-spellable forms: `&P provides S from R`, `&mut P provides S from R`, and
`P provides S from R`. They select with shared, exclusive, and owned access respectively. If `S`
is already bound independently by an explicit generic or another argument occurrence, the relation
first requires exactly one normalized member and then validates that exact
access-capability-role entry. If `S` is unbound, it considers all compatible entries and requires a
unique result. Every public
selector puts `S` first in its generic binder list, so positional-prefix syntax can supply the
complete selected row without spelling later inferred arguments. Expected result types only check
the computed remainder and never bind `S`. Every independently bound `S` must normalize to exactly
one requirement member; empty or multi-member selectors fail cardinality validation before
provider matching regardless of how they were bound.

Provider constraints are a conjunction, not an ordered sequence. When multiple provider-selection
constraints share one unbound selected row, the solver first completely scans each finite source
row without binding `S` and constructs a canonical map from every compatible exact stored-member
key to `Unique(providerMatch) | Ambiguous(witnessIdentities) | Invalid(reason)`. `NoMatch`
contributes no key.
After the current substitution is applied and constraints are normalized, every occurrence with
the same semantic constraint key forms one solved relation. This includes textual duplicates and
previously distinct generic constraints whose keys collide after specialization. Their occurrence
origins are unioned as a canonical source-location set used only for diagnostics, not for
wanted/evidence identity. Any `Assumed` evidence referencing a grouped occurrence specializes to
the one grouped wanted and its eventual concrete evidence. A relation diagnostic is emitted once
per substituted semantic key, at its canonical first occurrence with remaining occurrences as
secondary spans. Distinct relations and per-key statuses are ordered by semantic key, then by
canonical first occurrence only as a stable tie-breaker.
The solver then intersects complete key sets to a fixed point before interpreting per-key status:

1. If one or more relation maps are empty, emit one provider no-match per empty relation in
   canonical relation order, at each relation's canonical primary span, and stop. Statuses in nonempty
   maps are irrelevant because no common key can survive.
2. If every map is nonempty but their key intersection is empty, emit one joint-selection conflict
   at the responsible selected-variable site—the explicit generic argument when present, otherwise
   the complete application span. Its span-free semantic payload contains every relation's
   constraint key and canonical full candidate-key set; a separate diagnostic-location record
   carries each relation's ordered occurrence origins as secondary spans.

   ```text
   JointProviderSelectionConflictPayload = {
     relations: CanonicalList<{
       constraintKey,
       fullCandidateKeySet: CanonicalList<ExactStoredMemberKey>
     }>
   }
   ```
3. Discard statuses for keys outside the intersection. For surviving keys, emit every
   `Ambiguous` or `Invalid` conformance diagnostic in canonical `(memberKey, constraintKey)` order
   at its relation/member primary span with duplicate occurrence spans secondary, and stop without
   binding `S`.
4. If every surviving status is `Unique`, one key binds `S` and retains separate exact-wanted
   evidence for every relation; multiple keys produce one provider ambiguity containing the full
   canonical key list. That ambiguity is primary at the responsible selected-variable site—the
   explicit generic argument when present, otherwise the complete application span—and attaches
   every contributing relation's canonical primary occurrence as an ordered secondary span. Its
   span-free semantic payload is:

   ```text
   ProviderAmbiguityPayload = {
     survivingCandidates: CanonicalList<ExactStoredMemberKey>,
     relations: CanonicalList<{
       constraintKey,
       fullCandidateKeySet: CanonicalList<ExactStoredMemberKey>
     }>
   }
   ```

   Diagnostic locations are transported separately:

   ```text
   ProviderDiagnosticLocations = {
     primary: SourceOrigin,
     relations: CanonicalList<{
       constraintKey,
       origins: CanonicalList<SourceOrigin>
     }>
   }
   ```

   `survivingCandidates` is the common key intersection; each relation record preserves its full
   pre-intersection key set. Thus unequal maps such as `{A, B, C}` and `{A, B, D}` report common
   ambiguity `[A, B]` plus both distinct full candidate-key sets. Location records are excluded from
   semantic payload identity, equality, and source/intrinsic parity comparisons.

Thus `{A, B, C}` intersected with `{C}` selects `C` even when `C` sorts after `A` and `B`; an
ambiguous witness for eliminated `A` is irrelevant, but one for surviving `C` is diagnosed.
Reordering constraints or source rows cannot change substitutions, evidence, diagnostic
multiplicity, identities, or canonical payloads.

Provider compatibility is:

| Provider parameter | May select stored Shared | May select stored Exclusive |
|---|---|---|
| `&P` | yes | no |
| `&mut P` | yes | yes |
| owned `P` | yes | yes |

In every successful case `P` must equal the stored capability or have one unique valid service
conformance witness for it. A role is part of `S`; a role name alone never resolves multiple
capabilities with that role. Fixed parameter modes avoid an open handle type whose capture access
ordinary Silk cannot determine.

### 4. Solve givens and wanteds in ordered strata

Introduce immutable constraint data and a pure unifier below declaration indexing. A call is solved
in these strata:

1. Normalize direct and pipeline call syntax once and collect explicit generic arguments, value
   arguments, expected-type checks, and declared constraints without committing alternatives.
2. Apply ordinary directed call compatibility: success/failure outputs remain covariant,
   requirement inputs contravariant, and parameter modes/access keep their existing directions.
   Use equality-oriented unification only inside that directional relation and for exact whole-row
   occurrences. Do not descend backwards through `Without`, and do not use an expected result to
   bind a generic argument.
3. Treat constraints declared by the body being checked as givens. Turn the callee's specialized
   constraints into wanteds and discharge them only when a given is definitionally identical after
   substitution. This change adds no implicit reborrow, access weakening, subset transitivity, or
   open-row weakening entailment.
4. For a fully applied concrete call, evaluate remaining member, subset, and provider-selection
   wanteds. Member and subset wanteds require an independently bound left operand. Provider
   selections sharing an unbound row variable are solved by canonical candidate-map intersection
   to a constraint-order-independent fixed point. An independently bound `S` first passes exact-one
   cardinality validation and is then validated against every relation.
5. Substitute and normalize computed result rows. Retain unsolved but well-scoped wanteds,
   quantified binders, and evidence on a constrained semantic callable value when application is
   partial; reject an underconstrained wanted only when the application must be complete.

Evidence is explicit compiler data:

```text
ConstraintEvidence
  = Assumed(declarationConstraint, substitution)
  | Member(selectedNominal, sourceRow)
  | Subset(selectedRow, sourceRow)
  | RequirementSelection(solvedWanted, selectedStoredMember, providerType, providerMatch, providerAccess)

ProviderMatch
  = Identity
  | Conformance(WitnessIdentity)
```

`Assumed` evidence may remain symbolic in generic HIR. Specialization substitutes it and evaluates
the referenced obligation to concrete `Member`, `Subset`, or `RequirementSelection` evidence.
`Member` includes the canonical nominal identity and proves both singleton nominality and presence;
`Subset` is reserved for row-kind operands. There are no runtime dictionaries. Missing, weaker, or
differently role-constrained givens do not entail a wanted. A provider-binding proof is either
`Assumed` evidence whose referenced constraint is a provider selection or a concrete
`RequirementSelection`.

Concrete `RequirementSelection` is inseparably branded with the canonical specialized provider
selection wanted—including source row, selected row, provider mode, and provider type—so evidence
for one protected row or access mode cannot discharge another obligation.

Provider selection receives an injected conformance oracle with typed outcomes:

```text
ConformanceOutcome
  = NoMatch
  | Unique(ProviderMatch)
  | Ambiguous(WitnessIdentities)
  | Invalid(Reason)
```

`ProviderMatch`, `WitnessIdentity`, and the outcome are immutable declaration-index-neutral data
owned below elaboration. Elaboration observationally adapts the current declaration-index witness,
including its identity variant, into this data without emitting diagnostics. The pure unifier,
interface inference, row algebra, and evidence actors never import `DeclarationIndex`; the
constraint solver is the sole emitter after explicit selection or shared-variable key intersection.
`NoMatch` omits that candidate key and `Unique` retains it with evidence. `Ambiguous` and `Invalid`
remain statuses on their candidate key until selection determines whether that key survives; only a
surviving status produces its own constraint failure rather than masquerading as no match.
Interface inference reuses only directed compatibility's pure structural component and retains its
own diagnostics.

`WitnessIdentity` is canonical, serialization-safe, and specialization-complete. It distinguishes
source and intrinsic witness origins and contains the declaration/intrinsic identity plus every
specialized generic argument needed to identify a conditional witness instance. Exact provider
equality produces `ProviderMatch.Identity`; a distinct uniquely conforming provider produces
`ProviderMatch.Conformance(WitnessIdentity)`. Both variants remain inside exact-wanted-branded
selection evidence through witness reachability and service-slot specialization.

### 5. Give every callable one semantic contract

Introduce a canonical `CallableContract` containing function kind, generic binders, parameter
modes, parameter/result types, constraints, and capture relationships. Source declaration facts and
intrinsic operation metadata both produce this value. Inventory display, signature help,
application admission, explicit generic arguments, pipelines, partial application, and diagnostic
labels consume it.

The three conceptual intrinsic contracts are source-spellable and place `S` first:

```silk
effect fn Intrinsic.bindRequirement<?S, A, P, !E, ?R>(
  protected: once Effect<A ! E ? R>,
  provider: &P
) -> A ! E ? Without<R, S>
where &P provides S from R

effect fn Intrinsic.bindRequirementMut<?S, A, P, !E, ?R>(
  protected: once Effect<A ! E ? R>,
  provider: &mut P
) -> A ! E ? Without<R, S>
where &mut P provides S from R

effect fn Intrinsic.bindRequirementOwned<?S, A, P, !E, ?R>(
  protected: once Effect<A ! E ? R>,
  provider: P
) -> A ! E ? Without<R, S>
where P provides S from R
```

Fixed reference modes let ordinary effect-call capture finalization derive shared or exclusive
result access without inspecting an open parameter. The owned form follows ordinary value capture:
an affine provider is consumed and makes the result take-once, while a Copy provider is snapshotted
and remains repeatable. Provider-selection access and expression capture access are separate HIR
fields even when their concrete values coincide. The intrinsic post-hook never modifies the result
type.

Remove `EffectRule.BindRequirement` as a typing rule and remove the early
`isEffectBindRequirementTarget` call-analysis branch. After ordinary contract solving, a sealed
operation hook receives the analyzed provider argument, symbolic or specialized result, and a
mandatory proof of the provider-selection wanted. Generic bodies may supply `Assumed` proof;
concrete calls supply `RequirementSelection`. The hook performs only validations supported by the
fixed parameter mode, records captures, and constructs proof-bearing generic
`EffectBindRequirement` HIR without requiring a concrete capability, role, base provider type, or
witness. The instance frontier upgrades it to a branded concrete binding with
`RequirementSelection`. The hook cannot enumerate candidates, infer access or roles, subtract a
row, or call `Type.effect` to rebuild its result.

Structural tests enforce this architecture: inventory rendering and admission reference the same
contract object; the post-hook cannot be invoked without proof evidence; no intrinsic helper has a
row-enumeration or result-construction API; malformed arity, generic arguments, pipelines, and
partial calls traverse the common call path.

### 6. Concretize evidence at the instance frontier

`Type.Effect`, generic arguments, callable contracts, and generic HIR store `RowAlgebra.Row` values.
HIR binding facts carry proof evidence. Semantic callable values additionally carry any nested
quantified binders, residual constraints, evidence, and substitutions in their equality, keys,
hidden/environment identity, representation checks, module surface, and later application.

Once the substitution for a complete application is known, one specialization frontier substitutes
and concretizes that application's rows and evidence before:

- dependency and reachable-instance discovery;
- provider witness reachability and service-slot shaping;
- layout and callable-field realization;
- row-dependent ownership and cleanup specialization;
- provisional MIR, MIR, evaluation, or backend lowering.

Generic pre-specialization ownership queries use conservative symbolic projections; they never
claim a member was removed. The frontier upgrades every assumed binding proof used by that complete
application to concrete selection evidence. Concrete-only consumers accept a branded specialized
contract/evidence bundle so symbolic application obligations are not representable. Instance
identity is computed from concrete extensional row keys after this frontier, avoiding duplicate
instances from definitionally distinct but concretely equal expressions.

A residual constraint quantified inside an unapplied callable section is not an obligation of the
enclosing instance. It remains compile-time callable-schema metadata until static application, or is
erased when the section is dropped. It never becomes a runtime dictionary. The schema and its
obligations may flow through a closed, statically visible chain of function arguments, returns, or
local storage when that chain ends in a provable complete application; the compiler carries the
same semantic callable through the chain and monomorphizes at that application. An opaque module
boundary, runtime representation, or indirect invocation reached while obligations remain
quantified is rejected after any required closed-world identity-flow analysis and before runtime
representation, row-dependent instance consumers, or lowering.

### 7. Migrate binding, provision, acquisition, and catch

Public wrappers mirror the fixed intrinsic modes and put selected row `S` first:

```silk
pub effect fn bindRequirement<?S, A, P, !E, ?R>(
  self: once Effect<A ! E ? R>,
  provider: &P
) -> A ! E ? Without<R, S>
where &P provides S from R

pub effect fn bindRequirementMut<?S, A, P, !E, ?R>(
  self: once Effect<A ! E ? R>,
  provider: &mut P
) -> A ! E ? Without<R, S>
where &mut P provides S from R

pub effect fn bindRequirementOwned<?S, A, P, !E, ?R>(
  self: once Effect<A ! E ? R>,
  provider: P
) -> A ! E ? Without<R, S>
where P provides S from R
```

`provide` and `provideMut` are ordinary aliases over the shared and exclusive forms. Each generic
body discharges the intrinsic wanted from a definitionally identical enclosing given. The owned
body passes `move provider`; ordinary Copy/affine capture semantics determine repeatability.
Acquisition uses a distinct provider implementation type and puts `S` first:

```silk
pub effect fn provideWith<?S, A, P, !E, !F, ?R, ?Q>(
  self: once Effect<A ! E ? R>,
  acquire: Effect<P ! F ? Q>
) -> A ! E | F ? Without<R, S> | Q
where &mut P provides S from R
```

The acquired owner remains scoped around the protected execution and is dropped before its reified
outcome escapes.

Singleton catch keeps its selected nominal type first while lifting it into row algebra. Ordinary
call inference may infer `S` from the nominal handler parameter, or the caller may supply the
first-position prefix explicitly:

```silk
pub effect fn catch<S, A, !E, !F, ?R, ?Q>(
  self: once Effect<A ! E ? R>,
  onFailure: once fn(S) -> Effect<A ! F ? Q>
) -> A ! Without<E, S> | F ? R | Q
where S in E
```

Here `S in E` is the common kind-directed singleton-membership constraint. It retains an open
lifted member parameter plus assumed evidence in the generic declaration, then rejects `never`,
structural or multi-member unions, and non-nominal values at a complete application before
intrinsic availability. The old
whole-row `Effect.catch` alias is removed; executable whole-row recovery remains `Effect.catchAll`.
Every handwritten whole-row call, fixture, corpus program, and document migrates accordingly;
singleton calls may retain omitted type arguments and infer `S` normally.

Selective dispatch is the sealed `Intrinsic.catchFailure` operation with the same binder order,
parameters, constraints, and result as the wrapper above. `Intrinsic.Operation` replaces its
always-nonempty target list with:

```text
IntrinsicAvailability
  = Executable(nonEmptyTargets)
  | AnalysisOnly(diagnosticIdentity)
```

`catchFailure` is target-independent `AnalysisOnly(SEM0098)`. Its operation hook consumes
membership proof and constructs generic dispatch HIR without filtering or reconstructing the
failure row. No evaluator/backend lowering is added in this change.

Reachable instance discovery records an ordered set of originating source call edges while
expanding an ordinary or user-defined wrapper chain into its intrinsic dependency. Instance
deduplication unions rather than discards these origins. An origin key is
`(sourceId, start, end, intrinsicOperation, diagnosticIdentity)`, independent of owner-instance specialization. The
chain recursively retains its outermost incoming source edge; an intrinsic call's own edge is used
only when that direct call is itself the origin. Immediately after discovery, a
target-independent availability gate diagnoses every reachable `AnalysisOnly` dependency before layout, MIR, or
lowering; target-specific `Executable` selection remains in the target availability path. A
reachable `Effect.catch` therefore attributes `SEM0098` to the user's wrapper application; a
reachable direct intrinsic call uses its own span; an unreachable wrapper or direct call emits
nothing. For each distinct reachable origin edge the gate emits one diagnostic at the outermost
user-written application that entered that analysis-only dependency chain, ordered by canonical
source location; two call sites reaching one deduplicated instance therefore receive two stable
diagnostics, while repeated dependency paths from the same call edge do not duplicate one. Invalid
syntax, kind, inference, or membership fails before availability and suppresses the dependent
`SEM0098`. Elaboration and intrinsic post-contract hooks are structurally unable to emit
availability diagnostics, and `Effect.catch` is never recognized by standard-library spelling.

### 8. Make diagnostics stable and phase-specific

The generated diagnostic catalog gains distinct semantic identities for row kind mismatch, invalid
nominal singleton selection, exact membership access mismatch, checked absence, underconstrained row computation, provider no-match,
joint provider-selection conflict, provider ambiguity, selected-row cardinality, conformance ambiguity, invalid conformance, cyclic
substitution, analysis-only intrinsic availability, and non-concrete specialization. Provider
ambiguity carries canonically ordered common access-capability-role candidates plus an ordered
record of each relation's full candidate set. Source origins live only in a separate ordered
diagnostic-location record. Equivalent source and intrinsic contracts use the same identity and
span-free semantic payload; their primary and secondary spans remain local to each originating call
or constraint.

Diagnostic precedence is syntax/kind error, structural inference or underconstraint, checked
constraint failure, then specialization non-concreteness. `SEM0071` remains only the later run-boundary
diagnostic for an already concrete Effect. Implementation assigns catalog codes before tests, and
tests assert those codes and spans rather than wording.

### 9. Add the remaining capability deltas before implementation

The current OpenSpec update workflow permits edits only to existing delta files. Before applying
the change, artifact continuation must add dedicated deltas for syntax and formatting, semantic
facts and module surfaces, intrinsic inventory/admission, diagnostics, HIR, instances, MIR, Silk
stdlib, and standard-library documentation. Those deltas must expose the grammar, stable semantic
facts, single-contract intrinsic boundary, evidence lifecycle, and concrete-only downstream
contracts described here rather than leaving them as task-only implementation detail.

## Risks / Trade-offs

- **[Constraint evidence leaks into runtime]** → Keep evidence in generic/specialized compiler data
  and require concrete erasure before MIR.
- **[Open expression identity duplicates work]** → Use deterministic definitional keys only before
  specialization and concrete extensional instance keys afterwards.
- **[Access algebra is applied like plain set algebra]** → Centralize the exact access matrix in the
  requirement policy and add exhaustive matrix tests.
- **[Solver/declaration-index dependency cycle]** → Inject a conformance oracle at elaboration;
  lower-level constraint and evidence actors import no declaration service.
- **[Partial applications capture providers before selection]** → Retain residual wanteds and use
  fixed-mode ordinary capture ownership plus constrained-callable metadata until complete
  application, or erase both when the section is dropped.
- **[Combinatorial concrete selection]** → Enumerate each already finite concrete input row once,
  memoize complete canonical per-relation candidate maps, and intersect exact keys; never truncate a
  map needed for conjunction or a complete ambiguity/diagnostic payload.
- **[Generated surface and diagnostic drift]** → Render from semantic contracts and regenerate only
  after semantic and structural single-path tests pass.
- **[Large row-representation migration]** → Introduce the concrete specialization bundle first,
  migrate consumers by dependency order, then delete every legacy flat field.

## Migration Plan

1. Add all deferred capability deltas and pass strict OpenSpec validation before implementation.
2. Add regressions and implement deterministic keyed finite rows plus the requirement access matrix.
3. Add forward-only `RowAlgebra`, migrate Type/declaration representation, and define symbolic keys.
4. Add syntax, facts, formatting, presentation, and diagnostics.
5. Add `CallableContract`, given/wanted solving, evidence, constrained-callable metadata, and the
   injected provider-conformance oracle.
6. Route all fixed-mode requirement-binding intrinsics through common call analysis, delete the
   early typing path, and retain only proof-consuming HIR hooks.
7. Add the instance concretization frontier and migrate every row-dependent consumer.
8. Rewrite public binding/provision/acquisition and singleton catch in ordinary Silk, removing
   standard-library name recognition and intrinsic-only role selection.
9. Regenerate compiler/stdlib/docs artifacts and run the complete release verification sequence.

The repository is unreleased, so there is no compatibility bridge. Rollback reverts the compiler,
stdlib, generated surfaces, and delta specs as one unit; mixing new syntax with legacy row storage or
operation-specific typing is unsupported.
