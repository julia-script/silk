## MODIFIED Requirements

### Requirement: Effect catch subtracts and composes rows

`Effect.catch(effect, handler)` and `effect |> Effect.catch(handler)` SHALL select one nominal
failure type `S`, inferred from the handler or supplied as the first explicit argument in
`Effect.catch<S>`, require singleton `S` to be a checked member of protected failure row `E`, and
compute the semantic residual as `Without<E, S> | F`, where `F` is the handler's failure row.
Success bypasses the handler and every nonmatching member belongs to the computed remainder.
Membership proves a type relationship but SHALL NOT erase a failure without the handling operation.

The selected nominal binder `S` SHALL be first so positional-prefix syntax `Effect.catch<Problem>`
remains an ordinary generic call. A generic declaration MAY retain open `S` as a lifted singleton
member parameter with assumed membership evidence. At each complete application, whether inferred
or explicit, `S in E` SHALL require `S` to resolve to exactly one concrete nominal failure type;
`never`, a structural or multi-member union, and a non-failure value SHALL fail this ordinary
checked constraint before intrinsic availability.
The whole-row `Effect.catch` alias SHALL be removed; whole-row recovery SHALL use
`Effect.catchAll`. Singleton `Effect.catch` SHALL be an ordinary wrapper over sealed
`Intrinsic.catchFailure`, whose callable contract carries the same checked membership and
`Without` result and whose availability is target-independent `AnalysisOnly(SEM0098)`.
Neither standard-library name recognition nor a separate intrinsic typing rule MAY determine the
residual.

A reachable, otherwise valid direct or wrapped singleton-catch application SHALL report `SEM0098`
at the originating user application span immediately after reachable instance discovery and before
layout, MIR, or lowering. An unreachable application SHALL not report target availability. A
syntax, kind, inference, or membership failure SHALL take precedence and suppress `SEM0098` for
that invalid call. Elaboration and intrinsic post-contract hooks SHALL NOT emit availability
diagnostics. This change adds neither multi-member dispatch nor evaluator/backend execution
support.

#### Scenario: Type singleton catch through a pipeline

- **WHEN** `relay(0) |> Effect.catch<Problem>(recover)` is analyzed before target availability
- **THEN** the call contract gives `recover` the nominal `Problem` payload and computes the protected remainder with `Without`

#### Scenario: Infer singleton catch from its handler

- **WHEN** `Effect.catch(effect, recoverProblem)` supplies a handler whose input is nominal `Problem`
- **THEN** ordinary call inference selects singleton `Problem`, checks `Problem in E`, and computes the same remainder as `Effect.catch<Problem>(effect, recoverProblem)`

#### Scenario: Reject a non-singleton catch selector

- **WHEN** explicit arguments or handler inference would select `never`, `First | Third`, or a non-nominal failure payload for `S`
- **THEN** the common checked-constraint solver reports an invalid failure selector before intrinsic availability and without constructing multi-member dispatch

#### Scenario: Preserve an open failure remainder

- **WHEN** a generic catch selects nominal `Problem` from open row `E` under checked membership
- **THEN** its declared failure row remains `Without<E, Problem> | F` until specialization and preserves every nonmatching failure

#### Scenario: Reject absent selected failure

- **WHEN** `Effect.catch<Problem>` protects a concrete failure row that does not contain `Problem`
- **THEN** analysis reports the failed membership constraint at the call without also reporting `SEM0098`

#### Scenario: Reject reachable singleton catch on current targets

- **WHEN** an otherwise valid `Effect.catch<Problem>` application is executable-reachable
- **THEN** availability reports `SEM0098` at that wrapper application rather than at the standard-library body

#### Scenario: Ignore unreachable singleton catch availability

- **WHEN** an otherwise valid `Effect.catch<Problem>` appears only in an unreachable declaration
- **THEN** analysis records its semantic contract but emits no `SEM0098`

#### Scenario: Ignore an unreachable direct analysis-only intrinsic

- **WHEN** an otherwise valid `Intrinsic.catchFailure<Problem>` call appears only in an unreachable declaration
- **THEN** reachable instance discovery does not admit it to the post-discovery availability gate and no `SEM0098` is emitted

#### Scenario: Diagnose a direct analysis-only intrinsic call locally

- **WHEN** an otherwise valid executable-reachable `Intrinsic.catchFailure<Problem>` call is written directly
- **THEN** availability reports `SEM0098` at that direct intrinsic application with the same structured reason as the wrapper call

#### Scenario: Preserve origins through a nested wrapper

- **WHEN** a user-defined wrapper chain reaches singleton catch and the outer wrapper is called from one reachable source application
- **THEN** post-discovery availability reports one `SEM0098` at that outermost application rather than at an intermediate wrapper body

#### Scenario: Diagnose every distinct call to one deduplicated instance

- **WHEN** two reachable source applications specialize to one deduplicated wrapper instance containing singleton catch
- **THEN** post-discovery availability emits one `SEM0098` per distinct originating call edge in canonical source order and does not duplicate either diagnostic for repeated dependency paths

#### Scenario: Migrate whole-row catch to catchAll

- **WHEN** source needs executable whole-row recovery with a `Row<!E>` handler rather than one nominal handler input
- **THEN** it uses `Effect.catchAll`; omitting explicit generic arguments from a nominal-handler `Effect.catch(effect, handler)` instead performs ordinary singleton inference and does not select the removed whole-row alias

#### Scenario: Preserve singleton dispatch scope

- **WHEN** a caller wants to handle `First | Third` from a larger row
- **THEN** it composes singleton catches or another source operation while set-to-set `Without` remains only the underlying type algebra in this change

### Requirement: Provision distinguishes shared borrow, exclusive borrow, and acquisition

Requirement binding SHALL relate whole protected row `R`, exact selected singleton row `S`, and one
of three source-spellable provider parameter modes:

- shared `&P provides S from R`;
- exclusive `&mut P provides S from R`;
- owned `P provides S from R`.

Shared providers SHALL select only shared stored requirements. Exclusive and owned providers MAY
satisfy shared or exclusive stored requirements, but selection MUST return the stored member's
original access and role. For every mode, `P` MUST equal the selected capability or have one unique,
valid service-conformance witness.

When `S` is unbound, every compatible capability-role entry SHALL be considered and exactly one
candidate MUST remain. Whenever `S` is independently bound—by an explicit row generic argument or
by another supplied-argument occurrence—the constraint SHALL first require it to normalize to
exactly one member, then validate that member and its provider match (identity or one unique
conformance witness). Empty or multi-member
bound selectors SHALL report selector-cardinality failure before provider matching. Explicit
disambiguation SHALL name
the complete access-capability-role entry; there SHALL be no role-only intrinsic filter and an
expected result row MUST NOT disambiguate selection. No match, multiple inferred members, ambiguous
conformance, and invalid conformance SHALL have distinct deterministic outcomes before execution.
When multiple provider constraints share an unbound `S`, analysis SHALL solve them conjunctively by
completely scanning each finite source row, retaining a per-key
`Unique | Ambiguous | Invalid` provider-match status, and intersecting the complete exact-member key
sets before interpreting status rather than binding in source order. If any relation map is empty,
analysis SHALL emit one local provider no-match for every empty relation in canonical constraint-key
order and no other conformance or joint diagnostic. If all maps are nonempty but their intersection
is empty, analysis SHALL emit one joint-selection conflict whose span-free semantic payload carries
every constraint key and canonical full candidate-key set. That conflict SHALL be primary at the explicit selector argument when present and
otherwise at the complete application, with contributing relation origins ordered as secondary
spans in a separate diagnostic-location record. Statuses on eliminated keys SHALL be ignored. Every `Ambiguous` or `Invalid` status on a
surviving key SHALL produce its local diagnostic in canonical member/constraint order and prevent a
binding. Otherwise a singleton intersection SHALL bind `S` and retain evidence for every wanted,
and a larger intersection SHALL produce one provider ambiguity with the complete canonical key
intersection, primary at the same responsible selector/application site and with every contributing
relation's canonical primary origin as an ordered secondary span. The structured ambiguity payload
SHALL contain the common surviving candidate list plus canonical relation records holding each
constraint key and its full pre-intersection candidate-key set. Complete ordered relation-origin
sets SHALL live only in the separate diagnostic-location record and MUST NOT participate in payload
identity, equality, or source/intrinsic parity comparisons.
Constraint-order and source-row permutations MUST produce the same substitution, evidence,
diagnostic multiplicity, identity, and canonical payload.
Conformance lookup SHALL be observational and MUST NOT emit diagnostics before selection; the
constraint solver alone SHALL diagnose statuses on surviving keys. After substitution and
normalization, all provider-constraint occurrences with one semantic constraint key SHALL form one
solved wanted while retaining a canonical source-origin set. This applies both to textual duplicates
and to distinct generic constraints whose keys become equal after specialization. Assumed evidence
from each grouped occurrence SHALL map to the one specialized wanted. A diagnostic for that wanted
SHALL be emitted once at its canonical first occurrence with remaining occurrences attached as
ordered secondary spans; occurrence identity SHALL NOT alter semantic evidence branding.
Provider-match evidence SHALL distinguish exact identity from conformance. A conformance witness
identity MUST be canonical, serialization-safe, and specialization-complete, distinguishing source
and intrinsic origins and retaining every generic argument required to identify a conditional
witness instance.

The sealed shared, exclusive, and owned binding operations SHALL each carry a canonical
effect-function contract with selected `?S` first, fixed provider parameter mode, whole row `?R`,
and `Without<R, S>` result. They SHALL use the same ordinary call path, checked constraint solver,
and capture analysis as equivalent Silk declarations. Generic HIR MAY carry assumed proof evidence;
the intrinsic post-contract hook MAY validate mode-appropriate place/move syntax and construct HIR,
but MUST NOT separately analyze call shape, infer access or roles, select candidates, subtract rows,
or construct the result Effect type.

`Effect.bindRequirement`, `Effect.bindRequirementMut`, `Effect.bindRequirementOwned`,
`Effect.provide`, and `Effect.provideMut` SHALL be ordinary fixed-mode Silk declarations. Borrowed
forms SHALL not imply provider ownership or cleanup. Moving an affine owned provider SHALL make the
result take-once; an owned Copy provider SHALL follow ordinary snapshot and repeatability rules.
`Effect.provideWith` SHALL acquire a fresh provider implementation `P` per execution, where `P` may
differ from selected capability `C`, bind it through an exclusive provider parameter, and drop every
successfully acquired owner after success or typed failure without replacing the original outcome.

#### Scenario: Provide one shared service

- **WHEN** an Effect with one shared service requirement is composed with `Effect.provide(&service)`
- **THEN** shared selection returns that stored requirement and the result borrows the provider, subtracts it, and preserves success, failure, and every remaining requirement

#### Scenario: Provide one exclusive service implementation

- **WHEN** an Effect requiring exclusive capability `C` is composed with `Effect.provideMut(&mut provider)` and the provider type uniquely conforms to `C`
- **THEN** exclusive selection returns the stored exclusive capability-role entry, subtracts it, and writes mutations back before the borrow ends

#### Scenario: Remove a shared requirement with an exclusive provider

- **WHEN** an Effect requires shared `&Logger` and an exclusive `StdoutLogger` provider is supplied
- **THEN** selection returns and subtracts the actual shared `&Logger` member rather than synthesizing `&mut Logger`

#### Scenario: Bind an affine owned provider

- **WHEN** an affine provider is moved through `Effect.bindRequirementOwned`
- **THEN** the selected requirement is removed and ordinary capture analysis makes the resulting Effect take-once

#### Scenario: Bind a Copy owned provider

- **WHEN** a Copy provider is passed through `Effect.bindRequirementOwned`
- **THEN** the selected requirement is removed and ordinary snapshot capture keeps the resulting Effect repeatable

#### Scenario: Select by conformance rather than row order

- **WHEN** an Effect requires `&mut Clock | &mut Logger` and `StdoutLogger` conforms only to `Logger`
- **THEN** binding selects `&mut Logger` and preserves `&mut Clock` regardless of source or canonical row order

#### Scenario: Reject an ambiguous provider selection

- **WHEN** one provider conforms to more than one compatible capability-role entry and `S` is not explicit
- **THEN** binding reports every candidate in canonical order and subtracts none

#### Scenario: Reject ambiguous conformance for one selected capability

- **WHEN** an exact selected requirement has more than one valid provider-conformance proof
- **THEN** binding reports conformance ambiguity rather than treating the provider as a no-match

#### Scenario: Keep same-role multi-capability selection ambiguous

- **WHEN** a provider conforms to `Clock@Primary` and `Logger@Primary` and both occur in the protected row
- **THEN** the shared role name does not choose between them and inferred selection remains ambiguous

#### Scenario: Select an explicit complete shared requirement

- **WHEN** `Intrinsic.bindRequirement<&Logger@Audit>(effect, &provider)` is called
- **THEN** the first generic argument fixes the exact stored access, capability, and role before provider conformance is validated

#### Scenario: Select an explicit complete exclusive requirement in a pipeline

- **WHEN** `effect |> Effect.provideMut<&mut Logger@Audit>(&mut provider)` is composed
- **THEN** the ordinary positional generic prefix fixes `S` while all later type and row binders remain inferred from supplied arguments and constraints

#### Scenario: Reject an invalid explicit selector cardinality

- **WHEN** an explicit selected requirement row is empty or contains `&mut Clock | &mut Logger`
- **THEN** binding reports selector-cardinality failure before provider matching in direct, wrapper, and pipeline forms

#### Scenario: Reject a multi-member provider selector inferred elsewhere

- **WHEN** another supplied parameter occurrence independently binds provider selector `S` to two requirement members before a custom declaration's `provides` constraint is solved
- **THEN** the same selector-cardinality validation fails before provider compatibility or conformance matching

#### Scenario: Keep provider evidence local to its solved source row

- **WHEN** two provider-selection obligations have the same selected member and provider but different protected source rows or provider modes
- **THEN** concrete evidence for one obligation cannot discharge the other because it is branded with the complete specialized wanted

#### Scenario: Intersect overlapping provider constraints

- **WHEN** two constraints sharing unbound `S` independently produce candidate sets `{Clock, Logger}` and `{Logger}`
- **THEN** their conjunction binds `S` to `Logger` and retains one branded selection proof for each constraint regardless of constraint order

#### Scenario: Do not truncate a shared selector candidate map

- **WHEN** two constraints sharing unbound `S` produce `{Clock, Logger, Metrics}` and `{Metrics}`, with `Metrics` ordered after the first two members
- **THEN** complete candidate-map intersection selects `Metrics` under every constraint and source-row permutation

#### Scenario: Reject conflicting provider constraints deterministically

- **WHEN** two constraints sharing unbound `S` independently produce nonempty disjoint candidate sets `{Clock}` and `{Logger}`
- **THEN** their conjunction reports joint-selection conflict with the same canonical payload under every constraint-order permutation

#### Scenario: Locate a multi-relation provider ambiguity deterministically

- **WHEN** two distinct constraints sharing inferred `S` both retain `{Clock, Logger}` after complete intersection
- **THEN** analysis emits one provider ambiguity primary at the complete application, attaches both canonical relation origins as ordered secondary spans, and includes both full relation key sets in its canonical payload

#### Scenario: Preserve unequal relation maps in an ambiguity payload

- **WHEN** shared-S relation maps `{Clock, Logger, Metrics}` and `{Clock, Logger, Audit}` intersect to ambiguous common candidates `{Clock, Logger}`
- **THEN** the semantic payload contains common surviving list `[Clock, Logger]` and two ordered relation records containing their distinct full candidate-key sets, while ordered origin sets remain separate diagnostic locations under every constraint and source-row permutation

#### Scenario: Ignore conformance ambiguity on an eliminated key

- **WHEN** the first relation maps `Clock` to ambiguous witnesses and `Logger` to one witness while the second relation contains only uniquely witnessed `Logger`
- **THEN** intersection eliminates `Clock`, binds `S` to `Logger`, and emits no conformance ambiguity under every constraint-order permutation

#### Scenario: Diagnose conformance ambiguity on a surviving key

- **WHEN** a shared-S intersection retains `Logger` and one relation maps that `Logger` key to ambiguous witnesses
- **THEN** analysis reports that relation's conformance ambiguity at its local member span and does not bind `S`

#### Scenario: Diagnose multiple empty provider relations

- **WHEN** more than one relation sharing `S` has an empty candidate map while another nonempty map contains ambiguous or invalid statuses
- **THEN** analysis emits one provider no-match for each empty relation in canonical constraint-key order, suppresses irrelevant statuses and joint conflict, and preserves each local relation span under source-order permutations

#### Scenario: Coalesce duplicate provider-constraint diagnostics

- **WHEN** two textually duplicated provider constraints have one semantic key and that relation has no candidate
- **THEN** analysis emits one provider no-match at the canonical first occurrence, attaches the other occurrence as a secondary span, and creates no duplicate semantic wanted or evidence

#### Scenario: Coalesce provider constraints that collide after specialization

- **WHEN** generic constraints over `P, R` and `Q, T` are textually distinct but substitution makes `P = Q` and `R = T`, producing one normalized semantic constraint key
- **THEN** specialization solves one relation and one concrete wanted/evidence value, unions both occurrence origins, and yields the same diagnostic multiplicity and spans under declaration and substitution permutations

#### Scenario: Keep eliminated conformance lookup observational

- **WHEN** declaration-index lookup returns ambiguous or invalid data for a candidate key later eliminated by shared-S intersection
- **THEN** the adapter emits nothing and the solver suppresses that status, so no diagnostic escapes from the eliminated lookup

#### Scenario: Distinguish identity and conformance provider matches

- **WHEN** one binding uses a provider exactly equal to its selected capability and another uses a distinct provider with one conformance witness
- **THEN** their branded evidence contains `ProviderMatch.Identity` and `ProviderMatch.Conformance(WitnessIdentity)` respectively, and both retain the correct match through witness reachability and service-slot specialization

#### Scenario: Distinguish conditional conformance witness specializations

- **WHEN** one generic witness declaration produces two valid provider matches with different specialized generic arguments
- **THEN** their canonical `WitnessIdentity` values differ by those complete argument lists and reach the correct conditional witness instances

#### Scenario: Distinguish source and intrinsic witness origins

- **WHEN** a source witness and an intrinsic witness otherwise have colliding local declaration identities and generic arguments
- **THEN** their canonical `WitnessIdentity` values remain distinct by origin kind

#### Scenario: Round-trip provider match evidence

- **WHEN** branded `RequirementSelection` evidence containing each `ProviderMatch` variant and a specialized conditional `WitnessIdentity` is encoded, decoded, and carried through a module surface
- **THEN** exact evidence identity, source/intrinsic origin, specialized generic arguments, wanted branding, and witness reachability are preserved

#### Scenario: Agree with ordinary wrapper binding

- **WHEN** the same Effect and provider are passed to a fixed-mode sealed binding operation and an ordinary Silk wrapper with an equivalent contract
- **THEN** both use the same call analysis, substitutions, proof evidence, run access, remainder, and diagnostic identity

#### Scenario: Preserve a constraint through a stored provider section

- **WHEN** `let provideLogger = Effect.provideMut<&mut Logger>(&mut logger)` is stored or passed through another function before receiving an Effect
- **THEN** its semantic callable value retains the quantified selection obligation and provider capture until static application

#### Scenario: Drop a stored provider section

- **WHEN** a stored provider section is never applied and is dropped
- **THEN** its nested quantified obligation is erased with the callable rather than rejected as an unsolved enclosing-instance obligation

#### Scenario: Acquire a conforming implementation

- **WHEN** `provideWith` protects an Effect requiring `Logger` and acquisition produces owned `StdoutLogger` with its own failures and requirements
- **THEN** exclusive conformance selects `Logger`, acquisition channels compose, and the acquired owner drops before the completed outcome escapes

#### Scenario: Catch outside per-run acquisition

- **WHEN** a failing Effect is wrapped by `provideWith` and then by executable `Effect.catchAll`
- **THEN** the per-run provider drops before recovery begins

### Requirement: Effect exposes three transformable channels

An Effect contract SHALL treat success `A` and typed failure `E` as covariant output channels and
its access-qualified requirement row `R` as a contravariant input channel. Call compatibility SHALL
retain those directions independently of equality and row normalization. Ordinary Silk library code
SHALL be able to transform either output, adapt an unknown requirement row through checked provider
selection followed by `Without`, or compose an effectful transformation while preserving every
untouched channel and the input Effect's run access. Constraints alone SHALL establish type
relationships only; removing a failure or requirement requires the corresponding handling or
binding operation.

#### Scenario: Transform every pure channel

- **WHEN** library code maps `Effect<A ! E ? R>` with `A -> B`, `E -> F`, and a typed requirement adapter from `R2` to `R`
- **THEN** it produces `Effect<B ! F ? R2>` using ordinary directional compatibility without inspecting a runtime row value or changing execution timing

#### Scenario: Remove one requirement from an unknown row

- **WHEN** a fixed-mode provider constraint selects singleton `S` from `Effect<A ! E ? R>` and the body binds that provider
- **THEN** ordinary Silk can declare the resulting contract as `Effect<A ! E ? Without<R, S>>`

#### Scenario: Normalize set-to-set requirement difference

- **WHEN** type analysis evaluates `Without<R, S>` after both normalized requirement rows are independently known
- **THEN** it removes every exact entry in `S` without implying that an Effect value performed the bindings

#### Scenario: Preserve requirement contravariance outside provision

- **WHEN** a non-provider generic combinator checks an Effect argument against a compatible requirement input row
- **THEN** call analysis uses requirement contravariance rather than invariant row equality

#### Scenario: Reject a requirement-row lying wrapper

- **WHEN** a generic function declares `&mut P provides S from R` and a `Without<R, S>` result but returns the protected Effect without executing a binding operation
- **THEN** body compatibility rejects the declaration because the constraint alone did not discharge `S`

#### Scenario: Reject a failure-row lying wrapper

- **WHEN** a generic function declares `S in E` and a `Without<E, S>` result but returns the protected Effect without executing a handling operation
- **THEN** body compatibility rejects the declaration because membership alone did not discharge `S`
