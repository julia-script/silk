# bootstrap-flow-functions Specification

## Purpose

Define Silk's lazy statically shaped flow values and its exact owned typed-failure channel, including
one-layer execution, propagation, recovery, and separation from unrecoverable traps.

## Requirements

### Requirement: Effect expressions and functions are lazy

Evaluating `effect { ... }` SHALL construct `Effect<A ! E ? R>` without entering its body.
Invoking an `effect fn` SHALL have the same behavior for its entire body. `run` SHALL evaluate exactly
one Effect layer. Ordinary `fn` statements outside an explicit effect block SHALL execute eagerly.

#### Scenario: Preserve an eager setup boundary

- **WHEN** an ordinary function computes one value and returns an effect block that uses it
- **THEN** the setup executes at the call while the block executes only when run

#### Scenario: Lift an ordinary value into Effect

- **WHEN** `Effect.of(value)` receives a Copy or explicitly transferred affine value
- **THEN** argument evaluation and transfer occur at construction, and running the returned `Effect<A>` succeeds with the captured value without failure or requirement channels

### Requirement: Effect construction has hidden nominal identity

Every `effect {}` construction site SHALL produce one compiler-only nominal Effect instance with a
target-planned capture environment and generated runner. The public structural Effect contract MUST
NOT expose that identity, and the implementation MUST NOT use a universal runtime interpreter or
erase different construction sites merely because their public contracts match.

#### Scenario: Return a delayed computation across a function boundary

- **WHEN** an ordinary function performs eager setup and returns `effect { ... }`
- **THEN** the returned Effect preserves its construction-site identity and captured environment until it is run or dropped

### Requirement: Effect failure channels contain ordinary types

For `Effect<A ! E ? R>`, `E` SHALL be an ordinary detached owned type or normalized structural
union. The `!` token SHALL label the channel only; it SHALL NOT create a distinct type kind, binder
form, or value wrapper. `never` SHALL denote the empty failure channel. A declared failure-row
member whose canonical type is a structural union SHALL contribute each union member to the row as
a separate member, so the row is identical whether the members are spelled directly, through a
parenthesized union, or through an alias. A nominal union SHALL remain one atomic member. `fail`
SHALL stop the current Effect execution with success type `never`, copying a Copy payload or
consuming an explicitly moved affine payload.

#### Scenario: Fail with a Copy problem

- **WHEN** an Effect executes `fail problem` for a Copy nominal value
- **THEN** the failure channel receives the copied value without requiring `fail move`

#### Scenario: Use the same failure union as a value

- **WHEN** `E` is `NotFoundError | OfflineError`
- **THEN** the Effect failure channel, a propagated failure value, and a handler parameter all use that same ordinary union without a value conversion

#### Scenario: A union alias flattens into the declared row

- **WHEN** `type FetchError = HttpError | JsonError` is declared and a function is declared `-> () ! FetchError`
- **THEN** its failure row has the two members `HttpError` and `JsonError`, and `Effect.catch<HttpError>` leaves the residual row `JsonError`

#### Scenario: Selecting through a union alias removes every member

- **WHEN** `Effect.catch<FetchError>` protects a row declared `! HttpError | JsonError | Timeout`
- **THEN** the residual row is `Timeout`, identical to `Effect.catch<HttpError | JsonError>`

#### Scenario: A nominal union stays atomic

- **WHEN** `union HttpError { NotFound, Timeout }` is a member of a failure row
- **THEN** `Effect.catch<HttpError>` removes the whole member and no selector can remove `NotFound` alone

#### Scenario: Reject a borrowed failure payload

- **WHEN** an Effect attempts to fail with a non-detached lexical borrow
- **THEN** analysis reports the ordinary ownership violation rather than a failure-kind diagnostic

### Requirement: Selective recovery subtracts ordinary unions

`Effect.catch(effect, handler)` and `effect |> Effect.catch(handler)` SHALL select one ordinary type
or union `S`, inferred from the handler or supplied as the first explicit argument in
`Effect.catch<S>`, require nonempty `S` to be wholly contained in protected failure type `E`, pass
`S` directly to the handler, and compute the semantic residual as `Without<E, S> | F`, where `F` is
the handler's failure type. Handler success `B` SHALL join protected success `A` as an ordinary
finite `A | B` union when needed. Success bypasses the handler and every nonmatching alternative
belongs to the computed remainder. Membership proves a type relationship but SHALL NOT erase a
failure without the handling operation.

The selected binder `S` SHALL be first so positional-prefix syntax `Effect.catch<ProblemError>`
remains an ordinary generic call. A generic declaration MAY retain open `S` with assumed membership
evidence. At each complete application, whether inferred or explicit, `S in E` SHALL require every
alternative of nonempty `S` to belong to `E` before lowering.
The whole-row `Effect.catch` alias SHALL be removed; whole-row recovery SHALL use
`Effect.catchAll`. Singleton `Effect.catch` SHALL be an ordinary wrapper over sealed
`Intrinsic.catchFailure`, whose callable contract carries the same checked membership and
`Without` result and which is executable on the evaluator, WebAssembly, and native targets.
Neither standard-library name recognition nor a separate intrinsic typing rule MAY determine the
residual.

A valid direct or wrapped singleton-catch application SHALL lower and execute on every supported
target. The protected Effect and handler operands SHALL be formed in ordinary call-evaluation order,
the protected Effect SHALL run exactly once, success SHALL bypass the handler, a selected failure
SHALL invoke the handler with its payload, and a nonselected failure SHALL propagate in the residual
row unchanged. Evaluator, WebAssembly, and native execution SHALL agree on results, failure tags,
and cleanup order. A syntax, kind, inference, or membership failure SHALL take precedence and prevent
MIR construction. This change adds no runtime type dictionary.

#### Scenario: Type selective catch through a pipeline

- **WHEN** `relay(0) |> Effect.catch<Problem>(recover)` is analyzed before target availability
- **THEN** the call contract gives `recover` the selected `Problem` payload and computes the protected remainder with `Without`

#### Scenario: Infer singleton catch from its handler

- **WHEN** `Effect.catch(effect, recoverProblem)` supplies a handler whose input is nominal `Problem`
- **THEN** ordinary call inference selects singleton `Problem`, checks `Problem in E`, and computes the same remainder as `Effect.catch<Problem>(effect, recoverProblem)`

#### Scenario: Reject an invalid catch selector

- **WHEN** explicit arguments or handler inference would select `never` or a type containing an alternative absent from `E`
- **THEN** the common checked-constraint solver reports an invalid failure selector before intrinsic availability and without constructing dispatch

#### Scenario: Recover one member with a fallback

- **WHEN** an `Effect<i32 ! NotFoundError | OfflineError>` catches `NotFoundError` with a `string` fallback
- **THEN** the result is `Effect<i32 | string ! OfflineError>`

#### Scenario: Re-fail an unhandled member

- **WHEN** a catch-all handler matches one failure member and fails again with the unmatched value
- **THEN** ordinary union narrowing preserves that unmatched member in the output failure channel

#### Scenario: Catch the whole ordinary failure value

- **WHEN** `Effect.catchAll` protects `Effect<A ! E>`
- **THEN** its handler accepts `E` directly, without `Row<!E>` or another reification wrapper

#### Scenario: Reject an invalid selected subset

- **WHEN** `S` is `never` or contains an alternative absent from protected failure type `E`
- **THEN** ordinary type constraints reject the catch before lowering

#### Scenario: Preserve an open failure remainder

- **WHEN** a generic catch selects nominal `Problem` from open row `E` under checked membership
- **THEN** its declared failure row remains `Without<E, Problem> | F` until specialization and preserves every nonmatching failure

#### Scenario: Reject absent selected failure

- **WHEN** `Effect.catch<Problem>` protects a concrete failure row that does not contain `Problem`
- **THEN** analysis reports the failed membership constraint at the call before any runtime dispatch is constructed

#### Scenario: Execute a selected failure on every target

- **WHEN** an executable `Effect.catch<Problem>` protects an Effect that fails with `Problem`
- **THEN** the evaluator, WebAssembly, and native targets invoke the handler once with the selected payload and produce the same result

#### Scenario: Bypass the handler on success

- **WHEN** the protected Effect succeeds
- **THEN** selective catch returns that success without invoking the handler

#### Scenario: Propagate a nonselected failure

- **WHEN** the protected Effect fails with a member other than the selected nominal type
- **THEN** the payload propagates under the corresponding residual-row tag without invoking the handler

#### Scenario: Execute the sealed primitive directly

- **WHEN** a valid `Intrinsic.catchFailure<Problem>` call is executable-reachable
- **THEN** it uses the same specialized dispatch and produces the same outcome as the ordinary `Effect.catch<Problem>` wrapper

#### Scenario: Preserve behavior through a nested wrapper

- **WHEN** a user-defined wrapper chain reaches singleton catch and the outer wrapper is called from one reachable source application
- **THEN** specialization and lowering preserve the selected member, residual row, and handler behavior through the complete wrapper chain

#### Scenario: Share one specialized implementation safely

- **WHEN** two reachable source applications specialize to one deduplicated wrapper instance containing singleton catch
- **THEN** both call sites execute through the deduplicated concrete instance without conflating their runtime payloads or outcomes

#### Scenario: Recover the whole ordinary failure value

- **WHEN** `Effect.catchAll` protects `Effect<A ! E>`
- **THEN** its handler accepts ordinary `E` directly and the protected failure type is removed in full

#### Scenario: Select a failure union

- **WHEN** a caller catches `FirstError | ThirdError` from a larger ordinary failure union
- **THEN** both selected alternatives invoke the handler and every unselected alternative propagates unchanged

#### Scenario: Recover through a pipeline

- **WHEN** `relay(0) |> Effect.catch<Problem>(recover)` fails with `Problem`
- **THEN** `recover` owns the payload and its success becomes the pipeline result

### Requirement: Capture access derives repeatability

Copy captures SHALL snapshot at construction, shared captures SHALL permit repeated shared runs,
exclusive captures SHALL require exclusive runs while preserving mutations across runs, and an
Effect whose execution consumes a captured affine owner SHALL be take-once.

#### Scenario: Reject a second consuming run

- **WHEN** one execution consumes a moved capture and the caller runs the same Effect again
- **THEN** ownership rejects the second run and identifies the consumed capture

### Requirement: Retry accepts only repeatable Effects

`Effect.retry` SHALL reconstruct execution-local state for every attempt while reusing captures. It
MUST reject a take-once Effect. Providers acquired inside the retried Effect SHALL be reacquired;
captured providers SHALL be reused.

#### Scenario: Preserve mutable retry state

- **WHEN** a repeatable Effect mutates an exclusive captured counter and is retried
- **THEN** each attempt receives fresh locals while observing the counter changes from earlier attempts

### Requirement: Provision distinguishes shared borrow, exclusive borrow, and acquisition

Requirement binding SHALL relate whole protected row `R`, exact selected service-role key `S`, and one
of three source-spellable provider parameter modes:

- shared `&P provides S from R`;
- exclusive `&mut P provides S from R`;
- owned `P provides S from R`.

Shared providers SHALL satisfy only shared stored requirements. Exclusive and owned providers MAY
satisfy shared or exclusive stored requirements. Selection MUST resolve the service-role key before
validating its stored access demand. For every mode, `P` MUST equal the selected capability or have one unique,
valid service-conformance witness.

When `S` is unbound, every compatible service-role key SHALL be considered and exactly one
candidate MUST remain. Whenever `S` is independently bound—by an explicit row generic argument or
by another supplied-argument occurrence—the constraint SHALL first require it to normalize to
exactly one member, then validate that member and its provider match (identity or one unique
conformance witness). Empty or multi-member
bound selectors SHALL report selector-cardinality failure before provider matching. Explicit
disambiguation SHALL name
the complete service-role key as `Service` or `Service at Role`; access SHALL NOT appear in the
selector, there SHALL be no role-only intrinsic filter, and an
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
`Effect.provideEffect` SHALL acquire a fresh provider implementation `P` per execution, where `P` may
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

- **WHEN** a provider conforms to `Clock at Primary` and `Logger at Primary` and both occur in the protected row
- **THEN** the shared role name does not choose between them and inferred selection remains ambiguous

#### Scenario: Select an explicit shared requirement key

- **WHEN** `Intrinsic.bindRequirement<Logger at Audit>(effect, &provider)` is called
- **THEN** the first generic argument fixes the exact service-role key before provider conformance and shared access are validated

#### Scenario: Select an explicit exclusive requirement key in a pipeline

- **WHEN** `effect |> Effect.provideMut<Logger at Audit>(&mut provider)` is composed
- **THEN** the ordinary positional generic prefix fixes `S` while the helper validates exclusive access and all later type and row binders remain inferred from supplied arguments and constraints

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

- **WHEN** `let provideLogger = Effect.provideMut<Logger>(&mut logger)` is stored or passed through another function before receiving an Effect
- **THEN** its semantic callable value retains the quantified selection obligation and provider capture until static application

#### Scenario: Drop a stored provider section

- **WHEN** a stored provider section is never applied and is dropped
- **THEN** its nested quantified obligation is erased with the callable rather than rejected as an unsolved enclosing-instance obligation

#### Scenario: Acquire a conforming implementation

- **WHEN** `provideEffect` protects an Effect requiring `Logger` and acquisition produces owned `StdoutLogger` with its own failures and requirements
- **THEN** exclusive conformance selects `Logger`, acquisition channels compose, and the acquired owner drops before the completed outcome escapes

#### Scenario: Catch outside per-run acquisition

- **WHEN** a failing Effect is wrapped by `provideEffect` and then by executable `Effect.catchAll`
- **THEN** the per-run provider drops before recovery begins

#### Scenario: Provide one shared service

- **WHEN** an Effect with one shared service requirement is composed with `Effect.provide(&service)`
- **THEN** the resulting Effect borrows that provider, removes the selected requirement, and preserves its success, failure, and remaining requirement channels

#### Scenario: Provide one exclusive service implementation

- **WHEN** an Effect requiring exclusive access to capability `C` is composed with `Effect.provideMut(&mut provider)` and the provider type conforms to `C`
- **THEN** the resulting Effect borrows the provider exclusively, removes that capability-role requirement, and writes provider mutations back before the borrow ends

#### Scenario: Catch outside per-run acquisition

- **WHEN** a failing Effect is wrapped by `provideEffect` and then by `Effect.catch`
- **THEN** the per-run provider drops before recovery begins

### Requirement: Traps remain outside Effect failure and cleanup

Bounds violations, arithmetic traps, impossible compiler states, and violated unsafe contracts
SHALL remain abnormal termination. `Effect.catch` MUST NOT intercept them, and bootstrap MUST NOT
promise Drop unwinding after a trap.

#### Scenario: Trap bypasses catch and cleanup claims

- **WHEN** a protected Effect divides by zero
- **THEN** execution traps without invoking the typed handler or reporting structured cleanup completion

### Requirement: Effect combinators accept ordinary callable values

`Effect.map`, `flatMap`, `tap`, `catch`, and other higher-order Effect operations SHALL accept
ordinary callable values and automatic sections under explicit callable contracts. Their direct
data-first forms and piped section forms SHALL be semantically identical. `map` SHALL preserve a
returned Effect as a nested success value, while `flatMap` and the Effect-specific behavior of
`tap` SHALL compose execution according to their declared contracts.

#### Scenario: Map with an arithmetic section

- **WHEN** `succeed(2) |> Effect.map(i32.add(2))` is run
- **THEN** the section maps the success to `4` without pipeline-specific callback syntax

#### Scenario: Keep effectful logging out of map

- **WHEN** an effectful logging function is passed to `Effect.map`
- **THEN** its Effect result remains nested rather than being executed implicitly

### Requirement: Callable captures derive composed Effect access

An Effect combinator that stores a callback SHALL incorporate the callback environment's shared,
exclusive, or consuming access into the resulting Effect's run access. Retry MUST reject a composed
Effect whose callback or input Effect is take-once. Dropping the composed Effect without running it
SHALL release the stored callback exactly once.

#### Scenario: Make map take-once

- **WHEN** `Effect.map` captures a mapper that consumes one owned capture
- **THEN** the mapped Effect is take-once and a second run is rejected before invoking the mapper

#### Scenario: Preserve exclusive callback state

- **WHEN** a mapped Effect uses a `mut fn` callback across repeated runs
- **THEN** each run requires exclusive Effect access and observes the callback's retained mutations

### Requirement: Logging remains effectful

Semantic logging SHALL remain an Effect operation with its declared Logger requirement. A log call
SHALL dispatch one complete semantic message in one Logger invocation rather than expose an API for
incrementally appending byte fragments,
so native, in-memory, browser, and telemetry providers can implement the same contract. Ordinary
functions that add logging MUST return or execute an Effect through the existing effect model; this
requirement MUST NOT introduce an eager non-effect trace, debugging intrinsic, or stdout shortcut.

#### Scenario: Propagate a temporary semantic log honestly

- **WHEN** a previously eager computation adds a Logger operation
- **THEN** its Effect and Logger requirements propagate to the execution boundary rather than bypassing the type system

#### Scenario: Compose logging through an Effect pipeline

- **WHEN** `Effect.log` is sequenced through `flatMap`, `tap`, recovery, or service provision
- **THEN** its event executes at the composed position and its Logger and failure channels remain explicit

### Requirement: Effect logging is ordinary source-defined API

`Effect.log` and its level-selecting sibling SHALL resolve to canonical ordinary Silk declarations.
The compiler MUST NOT select logging behavior from their names, actor, or standard-library origin.
Equivalent user code invoking the Logger service SHALL receive the same typing, ownership,
execution, and cleanup behavior.

#### Scenario: Navigate to Effect.log

- **WHEN** a program calls or navigates to `Effect.log`
- **THEN** the target is canonical shipped Silk source compiled through ordinary requirement and Effect composition paths

### Requirement: Effect recipes compose uniformly

Every semantically valid nesting of Effect construction, transformation, recovery, retry, and
service provision SHALL retain the same contract and execution behavior in data-first calls,
left-associated pipelines, explicitly grouped expressions, and stored intermediate values. `run`
SHALL execute exactly the composed outer Effect regardless of source shape. Construction-time
callable and provider evaluation, run-time operation order, failure and requirement rows, capture
access, and cleanup MUST remain equivalent across those forms.

#### Scenario: Map a provided Effect directly from an effectful entry

- **WHEN** an effectful `main` runs `source |> Capability.provide(provider) |> Effect.map(mapper)`
- **THEN** the provider satisfies the source requirement, the mapper receives the success once, and the entry completes with the mapped result

#### Scenario: Reverse the transformation and provision order

- **WHEN** a requirement-preserving transformation is applied before the required provider is supplied
- **THEN** provision satisfies the transformed Effect's requirement and execution agrees with the equivalent provision-first form

#### Scenario: Store a composed pipeline before running it

- **WHEN** a valid multi-combinator Effect pipeline is bound and run later
- **THEN** it behaves like the direct expression while preserving construction-time captures and without introducing a trap

#### Scenario: Preserve affine success through a mapped provided Effect

- **WHEN** a provided Effect succeeds with an affine value that a mapper consumes
- **THEN** the mapper receives ownership exactly once and every remaining owned component is cleaned exactly once

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

#### Scenario: Transform every pure channel

- **WHEN** library code maps `Effect<A ! E ? R>` with `A -> B`, `E -> F`, and a typed requirement adapter from `R2` to `R`
- **THEN** it produces `Effect<B ! F ? R2>` without inspecting a runtime row value or changing execution timing

#### Scenario: Remove one requirement from an unknown remainder

- **WHEN** a provider satisfies one capability-role entry in `Effect<A ! E ? Capability | Rest>`
- **THEN** the resulting Effect has contract `Effect<A ! E ? Rest>` for any normalized `Rest`

### Requirement: Completed Effect outcomes can be reified compositionally

Canonical ordinary Silk `Effect.result` SHALL execute exactly one Effect layer and reify its
completed typed outcome as direct ordinary nominal `Result<A, E>` data instead of propagating `E`.
It SHALL construct `Result<A, E>.Success` or `Result<A, E>.Failure` without a wrapper field,
detached member, or intermediate structural union. Its implementation SHALL first map the protected
success through an ordinary exact `once fn(A) -> Result<A, E>` constructor, then apply the general
`Effect.catchAll` operation with an ordinary exact `once fn(E) -> Result<A, E>` handler for the
complete typed failure value. It MUST NOT use a completed-outcome intrinsic, and the compiler MUST
NOT recognize `Result`, its module, or either variant by spelling. The composition SHALL preserve `R`,
ownership, cleanup, run access, and lazy timing, and its contract SHALL remain valid if execution can
suspend before producing the Result in a future runtime. Traps and future interruption MUST NOT be
converted into typed `E` values.

#### Scenario: Map both completed branches in library code

- **WHEN** ordinary Silk code reifies `Effect<A ! E ? R>` and matches its Result with success and failure callbacks
- **THEN** either callback can produce the corresponding transformed channel while `R` remains required

#### Scenario: Preserve future suspension transparency

- **WHEN** a future execution suspends before its typed outcome completes
- **THEN** outcome reification waits compositionally and does not expose a pending state as `Result<A, E>`

#### Scenario: Reify directly into the nominal Result

- **WHEN** an Effect completes once with success and once with a typed failure
- **THEN** ordinary source constructs the corresponding direct `Success` and `Failure` variants and every downstream phase observes one nominal Result layer

#### Scenario: Rename an equivalent source wrapper

- **WHEN** equivalent ordinary source maps success and catches failure into a user-defined result-like nominal union under another legal function name
- **THEN** it can construct and return a user-selected nominal union without compiler registration of that union or its variants

#### Scenario: Reify a compound failure union

- **WHEN** the protected Effect can fail with `HttpError | OutOfMemoryError`
- **THEN** `catchAll` passes that complete structural union to the Failure constructor without flattening the outer Result or losing either error alternative

#### Scenario: Preserve affine branch values

- **WHEN** either completed branch carries a move-only value and its constructor is an exact `once fn`
- **THEN** ordinary `map` and `catchAll` transfer that value exactly once and clean only the unselected callable environment

### Requirement: Standard Effect combinators are library-defined

`map`, `mapError`, `mapBoth`, `flatMap`, `tap`, `catch`, `retry`, `ensuring`, `ifThenElse`, `zip`, `zip3`,
`provide`, `provideMut`, and `provideEffect` SHALL resolve to canonical ordinary Silk declarations. The
compiler MUST NOT select their semantics from their names, actors, library origin, or a dedicated
combinator HIR/MIR operation. Equivalent user code using the compiler-owned Effect core SHALL
receive the same typing, ownership, execution, and cleanup behavior.

#### Scenario: Navigate and compile map as Silk

- **WHEN** a program calls or navigates to `Effect.map`
- **THEN** the target is canonical shipped Silk source compiled through ordinary declaration, callable, ownership, specialization, and lowering paths

#### Scenario: Navigate and compile exclusive provision as Silk

- **WHEN** a program calls either the data-first or piped form of `Effect.provideMut`
- **THEN** the target is canonical shipped Silk source and both forms produce the same contract and execution behavior

#### Scenario: Navigate and compile finalization as Silk

- **WHEN** a program calls or navigates to `Effect.ensuring`
- **THEN** the target is canonical shipped Silk source built from outcome reification and re-raise, with no combinator-specific compiler operation

#### Scenario: Navigate and compile sequential collection as Silk

- **WHEN** a program calls or navigates to `Effect.zip` or `Effect.zip3`, in either the data-first or the piped form
- **THEN** the target is canonical shipped Silk source built from ordinary `run` statements, with no combinator-specific compiler operation and no intrinsic

#### Scenario: Define an equivalent user combinator

- **WHEN** user source defines the same generic success-channel transformation under another name
- **THEN** it compiles and executes without intrinsic registration or a compiler-recognized operation identity

#### Scenario: Navigate and compile the conditional as Silk

- **WHEN** a program calls or navigates to `Effect.ifThenElse`
- **THEN** the target is canonical shipped Silk source built from an ordinary `if` statement over suspended callable arms, with no combinator-specific compiler operation

### Requirement: Synchronous Effects retain a suspension-compatible abstraction

The public Effect contract SHALL NOT expose a concrete callback ABI, scheduler object, coroutine
frame, runtime requirement record, execution-stack allocator, or complete-or-suspended
representation. A closed Effect call graph that cannot reach the suspension intrinsic MUST NOT
contain coroutine-frame transformation, scheduler or fiber linkage, atomic synchronization, a
mandatory complete-versus-pending branch, or a private suspension dispatcher. Existing
source-defined combinators SHALL compose with suspendable Effects without changing their contracts
or recognizing a private pending state.

#### Scenario: Run a closed synchronous pipeline

- **WHEN** a closed Effect call graph cannot reach suspension, fork, interruption, or a fiber observation
- **THEN** execution retains its direct synchronous entry and call shape and links no coroutine or concurrency runtime solely because it uses library Effect combinators

#### Scenario: Preserve the runner seam under suspension

- **WHEN** a source-defined combinator runs an Effect whose reachable call graph can suspend
- **THEN** the compiler-owned execution boundary resumes the composition without changing the combinator's public signature or exposing a pending state

### Requirement: Effects use source-defined services generically

An Effect requirement MAY name any visible source-declared service and role. Requirement
normalization, service-slot shaping, witness dispatch, and `Effect.provide`, `Effect.provideMut`,
and acquisition-based provision SHALL operate from declaration and conformance facts rather than a
compiler-known capability list. `Effect.result` and requirement binding MAY remain source wrappers
over minimal `Intrinsic` machinery.

#### Scenario: Compose an arbitrary service requirement

- **WHEN** an Effect calling a user-declared service is mapped, tapped, stored, and provided
- **THEN** every combinator preserves or discharges the service requirement by the ordinary row rules

#### Scenario: Avoid a service-specific Effect intrinsic

- **WHEN** Logger or FileSystem is added after this change
- **THEN** no new Effect intrinsic, compiler recipe kind, or name-based lowering rule is required

### Requirement: Effect suspension is explicit lazy composition

The canonical ordinary Silk function
`Effect.suspend<A, E, ?R>(deferred: once Effect<A ! E ? R>)` SHALL defer execution of `deferred`
until the returned Effect is run and SHALL transfer its execution through the explicit stack-safe
boundary. Its result contract SHALL be exactly `A ! E ? R`: suspension MUST NOT add an allocation
failure or allocator requirement. Each concrete suspendable invocation SHALL reuse one statically
shaped coroutine frame across its possible suspension states. Dynamic execution-stack exhaustion
SHALL be a fatal trap outside the typed failure channel. The compiler MUST NOT recognize the public
function by actor, module, or operation spelling.

#### Scenario: Keep suspension lazy

- **WHEN** an Effect with observable work is passed to `Effect.suspend` and the returned Effect is not run
- **THEN** the deferred work does not execute and dropping the returned Effect releases its captures exactly once

#### Scenario: Preserve the child channels

- **WHEN** `Effect.suspend` receives `Effect<A ! E ? R>`
- **THEN** the returned Effect has exactly `A ! E ? R` with no `OutOfMemoryError` member and no `Allocator` requirement introduced by suspension

#### Scenario: Preserve a nested Effect success value

- **WHEN** the deferred child succeeds with `Effect<i32>` as its declared success value
- **THEN** one run of `Effect.suspend` produces that nested `Effect<i32>` value without flattening or running it

#### Scenario: Exhaust private execution storage

- **WHEN** compiled suspended recursion exhausts its finite compiler-owned execution stack
- **THEN** execution traps without producing a typed failure or permitting `Effect.catch` to recover the exhaustion

#### Scenario: Do not interpret suspension as parking

- **WHEN** a running Effect reaches `Effect.suspend`
- **THEN** it transfers synchronous execution of its deferred child without creating a task, parking for a wakeup, yielding scheduler fairness, or adding interruption and cancellation semantics

### Requirement: Explicit suspension covers recursive cycles, not recursive declarations

A terminating self-recursive or mutually recursive Effect graph SHALL use bounded native and Wasm
machine stack when every possible recursive cycle crosses an explicit suspension origin. A
suspension origin on an unrelated or avoidable branch SHALL NOT cover a cycle. Recursive functions
and Effects without a covered cycle SHALL remain valid Silk and MUST NOT receive a mandatory
compiler diagnostic solely because their depth is unbounded.

#### Scenario: Cover mutual recursion with one suspension edge

- **WHEN** every path around a mutually recursive Effect cycle crosses one explicit `Effect.suspend` edge
- **THEN** terminating execution uses bounded native and Wasm machine stack even though the other recursive edges do not suspend

#### Scenario: Leave an uncovered cycle valid

- **WHEN** a recursive Effect cycle can execute without crossing any suspension origin
- **THEN** the compiler accepts the otherwise valid program without promising bounded machine stack

#### Scenario: Ignore suspension on an unrelated branch

- **WHEN** a recursive cycle can avoid a branch containing `Effect.suspend`
- **THEN** that branch does not establish the bounded-machine-stack guarantee for the cycle

### Requirement: Suspension imposes no allocator implementation restriction

An ordinary implementation of the `Allocator` service SHALL be permitted to suspend whenever its
declared Effect contract permits suspension. The compiler MUST NOT apply a suspension-specific
bootstrap, recursion, conformance, or self-hosting restriction to that implementation.

#### Scenario: Suspend inside an allocator operation

- **WHEN** an `Allocator` implementation satisfies its ordinary service contract and one operation reaches `Effect.suspend`
- **THEN** it is checked like any other service implementation and receives no suspension-specific diagnostic

### Requirement: Source-defined Effect combinators compose across suspension

`Effect.map`, `Effect.flatMap`, outcome reification, recovery, retry, provision, and equivalent user
combinators SHALL compose with a suspended Effect through their existing ordinary Silk definitions
and public signatures. They MUST NOT inspect or expose a pending state, coroutine frame, driver
token, or private runner ABI. Suspension SHALL preserve the child's failure and requirement rows
exactly; combinators SHALL compose only the rows contributed by their ordinary inputs and callbacks.

#### Scenario: Map after suspension

- **WHEN** a suspended Effect succeeds and its result is transformed with `Effect.map`
- **THEN** the mapper runs once after resumption and receives the original success value without adding a suspension-specific failure or requirement

#### Scenario: Flat-map into suspension

- **WHEN** `Effect.flatMap` selects a suspended Effect from an input success
- **THEN** execution waits for the suspended child and preserves the ordinarily unioned failure and requirement rows without exposing a pending representation or adding storage channels

### Requirement: Every executable body satisfies its resolved return contract before lowering

Semantic analysis SHALL prove that every reachable explicit return and fallthrough path of an ordinary function, Effect function, generic declaration, and conformance operation is compatible with the declaration's resolved return type. A reachable fallthrough SHALL produce `()` and therefore SHALL be accepted only for a unit result. An `Effect<A>` value SHALL NOT satisfy an `A` return merely because the surrounding function is effectful.

#### Scenario: Reject a nested Effect at its return

- **WHEN** a body declared to return `i32` returns a call whose value is `Effect<i32>`
- **THEN** analysis reports a return-type mismatch at that expression and constructs no executable HIR or MIR body for the declaration

#### Scenario: Accept an explicitly nested Effect

- **WHEN** a body declared to return `Effect<i32>` returns a call whose value is `Effect<i32>`
- **THEN** analysis accepts the return without running or flattening the value

#### Scenario: Accept terminal branches without a trailing return

- **WHEN** every reachable branch of a non-unit body ends in a compatible return or another terminal operation
- **THEN** analysis accepts the body without requiring a syntactically trailing return

#### Scenario: Reject reachable non-unit fallthrough

- **WHEN** a reachable path reaches the closing brace of a body declared to return `i32`
- **THEN** analysis reports a missing return at that fallthrough boundary

### Requirement: Invalid reachable bodies stop at the semantic boundary

A declaration with an unresolved or invalid executable body SHALL be unavailable to reachability and lowering. Calls through an interface witness SHALL preserve that same validity requirement rather than substituting an invalid mapped body into MIR.

#### Scenario: Reject issue 226 before the backend

- **WHEN** an interface-dispatched operation implementation violates its resolved return contract
- **THEN** the compiler emits the source semantic diagnostic and neither MIR verification nor a backend reports the primary failure

### Requirement: Requirement identity is keyed independently from access

An Effect requirement SHALL be identified by its canonical service identity plus optional `at` role. Shared, exclusive, and acquired access SHALL be checked as provider compatibility and SHALL NOT create different requirement keys. Requirement union, subtraction, and diagnostics SHALL be deterministic.

#### Scenario: Distinguish two clocks by role

- **WHEN** one Effect requires `Clock at source` and `Clock at destination`
- **THEN** the row contains two keys and each provision discharges only the selected role

#### Scenario: Reject insufficient provider access

- **WHEN** an exclusive requirement key and conformance match but the provider offers only shared access
- **THEN** provision reports `SEM0131` without changing the requirement's identity or treating the key as absent

#### Scenario: Select one key before checking access

- **WHEN** an explicit `Clock at Primary` selector names one row key
- **THEN** provision resolves that key and its conformance before validating the helper's provider access mode

### Requirement: Provision helpers discharge exact keys

`provide`, `provideMut`, acquisition provision, and `provideEffect` SHALL discharge only their exact selected keys and preserve all unrelated failures and requirements. `provideWith` SHALL NOT remain as an alias. `Effect.flatten` SHALL union the requirements of both layers before provision.

#### Scenario: Flatten a repeated requirement

- **WHEN** `Effect.flatten` receives `Effect<Effect<i32 ? &Clock> ? &Clock>`
- **THEN** the result is `Effect<i32 ? &Clock>` with one normalized key rather than two runtime slots

#### Scenario: Build a provider effectfully

- **WHEN** `provideEffect` obtains a provider from an Effect with its own failure and requirements
- **THEN** those channels compose normally while the selected provided key is removed from the protected Effect

### Requirement: Finite compatible Effects join without construction identity

A finite control-flow join SHALL admit Effect values whose success, failure, requirement,
capture-access, and ownership contracts have a valid common result, even when the Effects were
constructed at different source sites. The join SHALL preserve laziness and SHALL NOT allocate or
erase the concrete alternatives.

#### Scenario: Join two lazy branch Effects

- **WHEN** an `if` selects between independently constructed `Effect<i32 ! never>` values
- **THEN** the expression has one usable Effect type and only the selected branch runs

#### Scenario: Join compatible channels

- **WHEN** two branch Effects contribute distinct ordinary failure members and requirement keys
- **THEN** the joined Effect carries their normalized unions and preserves the selected branch's exact outcome

### Requirement: Composite Effect realization is finite and deterministic

HIR and MIR SHALL represent the admitted alternatives as a closed finite composite whose evaluator,
LLVM, and Wasm realizations select one alternative without heap allocation. A join with no finite
compatible representation SHALL retain a source diagnostic.

#### Scenario: Compare all engines

- **WHEN** equivalent joined Effects are evaluated and compiled repeatedly
- **THEN** all engines produce the same typed outcome, ownership cleanup, and deterministic artifact identity

### Requirement: Effect-block result typing accounts for every terminal

An effect block's success and failure types SHALL be derived from every `return` and `fail` terminal reachable in the block, including terminals nested inside `unsafe` blocks. Return sites with differing types SHALL combine through the language's canonical result join — never by silently adopting one site's type: joinable types form their union, and a join with no representable form is reported as a diagnostic at the offending return. A `fail` whose failure type is a value-kind type parameter SHALL contribute that parameter to the block's failure row exactly as a nominal failure would.

#### Scenario: Terminals inside unsafe blocks are collected

- **WHEN** an effect block's only `fail` (or only `return`) sits inside an `unsafe { }` statement
- **THEN** the block's failure row (or success type) includes it, and running the effect requires handling the failure

#### Scenario: Disagreeing branch returns cannot pass silently

- **WHEN** an effect block returns `bool` on one branch and `i32` on another inside a context expecting `Effect<i32>`
- **THEN** the block types as the canonical join (`Effect<bool | i32>`) and the context rejects it with a type-mismatch diagnostic — the block is never typed from the lexically last return alone

#### Scenario: Generic failures survive into the failure row

- **WHEN** a generic function's effect block fails with a value of type parameter `E`
- **THEN** the block types as an effect whose failure row contains `E`, and after specialization the concrete failure must be handled at `run`

### Requirement: Effect-block captures include enum-value arguments

Capture analysis for effect blocks SHALL register a capture for every binding referenced anywhere in the block body, including bindings referenced as the argument of an enum value construction.

#### Scenario: Enum.value argument is captured

- **WHEN** an effect block's body evaluates `Color.value(c)` for an outer binding `c`
- **THEN** `c` appears in the effect's capture environment and the deferred runner reads the captured value

### Requirement: Effects can be collected sequentially at a fixed arity

`Effect.zip` and `Effect.zip3` SHALL run their operands in declaration order and collect every
success value into ordinary public data — `Pair<A, B>` and `Triple<A, B, C>` respectively — whose
fields are readable from any module that can see the type.

Execution SHALL stop at the first typed failure. An operand that follows a failed one MUST NOT run,
and it MUST be released by the ordinary local cleanup of the frame the failure propagates out of, so
no unrun operand is stranded.

The result SHALL carry the union of every operand's failure row and the union of every operand's
requirement row, and MUST NOT add a failure or requirement of its own. Collecting the values MUST
NOT allocate.

Arity SHALL be extended by adding a parameter rather than by accepting a collection. Each operand is
a distinct parameter, so no Effect value is stored in runtime-indexed storage and every one of them
stays inside the hidden-identity specialization that erases it before lowering. Bootstrap MUST NOT
promise a combinator that takes a runtime-sized collection of Effects; that requires Effect values
to have a storable target layout, which they do not have.

Both combinators SHALL be ordinary Silk declarations with no intrinsic, no dedicated HIR or MIR
operation, and no compiler-side name recognition.

#### Scenario: Collect two success values in order

- **WHEN** two Effects that both succeed are combined with `Effect.zip`
- **THEN** the first Effect runs before the second and the returned pair carries both success values in that order

#### Scenario: Stop at a first-operand failure

- **WHEN** the first operand of `Effect.zip` fails with a typed failure
- **THEN** the second operand never runs, that same failure reaches the caller with its payload intact, and the unrun second operand is released exactly once

#### Scenario: Propagate a later operand's failure

- **WHEN** the second operand of `Effect.zip` fails after the first has succeeded
- **THEN** the failure reaches the caller unchanged and no pair is constructed

#### Scenario: Collect three success values in order

- **WHEN** three Effects are combined with `Effect.zip3` and the middle one fails
- **THEN** the first operand has already run, the third operand never runs, and the middle operand's failure reaches the caller

#### Scenario: Union every operand's rows

- **WHEN** operands with distinct failure rows and distinct requirement rows are combined
- **THEN** the resulting Effect's failure row is the union of theirs and its requirement row is the union of theirs, with nothing added

#### Scenario: Read the collected values from another module

- **WHEN** a caller in another module projects `first` and `second` from the returned pair
- **THEN** the projection is accepted, because the fields are public

### Requirement: A conditional combinator selects one suspended branch and never builds the other

`Effect.ifThenElse` SHALL take a `bool` condition and two suspended arms, each a
`once fn() -> Effect<...>`, and SHALL invoke exactly the arm the condition selects. It MUST NOT
invoke the other arm.

Because an arm produces its branch rather than being one, the branch not taken SHALL never be
constructed. This is stronger than the branch's effect not being performed: construction-time work
inside an unselected arm SHALL NOT happen, and an arm whose body is only well-defined under the
condition SHALL be safe to write. A form taking two pre-built `Effect` values would not satisfy
this, because both branches would be evaluated at the call site before either was chosen.

The arm that is not invoked SHALL be released exactly once. No arm can own a resource: a zero-arity
callable is either a named function, which has no environment, or a section, and section
construction supplies "exactly parameters one through the last" to produce "a unary callable
awaiting parameter zero", so it always leaves arity 1 and never 0. A capturing value SHALL
therefore be rejected against an arm's declared contract rather than accepted and leaked.

The result's failure row SHALL be the union of the two arms' failure rows and its requirement row
SHALL be the union of theirs, so a caller discharges whatever either branch could need without
knowing which is selected. Both arms SHALL agree on the success type.

The combinator SHALL be named `ifThenElse`. `if` is lexed unconditionally as a keyword and Silk has
no raw-identifier form, so an `effect fn` named `if` cannot be declared at all — this is a
constraint on the declaration, not one a qualified call spelling could avoid.

#### Scenario: Perform none of the unselected branch's effects

- **WHEN** two arms call a counting service a different number of times and `Effect.ifThenElse` selects one of them
- **THEN** only the selected arm's service calls are observed, in either polarity

#### Scenario: Never construct the unselected branch

- **WHEN** the arms are ordinary functions that perform observable work at invocation before returning their Effects
- **THEN** only the selected arm's construction-time work happens, in either polarity

#### Scenario: Reject an arm that owns a resource

- **WHEN** a value holding an owned resource is supplied where a zero-arity arm is required
- **THEN** it is rejected, because a zero-arity arm has no environment to hold it and therefore nothing to leak

#### Scenario: Union both arms' rows

- **WHEN** the two arms declare different typed failures and different service requirements
- **THEN** the result carries the union of both failure rows and both requirement rows, and either branch's selection is satisfied by discharging that union

#### Scenario: Agree across engines

- **WHEN** a program selecting either branch is run on the evaluator, on Wasm, and through the native toolchain
- **THEN** the three engines produce the same result

### Requirement: A finalizer runs on every Effect outcome without replacing it

`Effect.ensuring` SHALL run its finalizer after the protected Effect completes with a success and
after it completes with a typed failure, and SHALL then hand on that original success value or that
original typed failure unchanged. It MUST NOT replace the outcome, add to the protected Effect's
failure row, or let a recovering caller observe the outcome before the finalizer has run.

The finalizer SHALL be typed `Effect<() ! never ? S>`, so a finalizer failure is unrepresentable
rather than reconciled against the outcome being preserved. A caller whose release can fail SHALL
recover it into that contract before composing it, and the resulting Effect's requirement row SHALL
be the protected Effect's row widened by the finalizer's own.

The protected Effect's local cleanup SHALL run before the finalizer. The finalizer is acquired
outside the Effect it wraps, so the reverse-acquisition order that governs locals places it last.

A trap SHALL bypass the finalizer, as it bypasses `Effect.catch` and every `Drop` hook. Bootstrap
MUST NOT promise finalizer execution after a trap.

#### Scenario: Finalize after a success

- **WHEN** an Effect that succeeds is wrapped by `Effect.ensuring`
- **THEN** the finalizer runs and the original success value reaches the caller unchanged

#### Scenario: Finalize after a typed failure

- **WHEN** an Effect that fails with a typed failure is wrapped by `Effect.ensuring` and then recovered
- **THEN** the finalizer runs before recovery begins and the recovery handler receives that same failure with its payload intact

#### Scenario: Order the finalizer after local cleanup

- **WHEN** a protected Effect holding an owned local is wrapped by a finalizer that holds owned locals of its own
- **THEN** the protected Effect's local is released first and the finalizer's locals are released afterwards in reverse acquisition order

#### Scenario: Release an owner acquired inside the protected Effect

- **WHEN** the protected Effect acquires an owner inside its own body and then fails with a typed failure
- **THEN** that owner is released exactly once before the finalizer runs, and the finalizer's own owners are released exactly once after it

#### Scenario: Compose a fallible release

- **WHEN** a release that can fail is recovered into `() ! never` and passed as the finalizer
- **THEN** the composition is accepted, the recovery decides what a failed release means, and the protected Effect's outcome is still preserved

#### Scenario: Trap bypasses the finalizer

- **WHEN** a protected Effect divides by zero
- **THEN** execution traps without running the finalizer and without reporting structured cleanup completion
