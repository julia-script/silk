## MODIFIED Requirements

### Requirement: Calls infer only from supplied arguments

A generic call MAY supply a contiguous, ordered, kind-correct prefix of explicit generic arguments,
including value-type, failure-row, and requirement-row arguments. Analysis SHALL bind that prefix
positionally and infer every remaining suffix argument only from supplied call arguments and checked
constraints according to each constraint's binding policy: membership and subset are checking-only,
while provider selection may bind its selected row only by the unique-candidate rule.

When explicit arguments or supplied call operands have already fixed a provider parameter, a direct
applied-interface constraint on that parameter MAY additionally bind unresolved call arguments from
the one visible, coherent conformance selected for that known provider and interface. Analysis SHALL
unify the conformance's applied interface arguments monotonically with existing inference: it may
fill an unresolved argument but MUST NOT replace an explicit or operand-derived binding. Missing,
ambiguous, or conflicting conformance evidence SHALL produce a deterministic inference failure.
Analysis MUST NOT enumerate candidate providers in order to activate this rule.

Expected return types and uses after complete application MUST NOT bind generic arguments, prune
constraint candidates, select a provider, or select a conformance. Interface-constraint inference
MUST NOT create runtime type arguments, witness dictionaries, service requirements, or an erased
representation for a higher-ranked callable.

Forming an automatic leading-argument section SHALL infer from supplied trailing arguments and
retain every unresolved binder and constraint determined by the omitted leading parameter in the
section's semantic callable type. Applying that section SHALL complete inference from the leading
argument. A missing, conflicting, wrong-kind, or excess explicit argument MUST produce a
deterministic diagnostic at the responsible prefix or application.

#### Scenario: Infer identity from its argument

- **WHEN** `identity(value)` calls `identity<T>(value: T)` with a `Token`
- **THEN** the call specializes `T` as `Token`

#### Scenario: Infer through a generic section

- **WHEN** a generic data-first function forms a section from trailing arguments and is then piped a leading `Token`
- **THEN** the complete application resolves one canonical `Token` specialization

#### Scenario: Infer interface arguments from a known provider

- **WHEN** a supplied `ConcreteContext` fixes `C` in a call bounded by `C: BufferedContext<P, A, E, ?R>` and its one visible coherent conformance is `BufferedContext<Socket, Output, ContextError, ?&mut Clock>`
- **THEN** analysis binds `P`, `A`, `E`, and `R` to that applied conformance and records one fully static specialization

#### Scenario: Preserve earlier operand evidence

- **WHEN** another supplied operand has already bound `P = Socket` and the known context provider's selected conformance also applies `BufferedContext<Socket, A, E, ?R>`
- **THEN** conformance-derived inference agrees with and preserves the existing `P` binding while filling only unresolved arguments

#### Scenario: Reject conflicting conformance evidence

- **WHEN** supplied operand evidence binds `P = Socket` but the known context provider's selected conformance applies `BufferedContext<Pipe, A, E, ?R>`
- **THEN** analysis reports the conflicting origins and produces no specialization

#### Scenario: Reject non-unique known-provider evidence

- **WHEN** the known provider does not have exactly one visible coherent conformance satisfying the direct applied-interface constraint
- **THEN** analysis reports the missing or ambiguous constraint evidence and does not guess interface arguments

#### Scenario: Do not infer an unknown provider backwards

- **WHEN** a constrained provider parameter is not fixed by an explicit argument or supplied operand
- **THEN** analysis does not enumerate conformances to choose that provider or bind the interface arguments

#### Scenario: Refuse return-only inference

- **WHEN** `empty()` calls `empty<T>() -> T` without explicit type arguments
- **THEN** specialization fails even when the call result is later used where `Token` is expected

#### Scenario: Specialize explicitly

- **WHEN** `empty<Token>()` calls `empty<T>() -> T`
- **THEN** the call records the concrete `Token` specialization

#### Scenario: Supply a requirement-row prefix and infer the suffix

- **WHEN** `Effect.provideMut<Logger>(effect, &mut provider)` supplies first binder `?S` and leaves later binders implicit
- **THEN** analysis accepts the requirement-row argument, fixes `S`, and infers the suffix only from `effect`, `provider`, and their checked constraint

#### Scenario: Supply a row prefix through a pipeline

- **WHEN** `effect |> Effect.provideMut<Logger>(&mut provider)` supplies the same first row binder on a trailing-argument section
- **THEN** the section retains the omitted Effect-dependent suffix and completes it from the pipeline input without consulting the expected result

#### Scenario: Lift a failure singleton into a failure-row prefix

- **WHEN** a call whose first binder is `!E` supplies nominal `Problem` as its first explicit generic argument
- **THEN** analysis lifts `Problem` to the singleton failure row `Problem` and infers the remaining suffix from supplied arguments and constraints

#### Scenario: Reject a wrong-kind explicit prefix

- **WHEN** an explicit argument cannot form a member of the binder's row domain, such as `&Logger` for a failure-row binder or non-capability nominal `Problem` for a requirement-row binder
- **THEN** analysis reports a kind mismatch at that explicit generic argument rather than treating every value type as a valid row singleton

#### Scenario: Infer identity from its argument

- **WHEN** `identity(value)` calls `identity<T>(value: T)` with a `Token`
- **THEN** the call specializes `T` as `Token`

#### Scenario: Infer through a generic section

- **WHEN** a generic data-first function forms a section from trailing arguments and is then piped a leading `Token`
- **THEN** the complete application resolves one canonical `Token` specialization

#### Scenario: Refuse return-only inference

- **WHEN** `empty()` calls `empty<T>() -> T` without explicit type arguments
- **THEN** specialization fails even when the call result is later used where `Token` is expected

#### Scenario: Specialize explicitly

- **WHEN** `empty<Token>()` calls `empty<T>() -> T`
- **THEN** the call records the concrete `Token` specialization

#### Scenario: Reject excess explicit arguments

- **WHEN** a call supplies more explicit arguments than the declaration's canonical generic sequence
- **THEN** analysis reports the excess suffix and produces no specialization
