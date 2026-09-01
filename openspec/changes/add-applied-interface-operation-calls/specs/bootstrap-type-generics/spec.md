## ADDED Requirements

### Requirement: An applied interface operation fixes the contract before its provider

An operation qualified by `Interface<Arguments>` SHALL first resolve one complete, visible,
kind-correct interface application. Analysis SHALL then determine implicit `Self` only from the
operation's supplied operands or static bounds belonging to the enclosing declaration and SHALL
select the one coherent conformance for that provider and application. Operand-derived evidence
SHALL take precedence when present, every declared operand occurrence of `Self` SHALL agree, and an
enclosing bound SHALL be consulted only as a fallback when operands do not determine the provider.
More than one matching fallback bound SHALL be ambiguous. Expected result types,
assignments, later uses, declaration order, and import order MUST NOT infer an omitted interface
argument or choose a conformance. The selected operation SHALL retain its substituted ownership,
result, failure, and requirement contract exactly.

#### Scenario: Select one of two applications for the same provider

- **WHEN** `Age` conforms to both `Encodable<u32>` and `Encodable<string>` and source calls `Encodable<u32>.encode(&age)`
- **THEN** analysis fixes the `u32` application, infers `Self = Age` from the operand, and selects only the `Age: Encodable<u32>` witness

#### Scenario: Select the other application explicitly

- **WHEN** the same source calls `Encodable<string>.encode(&age)`
- **THEN** analysis selects only the `Age: Encodable<string>` witness without consulting the call's expected result

#### Scenario: Preserve an explicit shared borrow

- **WHEN** an applied operation declares `self: &Self` and receives `&age`
- **THEN** the call observes `Age` through that supplied shared borrow and introduces no ownership adaptation or additional reference

#### Scenario: Reject an incomplete interface application

- **WHEN** a qualified interface application omits a required argument or supplies an argument of the wrong kind
- **THEN** analysis reports the ordinary application diagnostic before attempting provider or witness selection

#### Scenario: Reject a provider-free applied operation

- **WHEN** a zero-operand operation is qualified by complete interface arguments but neither an operand nor an enclosing bound determines `Self`
- **THEN** analysis reports that the provider cannot be determined and does not infer it from the expected result

#### Scenario: Prefer operand evidence over an enclosing bound

- **WHEN** supplied operands determine one provider and an enclosing declaration also has an applicable interface bound
- **THEN** analysis uses the operand-derived provider and does not let the bound replace or compete with that evidence

#### Scenario: Reject conflicting provider operands

- **WHEN** two declared operand occurrences of `Self` infer different providers
- **THEN** analysis diagnoses the conflicting operand origins before conformance search

#### Scenario: Reject ambiguous fallback bounds

- **WHEN** operands do not determine `Self` and more than one enclosing bound matches the complete interface application
- **THEN** analysis diagnoses ambiguous provider evidence and selects no witness

#### Scenario: Exclude applied service operations

- **WHEN** the applied owner resolves to a `service` rather than an `interface`
- **THEN** analysis rejects the applied operation selection and does not create a static interface-operation call

### Requirement: Applied interface operation sections complete through ordinary application

An applied interface operation with unresolved `Self` MAY remain a static callable section only
while an enclosing direct application, pipeline, or generic body supplies evidence that determines
the provider. A pipeline SHALL use its completed left operand as the omitted leading operation
operand, apply the same explicit interface arguments, and select the same witness as the equivalent
direct call. An operation with unresolved `Self` MUST NOT escape as a stored runtime-polymorphic
callable and MUST NOT acquire a generated source-visible helper declaration, runtime type argument,
or witness dictionary.

#### Scenario: Pipe a provider into an applied operation

- **WHEN** source evaluates `&age |> Encodable<u32>.encode`
- **THEN** the pipeline completes the operation with `&age` and selects the same `Age: Encodable<u32>` witness as `Encodable<u32>.encode(&age)`

#### Scenario: Run an effectful applied operation pipeline

- **WHEN** `encode` is effectful and source evaluates `run &age |> Encodable<u32>.encode`
- **THEN** the applied operation constructs its declared Effect and `run` executes exactly that complete pipeline's one Effect layer

#### Scenario: Reject an escaping open applied operation

- **WHEN** source stores `Encodable<u32>.encode` without an operand, bound, or other static application that determines `Self`
- **THEN** analysis rejects the unresolved provider before executable specialization and creates no runtime-polymorphic callable

### Requirement: Applied interface resolution failures stop before realization

Every failed applied interface operation SHALL remain an unavailable semantic expression carrying a
deterministic source diagnostic with a stable code and owner- or member-qualified source span.
Missing operations, inaccessible interfaces, invalid applications, undetermined providers, missing
conformances, and ambiguous evidence MUST NOT publish a provisional call fact, witness, executable
instance, Effect site, Effect constructor, or Effect runner and MUST NOT reach lowering as a
partially resolved call.

#### Scenario: Report a missing conformance before lowering

- **WHEN** the operand determines a nominal provider that does not conform to the written interface application
- **THEN** analysis reports the complete provider-interface goal and no realization phase attempts to lower the call

#### Scenario: Keep an invalid effect operation out of entry lowering

- **WHEN** an applied effect operation cannot resolve its provider, application, or witness
- **THEN** compilation returns source diagnostics rather than failing because Effect entry lowering lacks a constructor or runner
