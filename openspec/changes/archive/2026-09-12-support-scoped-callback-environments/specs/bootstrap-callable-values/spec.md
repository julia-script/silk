## MODIFIED Requirements

### Requirement: Expected callable contracts admit one finite outer lifetime binder

Compatibility SHALL support one outer for<'a, ...> lifetime binder on an expected callable or operation contract, including references to already bound surrounding lifetimes. Checking SHALL introduce scoped rigid placeholders, apply ordinary variance and finite outlives obligations, and reject placeholder escape. The quantified signature SHALL NOT contain nested quantified callable contracts. The checker SHALL reject arbitrary unconstrained higher-rank inference. Compatibility SHALL validate the already selected operation rather than choose implementation or provider candidates.

The returned Effect environment SHALL be allowed to contain a finite intersection of invocation
and surrounding captured lifetimes. Opening the outer binder SHALL preserve surrounding
lifetimes as distinct free constituents, and closing it SHALL reject invocation placeholders in
inferred success, failure, requirement, or retained representation arguments. An environment
mismatch SHALL be diagnosed as a lifetime/contract failure rather than an open-row failure
solely because the callback preserves a symbolic requirement row.

#### Scenario: Compare a universally borrowed callback

- **WHEN** an expected for<'call> fn(&'call T) -> &'call T contract is supplied a compatible generic identity
- **THEN** the offered signature satisfies the scoped rigid lifetime for every invocation without a runtime lifetime argument

#### Scenario: Reject a nested quantified signature

- **WHEN** a quantified callable signature contains a second quantified callable type in a parameter or result
- **THEN** analysis reports the unsupported form without searching alternate quantifier arrangements

#### Scenario: Reject placeholder escape

- **WHEN** checking attempts to store a quantified invocation's rigid reference into surrounding longer-lived storage
- **THEN** the escape is rejected at the assignment or retaining boundary

#### Scenario: Compose a captured scoped callback

- **WHEN** an anonymous callback borrows a resource, retains independently borrowed configuration and a once callback, and satisfies a returned Effect environment of `'scope & 'env`
- **THEN** generic checking accepts the finite declared relationship and preserves the callback's exact success, failure, and symbolic requirement row without equating `'scope` with `'env`

#### Scenario: Preserve an invocation placeholder inside an intersection

- **WHEN** inferred output storage or a retained computation hides a fresh invocation lifetime inside an intersection
- **THEN** closing the higher-ranked comparison still rejects that escape

## ADDED Requirements

### Requirement: Scoped nonparking resource brackets accept captured Effect environments

A scoped use/release bracket SHALL accept use and release callbacks whose returned Effects are
valid for the intersection of the fresh resource borrow and their captured environment. It
SHALL own the resource and retain both callbacks until their obligations end, preserve arbitrary
callback failure and service rows, and keep success/failure values independent of the temporary
resource loan. Release SHALL remain nonparking and infallible, occur exactly once after success,
typed failure, or structured cancellation, and preserve the original outcome. The use Effect
and its loan SHALL end before release receives a fresh exclusive loan. Nested brackets SHALL
release in reverse order. Fatal-trap behavior SHALL remain unchanged.

#### Scenario: Lower the captured generic resource reproduction

- **WHEN** the bracket is supplied an owned resource, a callback retaining borrowed configuration and a higher-ranked callback with symbolic services, and a valid nonparking release
- **THEN** the program analyzes and lowers with exact service requirements and disjoint use/release loans

#### Scenario: Preserve service exclusion

- **WHEN** a wrapper excludes an ambient transport service from the callback row and that service is supplied as part of the callback's independent requirements
- **THEN** the wrapper call is rejected even when its captured environment satisfies the bracket lifetime contract

#### Scenario: Reject a resource-borrow result

- **WHEN** a callback tries to return a borrow of the bracket-owned resource or an Effect retaining that borrow as the bracket's result
- **THEN** the compiler rejects the escape before lowering

#### Scenario: Cancel a captured use Effect

- **WHEN** structured cancellation interrupts a suspended captured use Effect
- **THEN** its state and resource loan are cleaned before the release callback receives its fresh loan, and release runs exactly once without replacing the cancellation outcome

#### Scenario: Reject a parking release

- **WHEN** a release callback may park independently of its captured lifetime validity
- **THEN** the nonparking bracket rejects it
