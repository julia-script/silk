## ADDED Requirements

### Requirement: Exact executable bounds admit sealed static property conjuncts

One exact Effect or callable representation binder SHALL retain its representation identity when
conjoined with compiler-owned static properties such as `Intrinsic.Detached` and
`Intrinsic.NonParking`. The conjunction MUST NOT reinterpret the executable as an interface bound,
admit ordinary interface or service conjuncts in that position, create a runtime witness, or become
a general intersection type. Substitution SHALL select one concrete executable representation and
re-evaluate every sealed property; forwarding, caching, and serialization SHALL preserve the exact
identity and ordered property set deterministically.

#### Scenario: Admit a detached exact Effect binder

- **WHEN** a generic declares `F: once Effect<A> + Intrinsic.Detached`
- **THEN** `F` remains one exact Effect representation parameter with an additional sealed static obligation

#### Scenario: Admit a detached non-parking exact callback binder

- **WHEN** a generic declares one exact callback representation with Detached and NonParking conjuncts
- **THEN** analysis retains one callable identity and verifies both properties without producing runtime conformance data

#### Scenario: Reject an ordinary interface conjunct

- **WHEN** source conjoins an ordinary interface or service with an exact executable representation in the sealed-property position
- **THEN** analysis rejects the bound and does not reinterpret the executable as nominal conformance

#### Scenario: Preserve identity through forwarding

- **WHEN** one generic forwards its exact executable parameter and sealed properties through another generic and then specializes it
- **THEN** the final specialization retains the concrete representation identity and re-evaluated property verdicts without a representation join

#### Scenario: Encode the conjunction deterministically

- **WHEN** equivalent exact executable bounds are analyzed repeatedly or restored from cached semantic data
- **THEN** their canonical identity, sealed-property order, and verdict encoding are byte-identical
