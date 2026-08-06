## ADDED Requirements

### Requirement: Match modes preserve affine ownership

Ownership checking SHALL classify a bare match as a Copy read, a consuming match as one whole-value
transfer, a shared match as one lexical shared borrow, and an exclusive match as one lexical
exclusive borrow requiring a mutable live root. Borrowed pattern bindings SHALL end at their arm and
MUST NOT escape or be consumed. A consuming match SHALL make the source unavailable and transfer the
active payload into exactly one selected arm.

#### Scenario: End a shared arm borrow

- **WHEN** a shared arm reads a Copy field and returns a scalar
- **THEN** the borrow ends at the arm boundary and the source owner retains its original cleanup obligation

#### Scenario: Reject an escaping pattern borrow

- **WHEN** a shared or exclusive pattern binding would become the match result or enter owned storage
- **THEN** ownership reports the escape and publishes no executable match

### Requirement: Consuming destructuring cleans exactly one selected payload

For a consuming nominal arm, bound non-Copy fields SHALL become arm-local owners and omitted fields
acknowledged by `..` SHALL remain cleanup obligations. Branch exit, early return, nested control,
guard failure, and traps SHALL release every untransferred active field exactly once in canonical
cleanup order. Inactive union members and the consumed source SHALL receive no cleanup.

#### Scenario: Clean omitted fields

- **WHEN** a consuming `Token` arm returns one moved field and omits another with `..`
- **THEN** only the omitted active field is cleaned in the arm and neither the moved field nor inactive members are released there

#### Scenario: Guard failure preserves the payload

- **WHEN** a consuming guarded arm rejects the active member and a later arm handles the same member
- **THEN** ownership transfers the payload only into the selected later arm without cleaning or duplicating it during the failed guard
