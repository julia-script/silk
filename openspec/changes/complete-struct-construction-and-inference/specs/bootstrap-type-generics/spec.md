## MODIFIED Requirements

### Requirement: Generic applications are explicit canonical types

Applying a generic nominal declaration SHALL produce a canonical type identified by the declaration
plus normalized ordered arguments. In required type positions, every kind-correct argument SHALL
remain explicit. Named struct construction MAY instead supply a contiguous explicit prefix of
ordinary value arguments and SHALL infer its omitted ordinary suffix from all supplied fields;
construction MAY also infer concrete representation arguments from corresponding field
initializers. Applying arguments to a non-generic declaration, supplying the wrong kind, leaving a
parameter uninferred, or producing conflicting field constraints MUST remain explicit semantic
failures. Expected result types and later uses MUST NOT participate in construction inference.

#### Scenario: Reuse one applied type identity

- **WHEN** independent declarations refer to `Box<Token>`
- **THEN** both references resolve to the same canonical applied type identity

#### Scenario: Infer a construction representation

- **WHEN** `Mapper` construction supplies a named function for field `F`
- **THEN** the complete applied type includes that exact representation argument

#### Scenario: Infer an ordinary construction suffix

- **WHEN** `Pair<A, B>` construction writes `Pair<i32> { first: 1, second: true }`
- **THEN** the complete nominal type is `Pair<i32, bool>` using only the supplied fields

#### Scenario: Reject the wrong arity

- **WHEN** `Pair<i32>` appears in a required type position for a declaration with two parameters
- **THEN** analysis reports the expected and actual argument counts and produces no available applied type

#### Scenario: Reject conflicting construction evidence

- **WHEN** two supplied fields imply distinct arguments for the same omitted parameter
- **THEN** inference retains both field origins, reports the conflict, and produces no applied nominal type
