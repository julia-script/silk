## ADDED Requirements

### Requirement: An interface witness names one intrinsic or one function of its own provider

An interface conformance SHALL map each of the interface's operations to a two-segment path naming
either one sealed `Intrinsic` operation or one function declared by the provider type's own actor.
No other target SHALL be admitted, so a conformance can never name a function belonging to another
type. A mapping whose provider-qualified name matches no declaration SHALL be reported as a mapped
operation that does not exist.

Both targets SHALL be checked against the same contract, substituted by the conformance's interface
type arguments: the same arity, the same operand and result types, an ordinary function kind, and no
failure or requirement row. They differ only in operand form. A sealed operation SHALL take the
contract's operand types as declared. A source witness SHALL receive each contract operand by shared
borrow and return the contract result by value, because an interface operation never consumes what
it reads and no nominal provider is `Copy`; a source witness declaring a by-value operand, extra
type parameters, a failure row, or a requirement row SHALL be rejected as incompatible with the
operation it maps.

The completeness rule is unchanged by the target's form: a conformance SHALL still map every
operation the interface declares and no operation it does not.

#### Scenario: Admit a user type's own function as a witness

- **WHEN** `impl Order<Cell> for Cell { lessThan: Cell.cellLess }` names `fn cellLess(left: &Cell, right: &Cell) -> bool`
- **THEN** the conformance is admitted and recorded as the witness for `Cell`

#### Scenario: Reject a by-value operand

- **WHEN** the mapped function is declared `fn cellLess(left: Cell, right: Cell) -> bool`
- **THEN** the conformance is rejected as incompatible with the operation it maps

#### Scenario: Reject a mapping to a function that does not exist

- **WHEN** a conformance maps an operation to `Cell.absent` and the provider's actor declares no such function
- **THEN** the conformance reports that the mapped operation does not exist

#### Scenario: Keep every shipped intrinsic witness admissible

- **WHEN** the standard library maps `Order` and `Integer` to `Intrinsic.*` operations for each scalar
- **THEN** every one of those conformances is admitted exactly as before, with its source unchanged

#### Scenario: Require complete coverage whatever the target

- **WHEN** a user type's conformance to a two-operation interface maps only one operation
- **THEN** the conformance is rejected naming the unmapped operation, as it is for an intrinsic witness
