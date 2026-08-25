## ADDED Requirements

### Requirement: MIR verifies logical scalar enums over one physical representation plan

MIR SHALL carry canonical enum logical types and member constants together with the validated
fixed-width representation plan used for physical lowering. MIR verification SHALL reject a member
from another enum, a discriminant not belonging to a declared member, a representation-lane mismatch,
or an enum match decision that is invalid for the scrutinee's canonical enum. Equality, `value`, and
match operations SHALL remain target-neutral.

#### Scenario: Verify a complete enum match region

- **WHEN** HIR lowers an exhaustive enum match
- **THEN** MIR records decisions for the scrutinee enum's canonical members and one validated scalar representation without arbitrary integer cases

#### Scenario: Reject an undeclared MIR discriminant

- **WHEN** malformed MIR associates an enum constant with a backing value no member declares
- **THEN** MIR verification rejects the program before evaluation or backend lowering
