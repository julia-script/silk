## ADDED Requirements

### Requirement: Requirement access satisfaction follows one partial order

Requirement/access satisfaction SHALL be decided by a single partial order applied identically by type compatibility, representation-shape equality, and interface-witness selection. Given access levels Shared, Exclusive, and Take, a supplied access SHALL satisfy a required access when the supplied access is at least as strong as the required access: Take satisfies every access, Exclusive satisfies Exclusive and Shared, and Shared satisfies only Shared.

#### Scenario: A stronger access satisfies a weaker requirement

- **WHEN** a requirement demands Shared access
- **THEN** a provider supplying Exclusive or Take access satisfies it

#### Scenario: A weaker access does not satisfy a stronger requirement

- **WHEN** a requirement demands Exclusive access
- **THEN** a provider supplying Shared access does not satisfy it

#### Scenario: Shape equality and compatibility agree

- **WHEN** two requirement rows are compared for representation-shape equality and for compatibility
- **THEN** both paths report the same satisfaction result for every Shared/Exclusive/Take pair

#### Scenario: Witness selection obeys the same order

- **WHEN** an interface witness is selected for a provider whose access differs from the requirement
- **THEN** satisfaction is judged by the same partial order used by compatibility, never by an exact-equality-only rule
