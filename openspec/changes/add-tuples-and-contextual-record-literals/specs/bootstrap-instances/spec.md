## ADDED Requirements

### Requirement: Generated aggregates participate in canonical runtime reachability

Instance discovery SHALL follow every named tuple or anonymous aggregate nominal identity that
appears in a reachable contract, local, generic substitution, construction, projection, borrow, or
cleanup plan. Generic instance keys SHALL use the complete occurrence-based nominal identity for an
anonymous aggregate, so repeated uses of one bound value share a specialization while distinct
same-shaped literal occurrences remain distinct concrete type arguments.

Generated aggregate reachability and ordering SHALL be deterministic and SHALL recursively follow
member types through the existing nominal struct rules. An unused anonymous literal in an
unreachable declaration MUST NOT enter runtime reachability merely because its synthesized
declaration is present in semantic facts.

#### Scenario: Specialize a generic formatter-shaped consumer

- **WHEN** one anonymous record binding is passed repeatedly to the same reachable generic function
- **THEN** instance discovery records one concrete specialization for that occurrence-based aggregate type

#### Scenario: Distinguish separate anonymous arguments

- **WHEN** separate same-shaped record literals are passed to one generic function
- **THEN** instance discovery retains two concrete nominal type arguments rather than merging them by shape

#### Scenario: Omit an unreachable generated type

- **WHEN** an anonymous aggregate occurs only inside an unreachable declaration
- **THEN** its semantic declaration remains inspectable but is absent from runtime aggregate reachability
