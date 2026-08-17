## ADDED Requirements

### Requirement: Generic HIR carries symbolic row contracts and proof evidence

Generic HIR SHALL retain symbolic row expressions, lifted member terms,
member-well-formedness obligations, callable constraints, substitutions, and `Assumed` evidence.
Requirement-binding HIR SHALL store provider-selection access separately from expression capture
access and SHALL identify the exact solved wanted without requiring concrete capability, role,
provider match, or witness fields before specialization.

A specialized branded binding SHALL contain concrete `RequirementSelection` evidence. HIR encoding,
equality, keys, copying, and dependency analysis SHALL traverse symbolic rows and evidence
deterministically.

#### Scenario: Represent a generic wrapper binding

- **WHEN** a generic wrapper calls a binding intrinsic under a definitionally equivalent declared constraint
- **THEN** HIR contains the symbolic result row and assumed proof without choosing a concrete row member

#### Scenario: Separate selection and capture access

- **WHEN** an exclusive provider satisfies a stored shared requirement
- **THEN** HIR records selected stored access as shared and provider expression capture access as exclusive

#### Scenario: Reject symbolic proof from concrete HIR consumers

- **WHEN** a row-dependent HIR consumer is given an unupgraded assumed proof
- **THEN** the required concrete specialized bundle cannot be constructed
