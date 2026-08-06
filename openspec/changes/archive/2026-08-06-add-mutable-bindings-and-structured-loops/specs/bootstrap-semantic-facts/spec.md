## ADDED Requirements

### Requirement: Mutation facts identify one writable place

Semantic analysis SHALL publish whether each binding is mutable and one ordered place fact for every
assignment destination, including its root binding, field/index selectors, dynamic checks, exact
destination type, right-hand type, compatibility, provenance, and complete-or-unavailable write
outcome. A failed selector SHALL leave earlier place facts queryable and make later steps causally
unavailable without choosing another destination.

#### Scenario: Resolve a nested array write

- **WHEN** source assigns to `pairs[index].left`
- **THEN** facts identify the mutable array root, checked index, canonical field, exact `I32` destination, and assignment compatibility

#### Scenario: Diagnose a non-writable destination

- **WHEN** an assignment targets an immutable binding or unavailable projection
- **THEN** the destination facts retain the exact root or failed selector and one stable causal diagnostic

### Requirement: Loop facts retain regions and lexical transfers

Each loop SHALL publish a canonical loop identity, typed condition fact, ordered body statements,
lexical parent, and complete-or-unavailable outcome. Every `break` and `continue` SHALL identify its
innermost enclosing loop and exact transfer span. A non-`Bool` condition or transfer outside a loop
SHALL produce a stable diagnostic without erasing independent nested facts.

#### Scenario: Type a valid while condition

- **WHEN** a `while` condition is a comparison over `I32`
- **THEN** the loop fact records the canonical `Bool` condition and ordered body facts

#### Scenario: Diagnose an invalid loop condition

- **WHEN** a `while` condition has type `I32`
- **THEN** semantic analysis reports the exact expected and actual types and publishes no executable loop outcome
