## ADDED Requirements

### Requirement: Referent projection is a postfix place expression

The parser SHALL recognize `reference.*` as a postfix referent projection within the repeated
projection chain. Referent projection SHALL bind more tightly than prefix, infix, and pipeline
operators, compose with field, index, call, borrow, and assignment syntax, and remain distinct from
infix multiplication.

#### Scenario: Parse a scalar receiver projection

- **WHEN** source contains `self.*`
- **THEN** the parser produces a referent-projection expression whose subject is `self`
- **AND** the dot and star tokens belong to that projection

#### Scenario: Compose a referent with other projections

- **WHEN** source contains `items[index].*.field`
- **THEN** parsing retains the index, referent, and field projections in source order
- **AND** the chain can be used as a borrow or assignment subject

#### Scenario: Preserve multiplication syntax

- **WHEN** source contains `left * right`
- **THEN** parsing produces an infix multiplication expression rather than a referent projection

#### Scenario: Recover an incomplete referent projection

- **WHEN** source contains a postfix dot whose following star or subject is missing
- **THEN** parsing reports the ordinary local syntax failure
- **AND** subsequent declarations remain recoverable
