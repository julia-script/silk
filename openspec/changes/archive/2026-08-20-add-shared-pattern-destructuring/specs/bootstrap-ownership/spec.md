## ADDED Requirements

### Requirement: Statement patterns preserve whole-value ownership

Every local or conditional pattern SHALL receive one recursive ownership plan derived from its
initializer access. An unconditional pattern SHALL be irrefutable. A consuming conditional SHALL
consume before testing and clean the active payload exactly once on both outcomes; a borrowed
conditional SHALL retain the owner and scope its loans to the conditional body. Post-statement
ownership SHALL join deterministically.

#### Scenario: Consume on conditional mismatch

- **WHEN** `if let Token token = move value` does not select `Token`
- **THEN** the unmatched active payload is cleaned and `value` remains consumed after the statement

#### Scenario: End a conditional borrow

- **WHEN** `if let Token token = &value` completes either body
- **THEN** the pattern loan ends and the owner is available after the conditional

#### Scenario: Retain irrefutable borrowed bindings

- **WHEN** `let Point { x, .. } = &point` succeeds
- **THEN** `x` retains its scoped shared view while unrelated shared reads of `point` remain valid
