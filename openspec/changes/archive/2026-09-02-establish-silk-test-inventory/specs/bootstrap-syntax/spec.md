## ADDED Requirements

### Requirement: Test markers are lossless and declaration-local

The grammar SHALL recognize `test` contextually as a marker immediately associated with one
top-level named function declaration and SHALL retain it losslessly in syntax artifacts. The marker
MUST NOT reserve `test` in ordinary identifier positions. Recovery from a damaged marked
declaration SHALL remain within that declaration and preserve a following declaration.

#### Scenario: Parse a marked Effect function

- **WHEN** source declares `test effect fn succeeds() -> () { ... }`
- **THEN** syntax retains the marker, Effect kind, name, signature, body, spans, and trivia exactly

#### Scenario: Parse a public marked Effect for semantic rejection

- **WHEN** source declares `pub test effect fn exposed() -> () { ... }`
- **THEN** syntax retains public visibility followed by the test marker and Effect declaration so semantic eligibility, rather than parser recovery, owns the rejection

#### Scenario: Parse a marked ordinary function for semantic rejection

- **WHEN** source declares `test fn ordinary() -> () { ... }`
- **THEN** syntax retains the test marker and ordinary function kind so semantic eligibility owns the rejection

#### Scenario: Keep test usable as an identifier

- **WHEN** `test` appears in an ordinary non-marker identifier position
- **THEN** it retains the grammar meaning available to an ordinary identifier in that position

#### Scenario: Recover before the next declaration

- **WHEN** a marked function is malformed before a valid following declaration
- **THEN** parser recovery reports the local damage and preserves the following declaration intact
