## ADDED Requirements

### Requirement: Compile-time matching executes ordinary arms in the current computation

Compile-time evaluation SHALL accept ordinary match-arm blocks in every otherwise legal expression position and execute only the selected block eagerly in statement order. Block-local bindings and mutation SHALL obey ordinary lexical scope. Normal block completion SHALL yield unit without returning from the enclosing static computation; a body without normal completion SHALL produce the ordinary enclosing transfer rather than a block value. Explicit nested callable and Effect execution boundaries and existing static legality restrictions SHALL remain in force. No separate runtime evaluator SHALL be introduced.

#### Scenario: Evaluate selected sequential statements

- **WHEN** a static computation selects an ordinary block that mutates local state twice while an unselected block would alter it differently
- **THEN** the static result reflects only the selected statements in order and subsequent enclosing statements execute after unit completion

#### Scenario: Preserve guarded fallthrough

- **WHEN** a guarded ordinary block is rejected and a later arm is selected during compile-time evaluation
- **THEN** the rejected block performs no work and the later selected body determines the current computation outcome

#### Scenario: Return from a match argument at compile time

- **WHEN** a selected ordinary block nested in a call argument or initializer executes return during compile-time evaluation
- **THEN** the current static body returns immediately without evaluating later arguments, performing the call, storing the initializer, or executing later statements

#### Scenario: Distinguish loop targets during compile-time evaluation

- **WHEN** an ordinary arm breaks or continues an enclosing loop, or breaks a loop declared inside itself
- **THEN** evaluation uses the same lexical target as analysis and resumes only at the correct loop boundary

#### Scenario: Transfer while evaluating a static guard

- **WHEN** a static guard evaluates a nested match whose selected ordinary block returns or transfers to an enclosing loop
- **THEN** evaluation takes that transfer and does not evaluate later candidates; only a normally completing Boolean-false result advances to another candidate
