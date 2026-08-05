## ADDED Requirements

### Requirement: Conditional statements parse losslessly

The parser SHALL accept `if <expression> { <statements> } else { <statements> }` wherever a
statement may appear, with the `else` arm optional and no parentheses around the condition, per
the accepted surface. Each arm SHALL be a brace-delimited statement sequence (bindings,
conditionals, and return statements), and the function body SHALL still end in exactly one
trailing return statement after any conditionals. `true` and `false` SHALL parse as boolean
literal expressions wherever an expression is allowed. Missing conditions, braces, and damaged
arms SHALL remain explicit recovery data bounded by the existing statement and declaration
anchors, and every token SHALL be retained losslessly.

#### Scenario: Parse a conditional with both arms

- **WHEN** a body spells `if flag { return 1 } else { return 2 } return 0`
- **THEN** the block contains one conditional statement retaining the keyword, condition expression, both brace-delimited arms, and the else keyword, followed by the trailing return statement

#### Scenario: Parse a conditional without an else arm

- **WHEN** a body spells `if flag { return 1 } return 0`
- **THEN** the conditional retains one arm and no else branch, and the trailing return remains a sibling statement

#### Scenario: Parse boolean literals

- **WHEN** a return expression spells `true` and a binding initializer spells `false`
- **THEN** both are boolean literal expressions retaining their keyword tokens

#### Scenario: Recover a missing condition

- **WHEN** a body spells `if { return 1 } return 0`
- **THEN** the conditional retains an explicit missing condition with one parser diagnostic and both blocks keep parsing

#### Scenario: Recover a damaged arm before the next statement

- **WHEN** an arm omits its closing brace before the trailing return statement
- **THEN** recovery inserts the missing brace and the trailing return remains a separate statement
