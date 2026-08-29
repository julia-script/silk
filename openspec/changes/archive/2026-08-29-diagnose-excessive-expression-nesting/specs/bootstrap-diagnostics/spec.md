## ADDED Requirements

### Requirement: Excessive expression nesting has one stable parser diagnostic contract

The parser SHALL report code `PAR0005` with parser phase, error severity, and structured reason
`ExpressionNestingLimitExceeded` when an expression child would begin at depth 257. The reason
SHALL retain the configured limit `256` and attempted depth `257`. Its half-open primary span SHALL
cover exactly the first significant source token that would begin the over-budget child expression,
excluding leading trivia.

One maximal over-budget expression region SHALL produce exactly one `PAR0005` diagnostic regardless
of how far the rejected region extends beyond the limit. Two independently parsed over-budget
expressions SHALL produce two diagnostics. The diagnostic SHALL be produced only by the explicit
depth comparison; parser invariant `RangeError` defects and other unexpected exceptions MUST remain
defects and MUST NOT be reclassified as `PAR0005`.

#### Scenario: Diagnose the first over-budget token

- **WHEN** the first child beyond depth 256 begins with a concrete grouping delimiter, container delimiter, callee or argument token, or prefix operator
- **THEN** one `PAR0005` diagnostic spans exactly that first significant token and records limit 256 and attempted depth 257

#### Scenario: Collapse one maximal rejected region

- **WHEN** one expression continues for thousands of nested edges beyond the first rejected edge
- **THEN** the parser reports one `PAR0005` diagnostic for the maximal recovered region

#### Scenario: Diagnose independent expressions independently

- **WHEN** two separate statements or declarations each contain an over-budget expression
- **THEN** the parser reports two `PAR0005` diagnostics at their respective first over-budget tokens

#### Scenario: Preserve parser invariant defects

- **WHEN** a parser invariant fails with a `RangeError` outside the explicit expression-depth comparison
- **THEN** that `RangeError` remains a defect and no `PAR0005` diagnostic is synthesized
