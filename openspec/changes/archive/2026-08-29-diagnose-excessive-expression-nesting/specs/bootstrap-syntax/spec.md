## ADDED Requirements

### Requirement: Expression nesting is bounded and losslessly recoverable

The parser SHALL accept at most 256 nested expression edges. An expression parsed directly in a
statement, declaration, or other non-expression position SHALL have depth zero. Beginning an
expression while another containing expression remains active SHALL increment the containing
depth by one, including grouped contents, prefix operands, operator operands, pipeline targets,
call arguments, array or aggregate elements, field initializers, match components, and every other
expression child. Sequential sibling expressions SHALL each use their common parent's depth plus
one and SHALL NOT accumulate depth from preceding siblings.

Concrete syntax whose maximum depth is at most 256 SHALL retain its ordinary concrete shape. When
parsing would begin a child at depth 257, the parser SHALL instead retain the over-budget region as
one explicit error branch, consume at least the first significant token of that child, preserve all
source tokens exactly once, and resume at the owning expression boundary. Recovery SHALL remain
host-stack independent even when the rejected region is substantially deeper than the limit.

#### Scenario: Preserve syntax immediately below the limit

- **WHEN** a valid expression has maximum depth 255
- **THEN** the parser produces its ordinary concrete syntax without an excessive-nesting diagnostic

#### Scenario: Preserve syntax at the limit

- **WHEN** a valid expression has maximum depth exactly 256
- **THEN** the parser produces its ordinary concrete syntax without an excessive-nesting diagnostic

#### Scenario: Recover the first edge beyond the limit

- **WHEN** parsing a child expression would increase the active expression depth from 256 to 257
- **THEN** that child is represented by one explicit error branch and parsing advances beyond the offending syntax

#### Scenario: Bound every recursive expression form

- **WHEN** grouping, array nesting, call or container nesting, or direct prefix nesting extends substantially beyond depth 256
- **THEN** each form returns a lossless recovered syntax tree without exhausting the host stack

#### Scenario: Measure siblings independently

- **WHEN** one container contains multiple sibling expressions whose individual depths do not exceed 256
- **THEN** earlier siblings do not reduce the nesting available to later siblings

#### Scenario: Resume after an over-budget expression

- **WHEN** an over-budget expression is followed by another statement in its block and another top-level declaration
- **THEN** both following constructs remain independently parseable and no token from either is consumed into the recovered expression
