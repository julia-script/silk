## ADDED Requirements

### Requirement: Comparisons and user branches lower to real control flow

The MIR type vocabulary SHALL grow to `I32` and `Bool`, and the binary operator vocabulary SHALL
grow with the non-trapping comparisons `Equals`, `NotEquals`, `LessThan`, `LessOrEqual`,
`GreaterThan`, and `GreaterOrEqual`, producing `Bool` from two `I32` operands. `Bool.not` SHALL
lower through existing operations rather than a new operation kind. Lowering a conditional
statement SHALL produce a user-authored `Branch` terminator on the condition local, arm blocks
in taken-then-otherwise order, and a join block where fall-through control continues; arm-local
drops follow the cleanup plan, and lowered programs SHALL verify clean and encode
deterministically, gated by committed golden files.

#### Scenario: Lower a conditional to a diamond

- **WHEN** `pub fn main() -> I32 { if I32.equals(1, 1) { return 1 } return 0 }` is lowered
- **THEN** the entry block computes the comparison and ends in a branch whose taken block returns `1` and whose otherwise path reaches the trailing return, verifying clean

#### Scenario: Keep comparisons non-trapping

- **WHEN** any comparison operation executes at any operand values
- **THEN** it produces a boolean result and never traps

#### Scenario: Match the branching golden encoding

- **WHEN** the committed conditional fixture is lowered and encoded
- **THEN** the encoding equals the committed golden text byte-for-byte
