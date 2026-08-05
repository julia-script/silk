## ADDED Requirements

### Requirement: Binary arithmetic is a trapping MIR operation

MIR SHALL represent arithmetic as one binary operation carrying the closed operator (`Add`,
`Subtract`, `Multiply`, `Divide`, `Remainder`), typed left and right operand locals, a typed
destination local, and provenance. The operation's semantics SHALL be trapping: signed overflow,
division by zero, and `-2147483648` divided or remaindered by `-1` abort the function exactly
like an explicit trap terminator, in every build mode. Division SHALL truncate toward zero and
remainder SHALL take the dividend's sign. Lowering SHALL map HIR builtin calls to binary
operations after their operands, the verifier SHALL check operand and destination locals like
every other operation, and the encoder SHALL cover the operator vocabulary, gated by committed
golden files.

#### Scenario: Lower a built-in call to a binary operation

- **WHEN** `main` returning `I32.add(40, 2)` is lowered
- **THEN** the block computes both literal operands and one `Add` binary operation into the returned local, verifying clean

#### Scenario: Verify binary operand references

- **WHEN** a hand-built module's binary operation references an undeclared local
- **THEN** the verifier reports that violation deterministically

#### Scenario: Match the arithmetic golden encoding

- **WHEN** a committed arithmetic fixture is lowered and encoded
- **THEN** the encoding equals the committed golden text byte-for-byte, naming each binary operator
