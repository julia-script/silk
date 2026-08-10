## MODIFIED Requirements

### Requirement: Text and byte literals parse losslessly

The parser SHALL accept single-line and multiline text and byte literal tokens as static-literal
primary expressions. It SHALL preserve the recognized modifier, one-quote or three-quote
delimiters, content, escapes, trivia, recovery elements, and exact spans without decoding storage
during syntax construction. Every complete literal form SHALL remain valid anywhere a primary
expression is accepted, including as the left operand of a pipeline.

#### Scenario: Recover a malformed escape

- **WHEN** a complete literal contains a malformed escape
- **THEN** the literal remains one lossless static-literal expression, damage remains local, and the following statement remains parseable

#### Scenario: Parse every literal width and category

- **WHEN** one body contains `"text"`, `b"bytes"`, `"""text"""`, and `b"""bytes"""`
- **THEN** the syntax tree contains four static-literal expressions whose tokens reproduce every modifier, delimiter, and content byte exactly

#### Scenario: Parse piped literal operands

- **WHEN** single-line and multiline text and byte literals each appear before `|>`
- **THEN** every pipeline retains the complete literal expression as its left operand and the complete callable expression as its right operand
