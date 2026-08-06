## ADDED Requirements

### Requirement: Fixed-array type syntax is lossless and recoverable

The parser SHALL recognize `Array<T, N>` wherever an explicit type is accepted, retaining the
`Array` token, angle brackets, nested element-type syntax, comma, non-negative decimal length,
trivia, missing elements, and exact spans. Recovery SHALL remain inside the type constructor.

#### Scenario: Parse a nested array type

- **WHEN** a field declares `cells: Array<Array<I32, 4>, 3>`
- **THEN** the concrete tree retains both nested constructors and every punctuation token byte-for-byte

#### Scenario: Recover a missing array length

- **WHEN** a declaration contains `Array<I32, >`
- **THEN** the tree records missing length syntax without consuming the following declaration

### Requirement: Array literals parse losslessly

The parser SHALL recognize empty and comma-separated array literals in every expression position,
including nested literals and an optional trailing comma. It SHALL retain element source order,
punctuation, trivia, recovery nodes, and exact spans.

#### Scenario: Parse a nested literal

- **WHEN** source contains `[[1, 2], [3, 4]]`
- **THEN** the concrete tree contains two nested element lists in exact source order

### Requirement: Indexing is a repeated postfix expression

Indexing SHALL parse as a repeated postfix operation that binds more tightly than prefix, infix,
equality, and pipeline expressions and composes left-to-right with field projection and calls.
Recovery for a missing subject, bracket, index expression, or closing bracket SHALL stay within the
containing expression.

#### Scenario: Parse a mixed place chain

- **WHEN** source contains `matrix[row][column].value`
- **THEN** the tree nests two index projections and one field projection from left to right

#### Scenario: Recover a missing closing bracket

- **WHEN** source contains `values[index` before a return boundary
- **THEN** the index node records the missing bracket without consuming the following statement
