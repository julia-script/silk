## MODIFIED Requirements

### Requirement: Fixed-array type syntax is lossless and recoverable

The parser SHALL recognize `[T; N]` wherever an explicit type is accepted, retaining the left
bracket, nested element-type syntax, semicolon, non-negative decimal length, right bracket, trivia,
missing elements, and exact spans. Recovery SHALL remain inside the type constructor. The former
`Array<T, N>` spelling SHALL NOT remain as a compatible fixed-array source form.

#### Scenario: Parse a nested array type

- **WHEN** a field declares `cells: [[I32; 4]; 3]`
- **THEN** the concrete tree retains both nested constructors and every punctuation token byte-for-byte

#### Scenario: Recover a missing array length

- **WHEN** a declaration contains `[I32; ]`
- **THEN** the tree records missing length syntax without consuming the following declaration

#### Scenario: Reject the former array spelling

- **WHEN** an explicit type position contains `Array<I32, 4>`
- **THEN** the parser does not produce a fixed-array type or silently translate the former spelling

## ADDED Requirements

### Requirement: JSX-like template starts are reserved at primary-expression boundaries

When a primary expression is required, `<` immediately followed by a tag identifier or `>` SHALL be
reserved for a future built-in template expression. Until template expressions are implemented,
the parser SHALL reject that reserved syntax with a parser-owned diagnostic, SHALL NOT reinterpret
it as prefix type syntax or an infix relational expression, and SHALL recover without consuming a
following statement or declaration. A relational token following an already-started expression
SHALL retain its existing operator meaning.

#### Scenario: Reserve an element start

- **WHEN** a return expression starts with `<Button />` before a following statement
- **THEN** the parser reports reserved template syntax and the following statement parses independently

#### Scenario: Reserve a fragment start

- **WHEN** a return expression starts with `<>` before a following statement
- **THEN** the parser reports reserved template syntax and the following statement parses independently

#### Scenario: Preserve relational less-than

- **WHEN** an expression spells `left < right`
- **THEN** `<` remains the non-associative relational operator between the two expressions
