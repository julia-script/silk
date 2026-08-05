## MODIFIED Requirements

### Requirement: First function grammar
The parser SHALL recognize a source file containing one or more public function declarations
followed by end-of-file. Every function SHALL have the form `pub fn <name>(<parameters>) ->
<return-type> { <statements> }`, where `<statements>` is zero or more binding statements followed
by exactly one return statement. A binding statement SHALL have the form `let <name> =
<expression>`. A parameter SHALL have the form `<name>: <type>` and parameters SHALL be
comma-separated. An expression SHALL be a decimal integer with an optional directly applied `-`
sign, a bare identifier, a `move <name>` operand, or a call. A call SHALL have the form
`<callee>(<arguments>)`, where `<callee>` is one identifier or a qualified actor path
`<actor>.<operation>`; arguments SHALL be comma-separated expressions. Empty parameter and
argument lists SHALL remain valid. Lexer trivia SHALL be permitted between grammar elements,
statements, and declarations. Names and types SHALL remain uninterpreted identifier tokens, and
declaration, statement, parameter, and argument order SHALL match concrete source order.

#### Scenario: Parse the accepted integer fixture
- **WHEN** the source bytes spell `pub fn main() -> I32 { return 42 }`
- **THEN** the result contains one complete function declaration with an empty parameter list, an integer literal return expression, and end-of-file

#### Scenario: Parse trivia between grammar elements
- **WHEN** whitespace and line comments appear between every pair of grammar elements in the accepted fixture
- **THEN** the parser recognizes the same grammatical structure while retaining the exact trivia tokens

#### Scenario: Parse two functions in source order
- **WHEN** `answer` returning `42` is followed by `main` returning `0`
- **THEN** the source-file tree contains exactly two complete function declarations in that order before end-of-file

#### Scenario: Parse a typed parameter reference
- **WHEN** the source spells `pub fn identity(value: I32) -> I32 { return value }`
- **THEN** the function contains one typed parameter and a bare-identifier return expression with exact concrete provenance

#### Scenario: Parse the first value-carrying call
- **WHEN** `identity` accepts one `I32` parameter and `main` returns `identity(42)`
- **THEN** `main` contains one complete call expression with one decimal-integer argument

#### Scenario: Parse trivia inside a call
- **WHEN** whitespace and line comments appear around a call's callee, parentheses, arguments, and commas
- **THEN** the parser recognizes the same call structure while retaining every trivia token exactly

#### Scenario: Parse a binding sequence

- **WHEN** the source spells `pub fn main() -> I32 { let value = 42 return value }`
- **THEN** the block contains one binding statement retaining its keyword, name, equals, and initializer expression, followed by one return statement, in source order

#### Scenario: Parse a move operand

- **WHEN** a binding initializer or call argument spells `move value`
- **THEN** the expression is a move operand retaining the keyword and the moved name with exact spans

#### Scenario: Parse a signed literal

- **WHEN** a return expression or call argument spells `-42`
- **THEN** the integer literal expression retains the minus token and the decimal token as one concrete branch with exact spans

#### Scenario: Parse a qualified callee

- **WHEN** a body spells `I32.add(1, 2)`
- **THEN** the call expression retains the actor identifier, the dot, the operation identifier, and the argument list in concrete order

## ADDED Requirements

### Requirement: Qualified callees and signed literals remain recoverable

A qualified callee SHALL retain its actor identifier, dot, and operation identifier with exact
spans; a dot with a missing operation identifier SHALL become an explicit missing token with a
parser diagnostic while the argument list keeps parsing. A minus token not directly applicable to
a decimal literal in expression position SHALL remain recoverable: the minus stays in the tree
and the expression recovers at the existing statement and argument boundaries.

#### Scenario: Recover a missing operation name

- **WHEN** a body spells `I32.(1, 2)`
- **THEN** the callee retains the actor identifier and dot with an explicit missing identifier and one parser diagnostic, and both arguments remain parseable

#### Scenario: Recover a dangling minus

- **WHEN** a return expression spells `-` immediately before the closing brace
- **THEN** the minus token stays in the tree with an explicit missing decimal literal and one parser diagnostic
