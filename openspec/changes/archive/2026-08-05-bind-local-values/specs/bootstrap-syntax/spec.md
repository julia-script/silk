## MODIFIED Requirements

### Requirement: First function grammar

The parser SHALL recognize a source file containing one or more public function declarations
followed by end-of-file. Every function SHALL have the form `pub fn <name>(<parameters>) ->
<return-type> { <statements> }`, where `<statements>` is zero or more binding statements followed
by exactly one return statement. A binding statement SHALL have the form `let <name> =
<expression>`. A parameter SHALL have the form `<name>: <type>` and parameters SHALL be
comma-separated. An expression SHALL be a decimal integer, a bare identifier, a `move <name>`
operand, or a call. A call SHALL have the form `<callee>(<arguments>)`; arguments SHALL be
comma-separated expressions. Empty parameter and argument lists SHALL remain valid. Lexer trivia
SHALL be permitted between grammar elements, statements, and declarations. Names and types SHALL
remain uninterpreted identifier tokens, and declaration, statement, parameter, and argument order
SHALL match concrete source order.

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

## ADDED Requirements

### Requirement: Binding statements remain explicit and recoverable

Each binding statement SHALL retain its `let` keyword, bound name, equals token, initializer
expression, adjacent trivia, and source-owned span as its own concrete branch. Missing names,
equals tokens, and initializer expressions SHALL become explicit missing elements with parser
diagnostics, and recovery SHALL resume at the next `let`, `return`, closing brace, following
declaration, or end-of-file. A `move` operand missing its name SHALL retain the keyword with an
explicit missing identifier. Statements after the return statement and blocks without a return
statement SHALL remain recoverable: extra statements stay in the tree as concrete branches, and a
missing return statement becomes an explicit recovered return structure with parser diagnostics.

#### Scenario: Recover a missing initializer

- **WHEN** a block spells `let value = return 42`
- **THEN** the binding retains a missing initializer expression at the `return` boundary with one parser diagnostic, and the return statement parses completely

#### Scenario: Recover a missing binding name

- **WHEN** a block spells `let = 42 return 0`
- **THEN** the binding contains a missing identifier before the equals token and both statements remain separate concrete branches

#### Scenario: Recover a missing return statement

- **WHEN** a block contains only binding statements before its closing brace
- **THEN** the block retains every binding followed by a recovered return structure whose missing elements carry parser diagnostics

#### Scenario: Recover a bare move

- **WHEN** an initializer spells `move` immediately before the statement boundary
- **THEN** the move operand retains its keyword and a missing identifier with one parser diagnostic
