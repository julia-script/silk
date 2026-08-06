## MODIFIED Requirements

### Requirement: First function grammar

The parser SHALL recognize a source file containing one or more top-level import or function
declarations followed by end-of-file. Every function SHALL have the form `[pub] fn <name>(<parameters>)
-> <return-type> { <statements> }`, where `pub` is optional and `<statements>` is zero or more binding
statements followed by exactly one return statement. A binding statement SHALL have the form `let
<name> = <expression>`. A parameter SHALL have the form `<name>: <type>` and parameters SHALL be
comma-separated. An expression SHALL be a decimal integer with an optional directly applied `-`
sign, a bare identifier, a `move <name>` operand, or a call. A call SHALL have the form
`<callee>(<arguments>)`, where `<callee>` is one identifier or a qualified path
`<namespace>.<member>`; arguments SHALL be comma-separated expressions. Empty parameter and
argument lists SHALL remain valid. Lexer trivia SHALL be permitted between grammar elements,
statements, and declarations. Names, qualifiers, members, and types SHALL remain uninterpreted
identifier tokens, and declaration, statement, parameter, and argument order SHALL match concrete
source order.

#### Scenario: Parse the accepted integer fixture

- **WHEN** the source bytes spell `pub fn main() -> I32 { return 42 }`
- **THEN** the result contains one complete function declaration with an empty parameter list, an integer literal return expression, and end-of-file

#### Scenario: Parse a private function

- **WHEN** the source bytes spell `fn helper() -> I32 { return 42 }`
- **THEN** the result contains one complete function declaration with no public-modifier token and exact source provenance

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

### Requirement: Minimal import declarations parse losslessly

The parser SHALL accept the accepted unconditional top-level forms: `import <path>`, `import <path>
as <namespace>`, `import <path> { <members> }`, and `import <path> as <namespace> { <members> }`.
`<path>` SHALL contain one or more identifier segments separated by dots. `<members>` SHALL contain
one or more comma-separated identifiers, each optionally followed by `as <local-name>`. The import
declaration SHALL retain its keyword, ordered path segments and dots, optional namespace alias,
optional selected-member list with aliases and separators, adjacent trivia, and exact source-owned
span as one concrete branch. The concrete tree MUST NOT decide what any path, alias, or member
resolves to. Missing segments, aliases, members, separators, and closing braces SHALL become
explicit parser recovery data while following top-level declarations remain parseable.

#### Scenario: Parse a namespace import

- **WHEN** the source spells `import compiler.Syntax` before a complete function declaration
- **THEN** the import branch retains both path segments and their dot followed by the complete function branch

#### Scenario: Parse a changed namespace alias

- **WHEN** the source spells `import compiler.Syntax as Tree`
- **THEN** the import branch retains the `as` keyword and `Tree` alias after the complete path

#### Scenario: Parse selected members with an alias

- **WHEN** the source spells `import compiler.Syntax { Node, parse, encode as encodeSyntax }`
- **THEN** the import branch retains three ordered member entries, both commas, and the changed local alias without inventing a namespace binding

#### Scenario: Parse a hybrid import

- **WHEN** the source spells `import compiler.Syntax as Tree { Node, parse }`
- **THEN** one import branch retains the complete path, namespace alias, and both selected members in concrete order

#### Scenario: Recover a missing path segment

- **WHEN** the source spells `import compiler. as Tree` before a function declaration
- **THEN** the import path contains an explicit missing identifier after the dot and recovery retains the alias and following function

#### Scenario: Recover a missing alias

- **WHEN** the source spells `import compiler.Syntax as` before a following declaration
- **THEN** the import branch contains a missing alias identifier with one parser diagnostic and the following declaration remains separate

#### Scenario: Recover a damaged selected list

- **WHEN** a selected-member list has a missing member, comma, alias, or closing brace
- **THEN** the damaged element remains explicit and recovery resumes at the next member boundary, closing brace, or following top-level declaration

#### Scenario: Parse multiple imports losslessly

- **WHEN** a source begins with two import declarations separated by trivia
- **THEN** both imports are separate concrete branches in source order and every token and trivia slice is retained exactly once
