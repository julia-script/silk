## MODIFIED Requirements

### Requirement: First function grammar
The parser SHALL recognize a source file containing zero or more top-level import or function
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

#### Scenario: Parse an empty module
- **WHEN** the source contains only end-of-file
- **THEN** the result contains a source-file root with end-of-file, no recovered declaration, and no parser diagnostic

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

### Requirement: Function-boundary recovery remains local
Recovery inside one function SHALL stop at a following function's `pub` token when that token can
begin the next declaration. Unexpected concrete input between declarations SHALL remain lossless,
and parsing SHALL either consume a concrete token or insert missing syntax without looping. An
empty module SHALL terminate directly at end-of-file without entering declaration recovery. After
the first diagnostic in one recovery episode, dependent missing or damaged syntax SHALL remain
explicit in the tree without producing further diagnostics until the parser consumes a concrete
token expected by the grammar. At that synchronization token, ordinary diagnostic reporting SHALL
resume for later independent mistakes.

#### Scenario: Preserve a second function after a missing brace
- **WHEN** the first function omits its closing brace immediately before a valid second function
- **THEN** the first function receives a missing right brace and the second function remains a separate complete declaration

#### Scenario: Recover unexpected input between functions
- **WHEN** unsupported punctuation appears after one complete function and before the next `pub`
- **THEN** the punctuation is retained in an error region with a parser diagnostic and the following function remains parseable

#### Scenario: Preserve empty module structure
- **WHEN** the source is empty
- **THEN** the parser returns only the source-file root and end-of-file without missing syntax or parser diagnostics

#### Scenario: Stop an end-of-file declaration cascade
- **WHEN** the source contains only `pub`
- **THEN** the parser reports the missing `fn`, retains the remaining recovered function structure, and suppresses its dependent missing-token and missing-return diagnostics through end-of-file

#### Scenario: Report again after a grammar anchor
- **WHEN** one missing token starts recovery, a later expected concrete token is consumed, and another independent token is missing after that anchor
- **THEN** the parser reports both independent mistakes without reporting dependent insertions between them

### Requirement: Missing syntax remains explicit
When a required grammar element is absent at the current source position, the parser SHALL insert a
missing element with the expected token kind and an empty source-owned span at that byte boundary.
The parser SHALL emit a stable diagnostic for the source-level mistake without consuming an
unrelated concrete token. Multiple missing elements introduced solely to represent one absent
construct MAY share one construct-level diagnostic rather than producing one diagnostic per leaf.

#### Scenario: Recover a missing function name
- **WHEN** the source spells `pub fn () -> I32 { return 42 }`
- **THEN** the function contains a missing identifier before `(`, the remaining structure parses, and one parser diagnostic identifies the missing name position

#### Scenario: Recover a missing closing brace
- **WHEN** the accepted fixture ends after the decimal integer
- **THEN** the block contains a missing right brace at end-of-file and the parser returns the partial tree with one parser diagnostic

#### Scenario: Aggregate a wholly missing return statement
- **WHEN** a required-return block reaches its closing brace without a return keyword or expression
- **THEN** the CST retains the recovered return structure and one parser diagnostic identifies the missing return statement

### Requirement: Binding statements remain explicit and recoverable

Each binding statement SHALL retain its `let` keyword, bound name, equals token, initializer
expression, adjacent trivia, and source-owned span as its own concrete branch. Missing names,
equals tokens, and initializer expressions SHALL become explicit missing elements with parser
diagnostics, and recovery SHALL resume at the next `let`, `return`, closing brace, following
declaration, or end-of-file. A `move` operand missing its name SHALL retain the keyword with an
explicit missing identifier. Statements after the return statement and blocks without a return
statement SHALL remain recoverable: extra statements stay in the tree as concrete branches, and a
missing return statement becomes an explicit recovered return structure covered by one
construct-level parser diagnostic.

#### Scenario: Recover a missing initializer

- **WHEN** a block spells `let value = return 42`
- **THEN** the binding retains a missing initializer expression at the `return` boundary with one parser diagnostic, and the return statement parses completely

#### Scenario: Recover a missing binding name

- **WHEN** a block spells `let = 42 return 0`
- **THEN** the binding contains a missing identifier before the equals token and both statements remain separate concrete branches

#### Scenario: Recover a missing return statement

- **WHEN** a block contains only binding statements before its closing brace
- **THEN** the block retains every binding followed by a recovered return structure with one parser diagnostic for the missing statement

#### Scenario: Recover a bare move

- **WHEN** an initializer spells `move` immediately before the statement boundary
- **THEN** the move operand retains its keyword and a missing identifier with one parser diagnostic

### Requirement: Mutable bindings and assignments parse losslessly

The parser SHALL recognize `let mut name = expression` and statement-form `place = expression`,
where a place is a binding followed by zero or more field or index projections. An identifier-led
construct SHALL be classified as an assignment only when the complete place is followed by `=`.
The concrete tree SHALL retain the `mut` token, assignment token, complete place syntax,
expression, trivia, and exact spans. Assignment SHALL remain distinct from equality and SHALL NOT
be an expression.

#### Scenario: Parse an indexed field update

- **WHEN** source contains `pairs[index].left = value`
- **THEN** the tree retains the binding root, index projection, field projection, assignment token, and right-hand expression in source order

#### Scenario: Distinguish assignment from equality

- **WHEN** a loop body contains both `current = next` and `current == next`
- **THEN** the first is an assignment statement and the second remains an equality expression

#### Scenario: Recover a final identifier as a missing-keyword return

- **WHEN** a required-return block ends with `foo` immediately before its closing brace
- **THEN** `foo` is the recovered return expression, one parser diagnostic identifies the missing `return` keyword, and no assignment syntax is synthesized
