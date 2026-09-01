## MODIFIED Requirements

### Requirement: First function grammar

The parser SHALL recognize a source file containing one or more public function declarations
followed by end-of-file. Every function SHALL have the form `pub fn <name>(<parameters>) ->
<return-type> { return <expression> }`. A parameter SHALL have the form `<name>: <type>` and
parameters SHALL be comma-separated. A return expression SHALL be a decimal integer, a bare
identifier, or a call. A call SHALL have the form `<callee>(<arguments>)`; arguments SHALL be
comma-separated decimal integers or bare identifiers. Empty parameter and argument lists SHALL
remain valid. Lexer trivia SHALL be permitted between grammar elements and declarations. Names and
types SHALL remain uninterpreted identifier tokens, and declaration, parameter, and argument order
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

### Requirement: First call syntax remains explicit and recoverable

A call expression SHALL retain its callee identifier, left parenthesis, ordered arguments,
separators, right parenthesis, trivia, and exact source-owned span in concrete order. Missing call
elements SHALL become explicit missing tokens with parser diagnostics. Unexpected tokens inside an
argument position SHALL remain in an error region without being interpreted as an argument, and
recovery SHALL resume at the next comma, right parenthesis, enclosing brace, following declaration,
or end-of-file.

#### Scenario: Recover a missing right call parenthesis

- **WHEN** a returned call spells `answer(` immediately before the function's closing brace
- **THEN** the call contains a missing right parenthesis, the block retains its closing brace, and parsing completes

#### Scenario: Recover a missing call callee

- **WHEN** the returned expression consists only of `()`
- **THEN** the call contains a missing identifier and retains both parentheses without inventing a callee name

#### Scenario: Preserve a supported call argument

- **WHEN** the returned call spells `identity(42)`
- **THEN** `42` is retained as the call's first decimal-integer argument rather than an error region

#### Scenario: Recover between call arguments

- **WHEN** unsupported punctuation appears between two otherwise valid arguments
- **THEN** the punctuation remains in an error region and the following comma-bounded argument remains parseable

#### Scenario: Keep integer expressions unchanged

- **WHEN** a function returns a decimal integer
- **THEN** its existing integer-literal concrete shape and recovery behavior remain unchanged

### Requirement: Lossless concrete syntax tree

The concrete syntax tree SHALL retain every lexer token exactly once as a leaf in source order,
including whitespace, comments, invalid tokens, and end-of-file. Concrete nodes SHALL expose their
kind, ordered children, and source-owned half-open span. The tree SHALL distinguish the source file,
function declaration, parameter list, parameter declaration, return type, block, return statement,
integer literal expression, identifier expression, call expression, argument list, missing element,
and unexpected-token error region without claiming semantic meaning.

#### Scenario: Reconstruct parsed source

- **WHEN** all concrete token leaves except end-of-file are sliced from their owning source in tree order
- **THEN** their concatenated bytes reproduce the original source exactly

#### Scenario: Preserve invalid lexer data

- **WHEN** the lexer emits an invalid token inside a parameter or argument list
- **THEN** the concrete tree retains that exact token inside an error region and preserves its lexical diagnostic

## ADDED Requirements

### Requirement: Typed parameter lists remain explicit and recoverable

Each parameter SHALL retain its name, colon, type identifier, adjacent trivia, and source-owned span.
Each comma SHALL remain a concrete separator rather than belonging to either neighboring parameter.
Missing names, colons, types, commas, and closing parentheses SHALL remain explicit parser-owned
recovery data, and a following return arrow or declaration SHALL bound recovery.

#### Scenario: Parse two typed parameters

- **WHEN** a function declares `(left: I32, right: I32)`
- **THEN** the parameter list contains two ordered parameter declarations and one concrete comma separator

#### Scenario: Recover a missing parameter type

- **WHEN** a parameter name and colon are followed immediately by the list's right parenthesis
- **THEN** the parameter retains a missing type identifier at that boundary and the function continues parsing at the return arrow

#### Scenario: Recover a missing comma

- **WHEN** two complete typed parameters are adjacent without a comma
- **THEN** the list contains an explicit missing comma and preserves both parameters in source order
