# Bootstrap Syntax Specification

## Purpose

Turn the bootstrap lexer result into the smallest source-faithful grammatical structure that can
recover from ordinary mistakes without introducing semantic or lowering representations.
## Requirements
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

### Requirement: Parse nested call arguments losslessly
The bootstrap parser SHALL accept a call expression wherever a call argument expression is allowed.
It SHALL preserve each nested call and argument list as its own concrete branch with every token,
separator, trivia slice, and owner-qualified half-open byte span retained exactly once. This grammar
extension MUST NOT imply that nested calls are already semantically resolved or evaluated.

#### Scenario: Parse one nested identity call
- **WHEN** a function returns `identity(identity(42))`
- **THEN** the outer argument contains a complete inner call-expression branch whose literal `42` and both parenthesis pairs retain exact source order and spans

#### Scenario: Parse two nested arguments
- **WHEN** a function returns `choose(identity(1), identity(2))`
- **THEN** both outer arguments contain independent nested call branches separated by the outer comma

#### Scenario: Recover a damaged inner call
- **WHEN** damaged inner syntax reaches an outer sibling boundary or an inner call lacks a closing parenthesis before the outer closing parenthesis
- **THEN** recovery records the inner error or missing token and keeps the outer argument boundary, following arguments, and enclosing call visible

#### Scenario: Preserve a following declaration after nested damage
- **WHEN** malformed nested call syntax is followed by another `pub fn` declaration
- **THEN** recovery remains bounded to the damaged function and the following declaration remains a separate complete concrete branch

### Requirement: Function-boundary recovery remains local
Recovery inside one function SHALL stop at a following function's `pub` token when that token can
begin the next declaration. Unexpected concrete input between declarations SHALL remain lossless,
and parsing SHALL either consume a concrete token or insert missing syntax without looping.

#### Scenario: Preserve a second function after a missing brace
- **WHEN** the first function omits its closing brace immediately before a valid second function
- **THEN** the first function receives a missing right brace and the second function remains a separate complete declaration

#### Scenario: Recover unexpected input between functions
- **WHEN** unsupported punctuation appears after one complete function and before the next `pub`
- **THEN** the punctuation is retained in an error region with a parser diagnostic and the following function remains parseable

#### Scenario: Preserve empty-input recovery
- **WHEN** the source is empty
- **THEN** the parser still returns one recovered missing function structure and end-of-file rather than treating an empty file as a valid program

### Requirement: Lossless concrete syntax tree

The concrete syntax tree SHALL retain every lexer token exactly once as a leaf in source order,
including whitespace, comments, invalid tokens, and end-of-file. Concrete nodes SHALL expose their
kind, ordered children, and source-owned half-open span. The tree SHALL distinguish the source file,
function declaration, parameter list, parameter declaration, return type, block, return statement,
integer literal expression, identifier expression, call expression, argument list, missing element,
and unexpected-token error region without claiming semantic meaning. The tree SHALL be owned by
the `SyntaxFile` artifact of its source module, and every node and token SHALL be addressable
there by a stable identity.

#### Scenario: Reconstruct parsed source

- **WHEN** all concrete token leaves except end-of-file are sliced from their owning source in tree order
- **THEN** their concatenated bytes reproduce the original source exactly

#### Scenario: Preserve invalid lexer data

- **WHEN** the lexer emits an invalid token inside a parameter or argument list
- **THEN** the concrete tree retains that exact token inside an error region and preserves its lexical diagnostic

#### Scenario: Address tree elements through the artifact

- **WHEN** a source module is parsed into its `SyntaxFile`
- **THEN** every tree node and token leaf resolves to a stable identity qualified by the source identity

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

### Requirement: Missing syntax remains explicit
When a required grammar element is absent at the current source position, the parser SHALL insert a
missing element with the expected token kind and an empty source-owned span at that byte boundary.
The parser SHALL emit a stable diagnostic for the missing element without consuming an unrelated
concrete token.

#### Scenario: Recover a missing function name
- **WHEN** the source spells `pub fn () -> I32 { return 42 }`
- **THEN** the function contains a missing identifier before `(`, the remaining structure parses, and one parser diagnostic identifies the missing name position

#### Scenario: Recover a missing closing brace
- **WHEN** the accepted fixture ends after the decimal integer
- **THEN** the block contains a missing right brace at end-of-file and the parser returns the partial tree with one parser diagnostic

### Requirement: Unexpected syntax remains explicit
When concrete tokens cannot satisfy the next grammar element, the parser SHALL consume a maximal
run of unexpected non-trivia tokens into one error region until it reaches the expected token, the
next structurally valid token, a closing brace, or end-of-file. Recovery SHALL always consume a
concrete token or insert a missing element, and SHALL retain every skipped token in the tree.

#### Scenario: Recover before a function name
- **WHEN** unsupported punctuation appears between `fn` and the function name
- **THEN** the punctuation is retained in one error region, the following identifier becomes the function name, and parsing continues through end-of-file

#### Scenario: Terminate on wholly unrelated input
- **WHEN** no input token begins the first function grammar
- **THEN** the parser returns a source-file tree containing the unexpected tokens, explicit missing structure, end-of-file, and a finite ordered diagnostic collection

### Requirement: Parser diagnostics are deterministic data

Parsing SHALL produce the `SyntaxFile` artifact, which retains the token stream and lexical
diagnostics and exposes parser diagnostics as a separate readonly collection. Every parser
diagnostic SHALL be a unified `Diagnostic` value whose originating phase is the parser, containing
a stable code, severity, concise message, structured reason data, and source-owned primary span.
Within the artifact, parser diagnostics SHALL be ordered by primary span and stable code, and
parsing SHALL return its artifact and diagnostics rather than throwing or failing an Effect for
source mistakes.

#### Scenario: Repeat malformed parsing

- **WHEN** equivalent malformed source files are lexed and parsed repeatedly in fresh processes
- **THEN** their tree kinds, ordered elements, spans, source slices, lexical diagnostics, and parser diagnostics are identical

#### Scenario: Parser diagnostics carry their phase

- **WHEN** a source produces any parser diagnostic
- **THEN** the diagnostic is a unified `Diagnostic` value identifying the parser as its originating phase

### Requirement: Minimal import declarations parse losslessly

The parser SHALL accept `import <module>` as an unconditional top-level declaration wherever a
function declaration may begin, where `<module>` is one identifier naming a logical module
identity. The spelling is deliberately provisional and owned by the syntax-prototype issue. The
import declaration SHALL retain its keyword, name, and adjacent trivia with exact source-owned
spans as its own concrete branch. A missing import name SHALL become an explicit missing token
with a parser diagnostic, and recovery SHALL keep following top-level declarations parseable.

#### Scenario: Parse an import before a function

- **WHEN** the source spells `import math` followed by a complete function declaration
- **THEN** the tree contains one import-declaration branch retaining the keyword and name, followed by the complete function branch

#### Scenario: Recover a missing import name

- **WHEN** the source spells `import` immediately followed by a function declaration
- **THEN** the import branch contains a missing identifier with one parser diagnostic and the following function remains a separate complete branch

#### Scenario: Parse multiple imports losslessly

- **WHEN** a source begins with two import declarations separated by trivia
- **THEN** both imports are separate concrete branches in source order and every token and trivia slice is retained exactly once

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

