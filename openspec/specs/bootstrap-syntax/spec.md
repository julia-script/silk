# Bootstrap Syntax Specification

## Purpose

Turn the bootstrap lexer result into the smallest source-faithful grammatical structure that can
recover from ordinary mistakes without introducing semantic or lowering representations.

## Requirements

### Requirement: First function grammar
The parser SHALL recognize a source file containing one or more public, parameterless function
declarations followed by end-of-file. Every function SHALL have the form `pub fn <name>() ->
<return-type> { return <expression> }`, where `<expression>` is either a decimal integer or a
zero-argument call spelled `<callee>()`. Lexer trivia SHALL be permitted between grammar elements
and declarations. Function names, return types, and call callees SHALL remain uninterpreted
identifier tokens, and declaration order SHALL match concrete source order.

#### Scenario: Parse the accepted integer fixture
- **WHEN** the source bytes spell `pub fn main() -> I32 { return 42 }`
- **THEN** the result contains one complete function declaration with an integer literal return expression and end-of-file

#### Scenario: Parse trivia between grammar elements
- **WHEN** whitespace and line comments appear between every pair of grammar elements in the accepted fixture
- **THEN** the parser recognizes the same grammatical structure while retaining the exact trivia tokens

#### Scenario: Parse two functions in source order
- **WHEN** `answer` returning `42` is followed by `main` returning `0`
- **THEN** the source-file tree contains exactly two complete function declarations in that order before end-of-file

#### Scenario: Parse the first function call
- **WHEN** `answer` returns `42` and `main` returns `answer()`
- **THEN** `main` contains one complete zero-argument call expression whose callee token spells `answer`

#### Scenario: Parse trivia inside a call
- **WHEN** whitespace and line comments appear between the call's callee and parentheses
- **THEN** the parser recognizes the same call structure while retaining every trivia token exactly

### Requirement: First call syntax remains explicit and recoverable
A call expression SHALL retain its callee identifier, left parenthesis, right parenthesis, trivia,
and exact source-owned span in concrete order. Missing call elements SHALL become explicit missing
tokens with parser diagnostics, and unexpected tokens inside the empty argument position SHALL
remain in an error region without being interpreted as arguments.

#### Scenario: Recover a missing right call parenthesis
- **WHEN** a returned call spells `answer(` immediately before the function's closing brace
- **THEN** the call contains a missing right parenthesis, the block retains its closing brace, and parsing completes

#### Scenario: Recover a missing call callee
- **WHEN** the returned expression consists only of `()`
- **THEN** the call contains a missing identifier and retains both parentheses without inventing a callee name

#### Scenario: Preserve unsupported call arguments
- **WHEN** the returned call spells `answer(42)`
- **THEN** `42` remains in an error region inside the call, a parser diagnostic is emitted, and no argument fact is invented

#### Scenario: Keep integer expressions unchanged
- **WHEN** a function returns a decimal integer
- **THEN** its existing integer-literal concrete shape and recovery behavior remain unchanged

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
function declaration, parameter list, return type, block, return statement, integer literal
expression, missing element, and unexpected-token error region without claiming semantic meaning.

#### Scenario: Reconstruct parsed source
- **WHEN** all concrete token leaves except end-of-file are sliced from their owning source in tree order
- **THEN** their concatenated bytes reproduce the original source exactly

#### Scenario: Preserve invalid lexer data
- **WHEN** the lexer emits an invalid token inside the function
- **THEN** the concrete tree retains that exact token inside an error region and preserves its lexical diagnostic

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
The parse result SHALL retain the lexical result and expose parser diagnostics as a separate
readonly collection. Every parser diagnostic SHALL contain a stable code, severity, concise
message, and source-owned primary span. Parser diagnostics SHALL be ordered by primary span and
stable code, and parsing SHALL return its tree and diagnostics rather than throwing or failing an
Effect for source mistakes.

#### Scenario: Repeat malformed parsing
- **WHEN** equivalent malformed source files are lexed and parsed repeatedly in fresh processes
- **THEN** their tree kinds, ordered elements, spans, source slices, lexical diagnostics, and parser diagnostics are identical
