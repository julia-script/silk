## Purpose

Turn the bootstrap lexer result into the smallest source-faithful grammatical structure that can
recover from ordinary mistakes without introducing semantic or lowering representations.

## ADDED Requirements

### Requirement: First function grammar
The parser SHALL recognize exactly one source file containing a public, parameterless function
declaration followed by end-of-file. The function SHALL have the form `pub fn <name>() ->
<return-type> { return <decimal-integer> }`, with lexer trivia permitted between grammar elements.
The function name and return type SHALL remain uninterpreted identifier tokens.

#### Scenario: Parse the accepted fixture
- **WHEN** the source bytes spell `pub fn main() -> I32 { return 42 }`
- **THEN** the result contains one complete function declaration with an empty parameter list, named return type, block, return statement, integer literal expression, and end-of-file

#### Scenario: Parse trivia between grammar elements
- **WHEN** whitespace and line comments appear between every pair of grammar elements in the accepted fixture
- **THEN** the parser recognizes the same grammatical structure while retaining the exact trivia tokens

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
run of unexpected non-trivia tokens into one error region until it reaches the expected token, a
closing brace, or end-of-file. Recovery SHALL always consume a concrete token or insert a missing
element, and SHALL retain every skipped token in the tree.

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
