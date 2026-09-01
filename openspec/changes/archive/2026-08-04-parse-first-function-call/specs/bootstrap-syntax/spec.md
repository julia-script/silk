## MODIFIED Requirements

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

#### Scenario: Parse two functions in source order

- **WHEN** `answer` returning `42` is followed by `main` returning `0`
- **THEN** the source-file tree contains exactly two complete function declarations in that order before end-of-file

#### Scenario: Parse the first function call

- **WHEN** `answer` returns `42` and `main` returns `answer()`
- **THEN** `main` contains one complete zero-argument call expression whose callee token spells `answer`

#### Scenario: Parse trivia inside a call

- **WHEN** whitespace and line comments appear between the call's callee and parentheses
- **THEN** the parser recognizes the same call structure while retaining every trivia token exactly

## ADDED Requirements

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
