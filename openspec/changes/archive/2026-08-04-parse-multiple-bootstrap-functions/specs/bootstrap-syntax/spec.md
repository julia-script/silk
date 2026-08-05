## MODIFIED Requirements

### Requirement: First function grammar
The parser SHALL recognize a source file containing one or more public, parameterless function
declarations followed by end-of-file. Every function SHALL have the form `pub fn <name>() ->
<return-type> { return <decimal-integer> }`, with lexer trivia permitted between grammar elements
and between declarations. Function names and return types SHALL remain uninterpreted identifier
tokens, and declaration order SHALL match concrete source order.

#### Scenario: Parse the accepted fixture
- **WHEN** the source bytes spell `pub fn main() -> I32 { return 42 }`
- **THEN** the result contains one complete function declaration with an empty parameter list, named return type, block, return statement, integer literal expression, and end-of-file

#### Scenario: Parse trivia between grammar elements
- **WHEN** whitespace and line comments appear between every pair of grammar elements in the accepted fixture
- **THEN** the parser recognizes the same grammatical structure while retaining the exact trivia tokens

#### Scenario: Parse two functions in source order
- **WHEN** `answer` returning `42` is followed by `main` returning `0`
- **THEN** the source-file tree contains exactly two complete function declarations in that order before end-of-file

## ADDED Requirements

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
