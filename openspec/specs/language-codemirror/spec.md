# language-codemirror Specification

## Purpose

Provides a CodeMirror 6 extension that syntax-highlights Silk source by classifying text with the
compiler's own bootstrap lexer, so editor highlighting can never drift from the language.

## Requirements

### Requirement: Highlighting reflects the compiler's token classification

The extension SHALL derive every highlight from the token stream produced by the compiler's
bootstrap lexer for the current editor document. Each keyword, comment, doc comment, number,
identifier, operator, and punctuation token SHALL receive a style class determined solely by its
compiler-reported token kind.

#### Scenario: Keywords are highlighted

- **WHEN** the editor contains `pub fn main() -> i32 { return 42 }`
- **THEN** `pub`, `fn`, and `return` carry the keyword style, `42` carries the number style, and
  `main` and `i32` carry the identifier style

#### Scenario: Comments and doc comments are distinct

- **WHEN** the editor contains a `//` line comment and a `///` doc comment
- **THEN** the doc comment carries a style distinct from the line comment style

### Requirement: Invalid tokens are visibly marked

Bytes the lexer classifies as invalid SHALL receive a distinct error style so users see what the
compiler will reject.

#### Scenario: Invalid character

- **WHEN** the editor contains a character outside the Silk lexical grammar (e.g. `@`)
- **THEN** that character carries the invalid-token style

### Requirement: Highlighting stays current across edits

The extension SHALL re-derive highlighting from a fresh lex of the document after every document
change.

#### Scenario: Typing a keyword

- **WHEN** the user types `le` and then `t` so the word becomes `let`
- **THEN** after the final keystroke the word carries the keyword style

### Requirement: Non-ASCII content does not corrupt highlight positions

The lexer classifies UTF-8 bytes while the editor addresses UTF-16 code units. The extension SHALL
place every highlight at the correct editor positions even when the document contains characters
that are not single-byte UTF-8.

#### Scenario: Multi-byte character before a keyword

- **WHEN** the document contains a multi-byte character (e.g. `é` in a comment) followed on a later
  line by `fn`
- **THEN** the `fn` keyword highlight covers exactly the two characters `fn` and nothing adjacent

### Requirement: CodeMirror highlights the match surface from compiler tokens

The CodeMirror extension SHALL style `match`, `move`, `mut`, and guard `if` by their compiler keyword
kinds and SHALL style `&`, `=>`, `..`, braces, and other pattern punctuation from their compiler
token kinds. Nominal pattern names and field bindings SHALL retain identifier/type classification,
and `_` SHALL remain visibly distinct only through its compiler-supported pattern context when that
context is available without reimplementing matching semantics.

#### Scenario: Highlight a guarded borrowed match

- **WHEN** the editor contains a shared match with a nominal guarded arm and `_` fallback
- **THEN** every keyword, operator, punctuation, type, binding, and literal receives the compiler-consistent highlight range

### Requirement: CodeMirror distinguishes generic angles contextually

CodeMirror highlighting SHALL classify type parameters and generic applications consistently with
the accepted syntax while retaining ordinary comparison and reserved-template highlighting in
their respective contexts.

#### Scenario: Highlight generic call and comparison
- **WHEN** one source contains `identity<i32>(value)` and `left < right`
- **THEN** the generic arguments and comparison operator receive their respective canonical styles
