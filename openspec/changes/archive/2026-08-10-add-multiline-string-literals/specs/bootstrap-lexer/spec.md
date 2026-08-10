## ADDED Requirements

### Requirement: String literal introductions are closed and extensible

The lexer SHALL recognize unmodified and `b`-modified string literal introductions with either one
quote or three quotes as one deterministic literal token. The modifier SHALL be adjacent to the
opening delimiter, delimiter recognition SHALL prefer three quotes over one quote, and the token
kind SHALL retain whether the literal denotes text or bytes without requiring a distinct kind for
every delimiter width. An identifier-like spelling immediately adjacent to a quote delimiter that
is not in the closed modifier vocabulary SHALL be reserved as an invalid literal introduction and
SHALL produce a lexical diagnostic rather than tokenize as an identifier followed by a literal.

#### Scenario: Recognize every committed literal introduction

- **WHEN** source contains `"text"`, `b"bytes"`, `"""text"""`, and `b"""bytes"""`
- **THEN** the lexer emits four literal tokens retaining their text-or-byte category, delimiter width, exact source span, and source slice

#### Scenario: Prefer the multiline delimiter

- **WHEN** the next source bytes begin with `"""`
- **THEN** longest recognition begins one triple-delimited literal rather than a sequence of empty or adjacent single-line literals

#### Scenario: Reserve an unknown modifier

- **WHEN** source contains `future"value"` and `future"""value"""`
- **THEN** each adjacent `future` spelling is retained with its literal as one invalid introduction and produces a stable lexical diagnostic naming the unknown modifier

### Requirement: String literal boundaries recover deterministically

An escaped single-line literal SHALL close at the first unescaped quote and SHALL otherwise stop
immediately before a physical CR or LF. An escaped multiline literal SHALL close at the first
unescaped run of three quotes and MAY contain physical line endings; if no closing delimiter
exists, it SHALL consume through end-of-file. Each unterminated literal SHALL produce exactly one
lexical diagnostic anchored to its introduction. Recovery MUST NOT infer a closing boundary from
indentation, keywords, declarations, braces, comments, or other code-like content inside a
multiline literal.

#### Scenario: Recover an unterminated single-line literal

- **WHEN** an opening single quote is followed by content and a line ending without a closing quote
- **THEN** the literal token stops before the line ending, one lexical diagnostic identifies the unterminated literal, and lexing resumes with the line ending and following source

#### Scenario: Consume an unterminated multiline literal

- **WHEN** a triple-delimited literal has no unescaped closing delimiter before end-of-file
- **THEN** one literal token covers its introduction through end-of-file and exactly one lexical diagnostic is produced without interpreting apparent declarations inside its body

#### Scenario: Ignore escaped quotes when finding the boundary

- **WHEN** a multiline literal body contains `\"\"\"` followed later by an unescaped `"""`
- **THEN** the escaped quotes remain literal content and the later unescaped triple quote closes the token
