## MODIFIED Requirements

### Requirement: Highlighting reflects the compiler's token classification

The extension SHALL derive every highlight from the token stream produced by the compiler's
bootstrap lexer for the current editor document. Each keyword, comment, doc comment, number,
identifier, text literal, byte literal, operator, and punctuation token SHALL receive a style class
determined solely by its compiler-reported token kind. Literal highlights SHALL cover complete
single-line and multiline tokens across editor lines, including their modifier and delimiters,
without independently reimplementing literal boundary rules.

#### Scenario: Keywords are highlighted

- **WHEN** the editor contains `pub fn main() -> i32 { return 42 }`
- **THEN** `pub`, `fn`, and `return` carry the keyword style, `42` carries the number style, and `main` and `i32` carry the identifier style

#### Scenario: Comments and doc comments are distinct

- **WHEN** the editor contains a `//` line comment and a `///` doc comment
- **THEN** the doc comment carries a style distinct from the line comment style

#### Scenario: Literal widths and categories are highlighted

- **WHEN** the editor contains single-line and multiline text and byte literals
- **THEN** every complete literal range carries the compiler-consistent string style from its modifier through its closing delimiter, including embedded line endings

#### Scenario: Multibyte content preserves following offsets

- **WHEN** a multiline text literal contains non-ASCII content followed by another highlighted token
- **THEN** both the literal and following token cover their exact UTF-16 editor ranges derived from compiler byte spans
