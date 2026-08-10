## MODIFIED Requirements

### Requirement: TextMate grammar covers the Silk lexical grammar

The package SHALL export a TextMate grammar for Silk (scope `source.silk`) that assigns scopes to
keywords (with differentiated control vs declaration families), line comments, doc comments,
decimal integer literals, single-line and multiline text and byte literals, function declaration
names, type-like identifiers, operators, and punctuation as defined by the compiler's token kinds
and the added scope requirements in this capability. Literal rules SHALL distinguish recognized
modifiers, opening and closing delimiters, bodies, and valid escapes, and multiline rules SHALL
continue across physical lines until the first unescaped matching delimiter.

#### Scenario: Keyword scoping

- **WHEN** the grammar tokenizes `pub fn main() -> i32 { return 42 }`
- **THEN** `pub` and `fn` receive declaration/storage keyword scopes, `return` receives a control keyword scope, `main` receives a function-name scope, `i32` receives a type scope, and `42` receives a numeric scope

#### Scenario: Doc comment scoping

- **WHEN** the grammar tokenizes a line starting with `///`
- **THEN** the line receives a documentation comment scope distinct from a `//` line comment scope

#### Scenario: Scope every committed literal form

- **WHEN** the grammar tokenizes `"text"`, `b"bytes"`, `"""text"""`, and `b"""bytes"""`
- **THEN** each literal receives a string scope while its recognized modifier, delimiters, and escapes receive stable nested scopes appropriate to their roles

#### Scenario: Keep comments inside multiline content

- **WHEN** a multiline literal body contains `//`, `///`, keywords, punctuation, and physical line endings
- **THEN** the complete body remains string content rather than being retokenized as Silk comments or code

## ADDED Requirements

### Requirement: Generated editor grammar preserves literal parity

The generated VS Code TextMate grammar SHALL contain the same ordered literal rules, scopes, and
delimiter behavior as the package grammar. Automated parity and tokenizer tests SHALL fail when a
single-line or multiline text/byte form, recognized modifier, delimiter, or escape is missing or
ordered so that a broader code/comment rule captures literal content first.

#### Scenario: Ship literal rules to VS Code

- **WHEN** the package grammar is synchronized into the VS Code extension
- **THEN** the checked-in grammar is structurally equal to the package grammar and highlights all four committed literal forms consistently

#### Scenario: Detect grammar drift

- **WHEN** either the package or generated grammar lacks one committed literal rule or nested scope
- **THEN** automated grammar parity or tokenizer coverage fails and identifies the unmatched behavior
