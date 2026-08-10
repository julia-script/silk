# language-textmate Specification

## Purpose

Provides a Silk TextMate grammar and language configuration as importable package artifacts, so any
TextMate-based consumer (Shiki, VS Code, Cursor) highlights Silk consistently.

## Requirements

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

### Requirement: Keyword coverage is verified against the compiler

An automated test SHALL derive the complete keyword list from the compiler's token kind definitions
and fail when the TextMate grammar's keyword set differs from it in either direction.

#### Scenario: Compiler gains a keyword the grammar lacks

- **WHEN** the compiler defines a keyword token kind whose keyword is absent from the grammar
- **THEN** the test fails naming the missing keyword

#### Scenario: Grammar lists a keyword the compiler lacks

- **WHEN** the grammar lists a keyword with no corresponding compiler token kind
- **THEN** the test fails naming the extra keyword

### Requirement: Language configuration for editors

The package SHALL export a language configuration declaring Silk's line comment (`//`), bracket
pairs (`()`, `{}`), and auto-closing pairs, suitable for VS Code-compatible editors.

#### Scenario: Editor consumes the configuration

- **WHEN** a VS Code-compatible editor loads the language configuration for a `.silk` file
- **THEN** toggle-comment inserts `//` and typing `(` auto-closes with `)`

### Requirement: TextMate grammars cover exhaustive-match syntax

The Silk TextMate grammar and generated VS Code grammar SHALL assign consistent scopes to the
`match` keyword, access-mode punctuation, nominal and universal patterns, guards, fat arrows,
omission markers, bindings, and arm expressions. Keyword parity tests SHALL continue to compare the
grammar vocabulary with compiler token definitions.

#### Scenario: Scope one consuming match

- **WHEN** a TextMate consumer tokenizes a consuming two-arm nominal match
- **THEN** `match`, `move`, type names, bindings, `..`, `=>`, and `_` receive stable appropriate scopes without changing pipeline or union-token recognition

### Requirement: TextMate tokenization covers generic contexts

TextMate and generated VS Code grammars SHALL tokenize generic declaration parameters and
applications without reclassifying comparison operators or reserved template starts.

#### Scenario: Tokenize mixed angle contexts
- **WHEN** a document contains a generic declaration, an explicit specialization, a comparison, and a reserved template start
- **THEN** generated grammar fixtures assign stable context-appropriate scopes to all four forms

### Requirement: Keywords use theme-friendly differentiated scopes

The TextMate grammar SHALL assign distinct scope families for control-flow keywords,
declaration/storage keywords, and boolean literals, rather than placing every non-boolean keyword
under a single undifferentiated keyword scope.

#### Scenario: Control versus declaration coloring

- **WHEN** a TextMate consumer tokenizes `pub fn main() { if true { return 1 } else { return 0 } }`
- **THEN** `if`, `else`, and `return` receive control-oriented keyword scopes while `pub` and `fn`
  receive declaration/storage-oriented scopes distinct from the control scopes, and `true` retains
  a boolean literal scope

### Requirement: Function names after fn receive entity scopes

The TextMate grammar SHALL scope the identifier immediately following `fn` as a function name so
themes can color declarations differently from ordinary identifiers.

#### Scenario: Function declaration name

- **WHEN** a TextMate consumer tokenizes `fn greet()`
- **THEN** `greet` receives a function-name entity scope and `fn` retains its declaration/storage
  keyword scope

### Requirement: Type-like identifiers receive type scopes beyond builtins and patterns

The TextMate grammar SHALL assign type-oriented scopes to PascalCase type identifiers used in
ordinary type positions (not only builtin names and nominal patterns before `{`), so user-defined
types are highlighted consistently with builtins where themes map type scopes.

#### Scenario: User type in a signature

- **WHEN** a TextMate consumer tokenizes `fn id(x: Point) -> Point`
- **THEN** both `Point` occurrences receive a type scope

### Requirement: TextMate scopes documentation forms and nested structure

The TextMate grammar SHALL assign distinct documentation scopes to `///` declaration comments and
`//!` module comments while keeping `//` ordinary comments distinct. Within documentation comments,
the grammar SHALL expose useful nested scopes for Markdown headings, emphasis, inline code,
intra-document links, fences, and Silk fenced content without changing compiler semantics.

#### Scenario: Scope declaration and module documentation

- **WHEN** a TextMate consumer tokenizes adjacent `//! module`, `/// declaration`, and `// ordinary` lines
- **THEN** all three forms receive distinct stable comment scopes

#### Scenario: Scope nested documentation markup

- **WHEN** a documentation block contains an `Examples` heading, ``[`Problem`]``, and a fenced `silk` block
- **THEN** the heading, link, fence, and nested Silk content receive theme-friendly scopes within the documentation comment

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
