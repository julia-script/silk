# language-textmate Specification

## Purpose

Provides a Silk TextMate grammar and language configuration as importable package artifacts, so any
TextMate-based consumer (Shiki, VS Code, Cursor) highlights Silk consistently.

## Requirements

### Requirement: TextMate grammar covers the Silk lexical grammar

The package SHALL export a TextMate grammar for Silk (scope `source.silk`) that assigns scopes to
keywords, line comments, doc comments, decimal integer literals, operators, and punctuation as
defined by the compiler's token kinds.

#### Scenario: Keyword scoping

- **WHEN** the grammar tokenizes `pub fn main() -> I32 { return 42 }`
- **THEN** `pub`, `fn`, and `return` receive keyword scopes and `42` receives a numeric scope

#### Scenario: Doc comment scoping

- **WHEN** the grammar tokenizes a line starting with `///`
- **THEN** the line receives a documentation comment scope distinct from a `//` line comment scope

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
