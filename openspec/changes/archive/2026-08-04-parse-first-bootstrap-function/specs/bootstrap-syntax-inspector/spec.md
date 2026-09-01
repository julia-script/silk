## Purpose

Give compiler developers a small direct-link browser surface for seeing how the first Silk fixture
and nearby malformed inputs become tokens, concrete syntax, and diagnostics.

## ADDED Requirements

### Requirement: Direct-link-only syntax inspector

The docs site SHALL expose a Syntax Inspector at `/docs/labs/syntax-inspector` while omitting the
page from the normal docs navigation and package sidebars. The page SHALL identify its output as a
concrete syntax tree rather than a semantic AST and SHALL state that names and types are not yet
resolved.

#### Scenario: Open the hidden inspector directly

- **WHEN** a developer navigates directly to `/docs/labs/syntax-inspector`
- **THEN** the docs site renders the inspector without advertising it in normal navigation

### Requirement: Inspect the accepted fixture

The inspector SHALL start with `pub fn main() -> I32 { return 42 }` and SHALL display the concrete
tree hierarchy, every token kind, owner-qualified half-open byte span, exact source slice, lexical
diagnostics, and parser diagnostics produced for the current input.

#### Scenario: Inspect the initial program

- **WHEN** the inspector first loads
- **THEN** the accepted fixture has a complete function tree, exact token coverage, and no diagnostics

### Requirement: Explore nearby malformed text

The inspector SHALL let a developer edit the source text and recompute lexing and parsing locally
without a network request. The output SHALL remain renderable for empty input, missing syntax,
unexpected ASCII punctuation, and valid Unicode text whose UTF-8 bytes are unsupported by the
bootstrap vocabulary.

#### Scenario: Remove the closing brace

- **WHEN** a developer deletes the fixture's closing brace
- **THEN** the tree shows an empty-span missing element and the parser diagnostic identifies the end-of-file position

#### Scenario: Enter unsupported Unicode text

- **WHEN** a developer enters a non-ASCII character
- **THEN** the inspector displays its UTF-8 bytes as retained invalid token data and remains interactive

### Requirement: Inspector state is disposable

The inspector SHALL keep source and derived results only in browser memory. It MUST NOT write files,
persist source text, invoke a compiler service, or imply that the page is a supported language
playground.

#### Scenario: Reload the inspector

- **WHEN** a developer reloads the page after editing the source
- **THEN** the accepted fixture is restored and no previous input is recovered from storage
