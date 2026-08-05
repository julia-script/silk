# Bootstrap Syntax Inspector Specification

## Purpose

Give compiler developers a small direct-link browser surface for seeing how the first Silk fixture
and nearby malformed inputs become tokens, concrete syntax, and diagnostics.

## Requirements

### Requirement: Direct-link-only syntax inspector
The docs site SHALL expose a Syntax Inspector at `/docs/labs/syntax-inspector` while omitting the
page from the normal docs navigation and package sidebars. The page SHALL distinguish its lossless
concrete syntax tree from its semantic fact view and SHALL state that no semantic AST, HIR, or code
generation exists yet.

#### Scenario: Open the hidden inspector directly
- **WHEN** a developer navigates directly to `/docs/labs/syntax-inspector`
- **THEN** the docs site renders the syntax and semantic inspection views without advertising the page in normal navigation

### Requirement: Inspect the accepted fixture
The inspector SHALL start with `pub fn main() -> I32 { return 42 }` and SHALL display the concrete
tree hierarchy, every token kind, owner-qualified half-open byte span, exact source slice,
declaration facts, type facts, integer-value facts, return compatibility, and separate lexical,
parser, and semantic diagnostic collections produced for the current input.

#### Scenario: Inspect the initial program
- **WHEN** the inspector first loads
- **THEN** the accepted fixture has a complete function tree, exact token coverage, a public `main` declaration, resolved `I32` facts, exact value `42`, compatible return, and no diagnostics

### Requirement: Explore nearby malformed text
The inspector SHALL let a developer edit the source text and recompute lexing, parsing, and semantic
analysis locally without a network request. The output SHALL remain renderable for empty input,
missing syntax, unexpected ASCII punctuation, unknown return types, out-of-range integer literals,
and valid Unicode text whose UTF-8 bytes are unsupported by the bootstrap vocabulary.

#### Scenario: Remove the closing brace
- **WHEN** a developer deletes the fixture's closing brace
- **THEN** the tree shows an empty-span missing element, the parser diagnostic identifies the end-of-file position, and available semantic facts remain visible

#### Scenario: Enter unsupported Unicode text
- **WHEN** a developer enters a non-ASCII character
- **THEN** the inspector displays its UTF-8 bytes as retained invalid token data and remains interactive

#### Scenario: Enter an unknown return type
- **WHEN** a developer replaces `I32` with `Mystery`
- **THEN** the semantic view shows an unresolved return type, unavailable return compatibility, and its semantic diagnostic

#### Scenario: Enter an out-of-range integer
- **WHEN** a developer replaces `42` with `2147483648`
- **THEN** the semantic view shows an unavailable integer value and its semantic diagnostic

### Requirement: Inspector state is disposable
The inspector SHALL keep source and derived results only in browser memory. It MUST NOT write files,
persist source text, invoke a compiler service, or imply that the page is a supported language
playground.

#### Scenario: Reload the inspector
- **WHEN** a developer reloads the page after editing the source
- **THEN** the accepted fixture is restored and no previous input is recovered from storage

### Requirement: Inspect multiple concrete function branches
The Syntax Inspector SHALL provide a two-function preset and display each parsed function as a
separate top-level concrete branch in source order. Until declaration collection is implemented,
the semantic panel SHALL state that it describes only the first function and MUST NOT imply that
later declarations have semantic facts.

#### Scenario: Inspect two parsed functions
- **WHEN** a developer selects the two-function preset
- **THEN** the concrete tree shows two function-declaration branches while the semantic panel clearly identifies its first-function-only boundary

#### Scenario: Inspect recovery at a function boundary
- **WHEN** the first function in a two-function source is missing its closing brace
- **THEN** the tree keeps the missing brace in the first branch and the complete second branch visible

### Requirement: Inspect the declaration collection
The Syntax Inspector SHALL show one semantic function card per parsed declaration in concrete source
order. Each card SHALL display declaration identity, name state, return type, integer value, return
compatibility, and provenance, and the inspector SHALL provide a duplicate-name preset that displays
the ambiguous lookup state and its semantic diagnostic.

#### Scenario: Inspect two collected declarations
- **WHEN** a developer selects the two-function preset
- **THEN** the semantic view shows ordered `answer` and `main` cards whose ordinals and spans match their concrete branches

#### Scenario: Inspect a duplicate declaration name
- **WHEN** a developer selects the duplicate-name preset
- **THEN** both declarations remain visible, name lookup is shown as ambiguous, and `SEM0003` identifies the later name

### Requirement: Inspect the first call expression
The Syntax Inspector SHALL provide valid-call, missing-call-syntax, and unsupported-argument presets.
It SHALL show the call's concrete subtree, exact token slices and spans, unresolved semantic fact,
unavailable compatibility, and separate parser and semantic diagnostic collections.

#### Scenario: Inspect a valid unresolved call
- **WHEN** a developer selects the valid-call preset
- **THEN** the concrete view shows `answer()` as a call expression and the semantic view labels its callee unresolved without displaying an unknown-name diagnostic

#### Scenario: Inspect damaged call syntax
- **WHEN** a developer selects a missing-parenthesis or unsupported-argument preset
- **THEN** explicit missing or error syntax stays visible beside the unavailable call facts and parser-owned diagnostics
