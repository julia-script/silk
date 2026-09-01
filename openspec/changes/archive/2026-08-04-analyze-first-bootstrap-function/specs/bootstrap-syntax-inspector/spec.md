## MODIFIED Requirements

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
