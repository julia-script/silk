# bootstrap-syntax-file Specification

## Purpose

The per-module lossless syntax artifact: one immutable `SyntaxFile` owning the source bytes, the
trivia-preserving token stream, and the source-faithful surface tree, with stable element
identities that every later fact table keys against and a deterministic textual encoder gating
byte-identical output.

## Requirements

### Requirement: One lossless syntax artifact per source module

Parsing one source module SHALL produce exactly one immutable `SyntaxFile` artifact that owns the
original source, the complete trivia-preserving token stream, the source-faithful surface tree
with explicit missing and error nodes, and the lexical and parser diagnostic collections. Slicing
every non-end-of-file token of the artifact in source order SHALL reconstruct the original bytes
exactly.

#### Scenario: Bundle the accepted fixture

- **WHEN** the source bytes spell `pub fn main() -> i32 { return 42 }` and are parsed
- **THEN** one `SyntaxFile` exposes that source, its full token stream including trivia, its surface tree, and empty diagnostic collections

#### Scenario: Reconstruct bytes from the artifact

- **WHEN** a source containing trivia, invalid bytes, and recovered syntax is parsed
- **THEN** concatenating the artifact's non-end-of-file token slices in source order reproduces the original bytes exactly

#### Scenario: Repeat artifact construction

- **WHEN** equivalent source modules are parsed repeatedly in fresh processes
- **THEN** the resulting artifacts are identical in tokens, tree structure, spans, identities, and diagnostics

### Requirement: Stable syntax element identities

Every token and tree node of a `SyntaxFile` SHALL be addressable by a deterministic identity
qualified by the owning source identity, identical across repeated runs over identical input.
Identity lookup for an element that does not belong to the artifact SHALL be rejected rather than
answered with a coincidental identity.

#### Scenario: Identify elements across runs

- **WHEN** the same source module is parsed in two fresh processes
- **THEN** every corresponding token and tree node carries the same identity in both artifacts

#### Scenario: Reject a foreign element

- **WHEN** an element from one artifact is looked up against a different artifact
- **THEN** the lookup reports the element as foreign instead of returning an identity

### Requirement: Deterministic textual syntax encoding

The artifact SHALL expose a deterministic textual encoding covering its tokens, surface tree,
missing and error structure, and diagnostics. The encoder SHALL observe a completed artifact
without participating in any later phase, and identical input bytes with identical source
identity SHALL produce byte-identical encodings across runs. Golden encodings SHALL gate the
format in tests.

#### Scenario: Match the golden encoding

- **WHEN** a committed fixture source is parsed and encoded
- **THEN** the encoding equals the committed golden text byte-for-byte

#### Scenario: Encode recovered syntax visibly

- **WHEN** a malformed source produces missing tokens and error regions
- **THEN** the encoding names each missing element and error region at its exact span

#### Scenario: Repeat encoding

- **WHEN** equivalent sources are parsed and encoded repeatedly in fresh processes
- **THEN** the encoded texts are byte-identical

### Requirement: Syntax files expose raw documentation blocks

A syntax file SHALL expose immutable source-owned raw documentation blocks formed from its original
documentation tokens and intervening line-ending trivia. A block SHALL retain its declaration or
module kind, exact token objects, and complete source span. Building or querying a raw block SHALL
NOT normalize or parse Markdown content.

#### Scenario: Query an attached declaration block

- **WHEN** a syntax node has consecutive attached `///` tokens
- **THEN** the syntax file returns one declaration documentation block containing the original token identities and source span

#### Scenario: Preserve raw module bytes

- **WHEN** a module begins with `//!` documentation containing non-ASCII UTF-8 bytes
- **THEN** its raw block retains exact source-owned byte spans without decoding or normalizing them

### Requirement: Over-budget expression syntax remains a deterministic lossless artifact

A `SyntaxFile` produced from source exceeding the expression-nesting limit SHALL own the complete
original token stream, an explicit recovered error branch for every maximal over-budget expression
region, and the corresponding parser diagnostics. Traversing its concrete tree SHALL encounter
every original token exactly once, and concatenating every non-end-of-file token slice in source
order SHALL reconstruct the original bytes exactly. Equivalent source identities and bytes SHALL
produce identical recovered trees, token identities, spans, diagnostics, and textual encodings.

#### Scenario: Reconstruct a substantially over-budget source

- **WHEN** a source contains an expression substantially deeper than 256 followed by valid syntax
- **THEN** the recovered `SyntaxFile` reconstructs every original byte and retains the following syntax outside the error branch

#### Scenario: Traverse each original token once

- **WHEN** the recovered tree for an over-budget expression is flattened to its original tokens
- **THEN** the flattened token sequence contains the same token objects as the artifact token stream in the same order with no omissions or duplicates

#### Scenario: Repeat over-budget artifact construction

- **WHEN** identical over-budget source is parsed repeatedly in fresh processes
- **THEN** the recovered tree, element identities, diagnostic sequence, and textual encoding are byte-identical across runs
