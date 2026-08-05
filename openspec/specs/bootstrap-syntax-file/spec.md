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

- **WHEN** the source bytes spell `pub fn main() -> I32 { return 42 }` and are parsed
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

