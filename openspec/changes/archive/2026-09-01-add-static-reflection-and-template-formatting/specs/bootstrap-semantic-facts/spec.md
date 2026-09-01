## ADDED Requirements

### Requirement: Static reflection provenance is deterministic and inspectable

Semantic inspection SHALL retain the concrete type being reflected, ordered authorized member
descriptors, static iterable encoding, per-iteration binding presentation, template segment or
placeholder range, and generated residual operation identities. These facts SHALL distinguish
authored syntax, static-only evaluation data, and residual runtime data without exposing compiler
storage, private inaccessible field names, or backend details.

#### Scenario: Inspect one named placeholder

- **WHEN** `{age}` selects an `i32` field from an anonymous record argument pack
- **THEN** inspection connects the placeholder byte range to the authorized field descriptor and resulting ordinary `i32` projection and Display call

#### Scenario: Repeat semantic encoding

- **WHEN** the same reflection-generated specialization is realized repeatedly
- **THEN** its descriptor, iteration, template, and residual-provenance encodings are byte-identical
