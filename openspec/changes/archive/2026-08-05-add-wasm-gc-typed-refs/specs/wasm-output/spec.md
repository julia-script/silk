# wasm-output Delta

## ADDED Requirements

### Requirement: GC type and reference encodings

The system SHALL encode recursive type groups, sub types with supertype lists and finality,
struct and array composite types with packed storage types and per-field mutability, and
parameterized reference types (abstract and concrete heap types) exactly as the binary format
specifies, and SHALL render the corresponding text forms (`(rec …)`, `(sub …)`,
`(struct (field …))`, `(array …)`, `(ref null? <heaptype>)`). All new forms SHALL satisfy the
same determinism, oracle-validation, and text-to-binary round-trip guarantees as the baseline
output.

#### Scenario: Recursive group round-trips

- **WHEN** a module defining a recursive group of mutually referring struct types is rendered
  as text and assembled by the pinned oracle
- **THEN** the resulting bytes equal the builder's binary encoding

#### Scenario: Shorthand references stay canonical

- **WHEN** a module uses only baseline `funcref`/`externref`/`exnref` declarations
- **THEN** its emitted bytes are identical to the pre-GC encoding of the same module
