## ADDED Requirements

### Requirement: The driver compiles internal aggregate call chains

The compiler driver SHALL carry nominal struct values through declaration analysis, HIR, ownership,
runtime discovery, target layout and calling-shape selection, MIR lowering, backend emission, and
native linking while preserving the fixed scalar host entry boundary. Struct construction and
projection failures SHALL remain ordinary deterministic diagnostics and SHALL prevent only invalid
downstream work.

#### Scenario: Compile through a public factory

- **WHEN** a root module calls another module's public factory, passes the returned struct through an internal function, and returns a projected `I32`
- **THEN** the driver produces a native executable whose exit result matches MIR evaluation

#### Scenario: Refuse external raw construction

- **WHEN** a root module attempts a raw literal for another module's struct
- **THEN** the driver reports the defining-module diagnostic and performs no MIR or backend work for the invalid program

### Requirement: Aggregate differential and determinism gates remain continuous

The driver corpus SHALL include valid, invalid, nested, empty, reordered, cross-module, moved,
projected, and cleanup-bearing aggregate programs. Native execution, WebAssembly execution, and MIR
evaluation SHALL agree where applicable, and repeated fresh-process compilation SHALL preserve
diagnostics, HIR, layouts, MIR, symbols, IR, WAT, and bitcode exactly.

#### Scenario: Run the aggregate parity corpus

- **WHEN** continuous checks execute the aggregate corpus on supported targets
- **THEN** every valid program agrees across evaluation and available backends and every invalid program preserves its expected phase-owned diagnostics
