## ADDED Requirements

### Requirement: Codegen is a facade query

The facade SHALL answer a snapshot's backend emission — bitcode, IR text, and symbols for a
given codegen request — through the nominal backend service, so tooling never invokes the
backend directly.

#### Scenario: Emit through the facade

- **WHEN** a tool requests a snapshot's release emission
- **THEN** the facade answers the artifact with its bitcode bytes, IR text, and symbol table
