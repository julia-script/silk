# object-emission Specification

## ADDED Requirements

### Requirement: Backend emission remains MIR-only

`Backend.emit` SHALL consume MIR and a backend code-generation request and SHALL NOT allocate build
directories, invoke native object compilers, link, or commit destinations.

#### Scenario: LLVM backend emission

- **WHEN** valid MIR is emitted for LLVM
- **THEN** the result is a bitcode/IR artifact with symbol and foreign metadata

### Requirement: Object materialization is explicit

`ObjectEmission.materialize` SHALL consume a backend artifact, resolved toolchain, compilation
profile, and caller-owned build scope and SHALL return its scope-bound object plus command,
inventory, and helper metadata.

#### Scenario: Scope cleanup

- **WHEN** the enclosing build scope closes without committing the object
- **THEN** its temporary path is cleaned with the scope
