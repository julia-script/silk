## ADDED Requirements

### Requirement: Backends emit deterministic concrete specializations

Native LLVM and direct WebAssembly emission SHALL lower each reachable generic-origin MIR instance
as one concrete definition using its compiler-selected type layout, calling shape, and deterministic
symbol. Backends MUST NOT merge layout-distinct instances or add runtime generic dispatch.

#### Scenario: Emit layout-distinct instances

- **WHEN** MIR contains specializations whose argument types have different selected layouts
- **THEN** each backend emits distinct concrete definitions and both executions agree with evaluation

#### Scenario: Repeat specialization symbols

- **WHEN** equivalent specialized MIR is emitted in fresh processes
- **THEN** native and WebAssembly symbol identities and artifacts are deterministic
