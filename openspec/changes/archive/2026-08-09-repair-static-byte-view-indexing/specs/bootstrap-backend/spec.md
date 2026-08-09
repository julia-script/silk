## ADDED Requirements

### Requirement: Backends emit indexed static-byte reads

Native LLVM and direct WebAssembly SHALL bounds-check a runtime index against the static view's
target-sized length and load the selected `u8` from immutable static storage. Both paths MUST trap
on the same invalid indices and MUST NOT allocate or copy the complete literal at runtime.

#### Scenario: Load a static byte on both targets

- **WHEN** accepted MIR indexes a static byte view at a valid runtime position
- **THEN** native and WebAssembly execution observe the same byte as evaluation

#### Scenario: Trap a backend overrun

- **WHEN** accepted MIR executes an index equal to the static view length
- **THEN** native and WebAssembly take their canonical bounds trap before reading storage
