## ADDED Requirements

### Requirement: Backends emit equivalent static data

Native LLVM and direct WebAssembly SHALL realize MIR static bytes, immutable addresses, and target-selected lengths without runtime allocation. Storage coalescing MUST NOT change observable content or identity semantics.

#### Scenario: Emit reused bytes

- **WHEN** one literal is referenced multiple times
- **THEN** both backends expose the specified identical byte views whether or not storage is coalesced

