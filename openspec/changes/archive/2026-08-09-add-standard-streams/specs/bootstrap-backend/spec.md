## ADDED Requirements

### Requirement: Backends realize explicit byte writes

Native lowering SHALL call the supplied process adapter; direct WebAssembly SHALL emit the declared host import. Both SHALL preserve MIR ordering, destinations, complete bytes, and typed failures with no implicit console behavior.

#### Scenario: Emit hosted Wasm output

- **WHEN** a Wasm program writes bytes with a supplied host
- **THEN** the host receives the same bytes and ordering as evaluation
