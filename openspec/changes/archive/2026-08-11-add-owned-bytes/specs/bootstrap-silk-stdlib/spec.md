## ADDED Requirements

### Requirement: Canonical source exports encoding-neutral Bytes

The generated standard-library module graph SHALL export one canonical `Bytes` actor implemented in
ordinary Silk source over `Vector<u8>`. Its manifest dependencies SHALL use the ordinary `Allocator`,
`OutOfMemory`, returned-borrow, and Drop contracts, and MUST NOT import filesystem or String policy.

#### Scenario: Load Bytes without platform facilities

- **WHEN** a portable program imports and uses `Bytes` on direct Wasm
- **THEN** module closure includes only its ordinary storage and allocation dependencies and no operating-system imports

#### Scenario: Keep text and I/O out of Bytes

- **WHEN** the canonical source inventory is inspected
- **THEN** `Bytes` contains no UTF-8 validation, formatting rules, filesystem service dependency, or platform provider
