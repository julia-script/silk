# bootstrap-algorithm-examples Specification

## Purpose
Use familiar readable algorithms as honest end-to-end probes of Silk's practical expressiveness, diagnostics, ergonomics, and evaluator/native/WebAssembly parity.
## Requirements
### Requirement: The suite contains recognizable complete programs

The repository SHALL contain Conway's Game of Life, Sieve of Eratosthenes, matrix multiplication, quicksort, CRC-32, and FFT under `examples/algorithms`. Each SHALL include complete source, deterministic input, expected behavior, capability inventory, and current status.

#### Scenario: Inspect an algorithm

- **WHEN** a contributor opens any algorithm directory
- **THEN** the source and expected behavior are understandable without compiler fixture internals

### Requirement: Status is executable or frontier

An example SHALL be `executable` only when it passes analysis and its declared target checks. Otherwise it SHALL be a complete `frontier` program with deterministic blocker evidence. Unsupported behavior MUST NOT be hidden by fake primitive wrappers, hard-coded answers, or algorithm-specific compiler exceptions.

#### Scenario: Keep FFT honest

- **WHEN** FFT requires a missing math, allocation, or collection capability
- **THEN** the recognizable program remains with precise blockers rather than precomputed output

### Requirement: The suite has an executable baseline

Game of Life, Sieve, matrix multiplication, and CRC-32 SHALL execute through evaluation and every supported backend whose host requirements they declare. Results or output SHALL match exactly. Quicksort and FFT MAY remain frontier only with precise current blockers.

#### Scenario: Run Game of Life

- **WHEN** the fixed initial board advances its declared generations
- **THEN** all supported paths produce the same final board

#### Scenario: Run CRC-32

- **WHEN** committed bytes are processed
- **THEN** `u8`/`u32` operations produce the committed checksum across engines

### Requirement: Walls remain durable evidence

The harness SHALL retain deterministic frontier evidence and SHALL fail if an executable example silently regresses to frontier status.

#### Scenario: Detect regression

- **WHEN** a compiler change breaks a previously executable example
- **THEN** the suite reports the regression without reclassifying it automatically
