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

### Requirement: Breadth-first search exercises owned allocation

The suite SHALL contain an executable breadth-first search over a deterministic 5×5 grid whose
queue is an ordinary `Vector<QueueEntry>`. The search SHALL visit all 25 cells, return the committed
shortest-path result, and force vector capacities 4, 8, 16, and 32 through the explicit allocator
capability without using raw storage in the example source.

#### Scenario: Traverse the complete grid

- **WHEN** breadth-first search runs from the first cell to the opposite corner
- **THEN** evaluation, native execution, and direct WebAssembly return the same shortest distance

### Requirement: Allocation-sensitive examples retain resource evidence

An algorithm manifest MAY declare exact evaluation allocation evidence. When present, the harness
SHALL verify successful acquisitions, matching releases, and peak simultaneously live allocations
in addition to the ordinary result and target checks.

#### Scenario: Observe vector growth and cleanup

- **WHEN** the breadth-first-search queue grows through capacities 4, 8, 16, and 32 and then leaves scope
- **THEN** evaluation records four acquisitions, four releases, and a peak of two live allocations

### Requirement: CRC-32 consumes committed static bytes

The executable CRC-32 example SHALL take its input from a static byte literal and index that
immutable view inside the checksum loop. A substitute fixed array containing the same numeric
values MUST NOT satisfy this example contract.

#### Scenario: Checksum a static literal

- **WHEN** CRC-32 processes the committed bytes `99 13 1d 00`
- **THEN** evaluation, native execution, and direct WebAssembly return the committed checksum with no allocation
