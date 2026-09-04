# bootstrap-algorithm-examples Specification

## Purpose

Use familiar readable algorithms as honest end-to-end probes of Silk's practical expressiveness, diagnostics, ergonomics, native runtime semantics, and LLVM-to-WebAssembly behavior.

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

Game of Life, Sieve, matrix multiplication, quicksort, and CRC-32 SHALL execute through the native
acceptance corpus and LLVM-generated WebAssembly where their host requirements permit. Results or output SHALL match
exactly. FFT MAY remain frontier only with precise current blockers.

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
shortest-path result to its effectful entry point for verification, and force vector capacities 4,
8, 16, and 32 through the explicit allocator capability without using raw storage in the example
source. The entry point SHALL leave reportable allocation failures unhandled so the runtime exposes
them as execution errors.

#### Scenario: Traverse the complete grid

- **WHEN** breadth-first search runs from the first cell to the opposite corner
- **THEN** native and LLVM-generated WebAssembly execution verify the same shortest distance
  before completing the effectful entry point successfully

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
- **THEN** native and LLVM-generated WebAssembly execution return the committed checksum with no allocation

### Requirement: Quicksort is an executable recursive algorithm

The in-place quicksort example SHALL recursively partition and sort its committed signed integers
through native LLVM and LLVM-generated WebAssembly execution. It MUST NOT be rewritten as an iterative
fixture or granted an algorithm-specific recursion exception.

#### Scenario: Sort through recursive partitions

- **WHEN** quicksort processes `[9, -3, 5, 1, 0, -8, 7, 2]`
- **THEN** all three engines return the fingerprint for `[-8, -3, 0, 1, 2, 5, 7, 9]`

### Requirement: FFT meaningfully exercises trigonometry

The radix-2 FFT example SHALL use an input and result fingerprint whose checked non-DC frequency
components depend on both sine and cosine. A unit impulse at index zero observed only through its DC
component MUST NOT satisfy the example contract.

#### Scenario: Execute the strengthened FFT

- **WHEN** the committed eight-sample signal is transformed through all three butterfly stages
- **THEN** native and LLVM-generated WebAssembly execution return the same committed fingerprint

#### Scenario: Detect a broken transcendental operation

- **WHEN** sine or cosine returns an incorrect conformance value used by a butterfly
- **THEN** the FFT fingerprint differs from the committed result and the example fails
