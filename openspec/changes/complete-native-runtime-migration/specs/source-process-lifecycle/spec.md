## Purpose

Child execution and captures belong to per-call source-owned resources.

## ADDED Requirements

### Requirement: Complete blocking execution

The selected source provider SHALL preserve ordered NUL-free byte arguments, explicit empty/custom environment, inherited/selected cwd, stdin EOF, complete owned stdout/stderr and exit/signal data. Startup, capture and wait failure SHALL remain distinct from nonzero exit.

#### Scenario: Independent outcomes

- **WHEN** a second child executes after a first outcome was returned
- **THEN** the first captures retain their complete bytes independently

### Requirement: Descriptor and child lifecycle

Source SHALL own every pipe end, staging value and child-reaping obligation. Both output streams SHALL drain concurrently with partial transfer and EINTR handling. Partial setup and capture failures SHALL release acquired resources, resolve the child and preserve the primary error. Blocking cancellation SHALL promise only actual recoverable boundaries, not arbitrary foreign-call preemption.

#### Scenario: Capture failure

- **WHEN** allocation fails while draining a live child
- **THEN** owned endpoints and reaping obligations are resolved without replacing the primary failure

### Requirement: Compiler protocol removal

The replacement SHALL delete execute/capture intrinsics, lowering, runtime reservations, generated C process helpers and global captures. Foreign signatures, records and constants SHALL have independent selected-platform evidence.

#### Scenario: Closure inspection

- **WHEN** the process replacement is claimed complete
- **THEN** source and emitted objects contain no compiler process protocol or global capture bridge
