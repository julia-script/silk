## ADDED Requirements

### Requirement: Runtime slices preserve three-engine parity

The canonical multi-module coverage fold SHALL accept a shared runtime slice, use its logical
length, and be invoked with at least two distinct fixed-array lengths through one discovered
function instance. Logical evaluation, native execution, and direct Wasm execution SHALL complete
with the agreed result `42`, and their artifacts SHALL remain deterministic.

#### Scenario: Generalize the coverage fold

- **WHEN** the acceptance entry calls the same coverage fold with the reviewed minimal and complete arrays
- **THEN** instance discovery reports one fold instance and all three engines return the pinned result `42`

### Requirement: Exclusive slices preserve caller-visible mutation across engines

One compiler-shaped acceptance program SHALL pass a mutable fixed array to an ordinary helper as an
exclusive slice, replace an element through a runtime index, return, and immediately inspect the
original owner. Logical, native, and Wasm execution MUST agree on the changed value and cleanup
trace.

#### Scenario: Mutate through a helper

- **WHEN** the exclusive-slice helper replaces one move-only aggregate element and returns to its caller
- **THEN** every engine observes the replacement in the caller's array and cleans the displaced and remaining elements exactly once

### Requirement: Slice acceptance exercises failure boundaries

The compiler corpus SHALL retain deterministic negative cases for implicit decay, immutable
exclusive borrowing, conflicting argument loans, recursive slice storage or return, unsupported
standalone binding, non-Copy extraction, unrepresentable length, and runtime out-of-bounds access.

#### Scenario: Repeat invalid slice compilation

- **WHEN** each invalid slice fixture is compiled repeatedly in fresh processes
- **THEN** it yields the same phase-owned diagnostic or runtime trap without producing a successful conflicting artifact
