## ADDED Requirements

### Requirement: Allocation acceptance covers the substrate vertically

The continuous compiler corpus SHALL cover valid and invalid layout formation, role-selected
allocator provision, successful and exhausted allocation, provider access ending before result
cleanup, affine moves, typed buffers and slots, initialization and rollback, restricted-hook
rejection, explicit early drop, every structured exit, trap separation, zero-sized and over-aligned
storage, and post-failure reuse. Evaluator, native, and Wasm SHALL agree on every logical result and
cleanup trace. Fresh-process runs SHALL keep syntax, facts, ownership, HIR, instances, target layout,
MIR, traces, textual output, and binary artifacts deterministic.

#### Scenario: Compile the construction-guard milestone

- **WHEN** a canonical program allocates runtime-counted move-only slots, initializes a guarded prefix, and exits through success and injected typed failure
- **THEN** all three engines agree on values, `OutOfMemory`, hook order, exact releases, target layout, and emitted artifacts

#### Scenario: Reject unsafe misuse before artifacts

- **WHEN** source accesses a Slot safely, escapes it, consumes its live buffer, duplicates an Allocation, or declares an invalid Drop hook
- **THEN** compilation emits the responsible stable diagnostic and produces no MIR or executable artifact for that program

#### Scenario: Preserve allocation-free stability

- **WHEN** an allocation-free corpus program compiles after the substrate is added
- **THEN** it gains no allocator witness, allocation layout, reclaim ticket, Drop hook, or heap operation solely because the feature exists
