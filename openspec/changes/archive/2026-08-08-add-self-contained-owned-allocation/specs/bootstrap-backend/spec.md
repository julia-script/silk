## ADDED Requirements

### Requirement: Native and Wasm realize self-contained allocation identically

Native LLVM and direct WebAssembly SHALL lower verified general allocator witness calls,
compiler-planned target layouts, typed `OutOfMemory`, affine allocation and reclaim tickets,
RawBuffer and Slot operations, restricted Drop, and cleanup ordering from MIR. Neither backend may
recognize an allocator implementation kind, retain a provider borrow in the result, substitute a
trap for exhaustion, choose a different typed stride, add a named lifetime scope, or promise cleanup
after a trap. Physical reclamation policy may differ, but observable logical ownership, failure,
and exactly-once release MUST match evaluation.

#### Scenario: Agree on successful allocation

- **WHEN** equivalent native and Wasm programs allocate, initialize, move, and explicitly drop one typed buffer
- **THEN** both match evaluator results, target-selected layouts, initialization order, and one logical release

#### Scenario: Agree under exhaustion

- **WHEN** deterministic exhaustion rejects a requested allocation
- **THEN** native and Wasm propagate the same `OutOfMemory`, clean earlier owners in the same order, and create no release for the rejected request

#### Scenario: Preserve zero-sized identity

- **WHEN** two zero-byte allocations remain live simultaneously
- **THEN** each backend preserves two distinct affine logical owners even if their physical address representation is shared or synthetic
