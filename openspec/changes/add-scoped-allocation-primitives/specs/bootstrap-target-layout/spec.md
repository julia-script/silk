## ADDED Requirements

### Requirement: Allocation layout is selected before lowering

The target-aware layout phase SHALL provide `Usize` size and alignment arithmetic, internal
addresses, `Layout`, `SlotLayout<T>`, affine allocation, private cleanup-control, and lexical-slot
representations for every reachable concrete allocation operation. It SHALL derive repeated-element
stride and bounds from the canonical element layout and selected target before MIR lowering.
Backends MUST consume these facts and MUST NOT recompute alignment, padding, address width, typed
failure shape, hidden service-slot shape, or cleanup calling shape.

#### Scenario: Plan native aggregate slots

- **WHEN** a native-target program allocates runtime-counted slots for a padded nominal struct
- **THEN** the compiler plan fixes target-width total-size checks, aligned stride, address representation, and cleanup shape before native emission

#### Scenario: Plan Wasm allocation addresses

- **WHEN** the same logical program targets `wasm32-unknown-unknown`
- **THEN** the plan uses the Wasm target's address width while preserving the same logical allocation and ownership semantics

### Requirement: Zero-sized allocations retain logical provenance

The layout plan SHALL preserve a valid aligned provenance position for a successful zero-sized
allocation without requiring physical bytes. Distinct live zero-sized allocations SHALL retain
distinct logical ownership and cleanup identities even when a backend reuses an address.

#### Scenario: Plan repeated zero-sized elements

- **WHEN** a zero-sized element layout is repeated by a nonzero runtime count
- **THEN** total byte size remains zero while logical count, alignment, slot bounds, and allocation identity remain explicit
