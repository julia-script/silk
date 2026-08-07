## ADDED Requirements

### Requirement: Usize layout and calling lanes are compiler-owned target facts

The target-aware layout phase SHALL represent `Usize` as size eight, alignment eight, and one
unsigned 64-bit scalar lane on each native target, and as size four, alignment four, and one unsigned
32-bit scalar lane on `wasm32-unknown-unknown`. It SHALL validate exact literal magnitudes against
that width before MIR lowering. Backends MUST consume the selected layout and calling lane rather
than choosing or narrowing them independently.

#### Scenario: Plan native Usize

- **WHEN** a reachable native signature contains `Usize`
- **THEN** the plan publishes one 64-bit unsigned lane and an eight-byte layout before MIR lowering

#### Scenario: Leave unrelated layouts byte-stable

- **WHEN** a reachable program contains no `Usize`
- **THEN** layout planning does not eagerly add a `Usize` entry or perturb its existing encoding
