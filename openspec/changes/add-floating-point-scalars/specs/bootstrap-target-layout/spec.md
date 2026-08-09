## ADDED Requirements

### Requirement: Float layouts and calling lanes are canonical

The target layout plan SHALL represent `f32` as IEEE binary32 with four-byte size/alignment and `f64` as IEEE binary64 with eight-byte size/alignment on every supported target. Backends MUST consume those planned lanes.

#### Scenario: Plan both float widths

- **WHEN** a reachable signature contains `f32` and `f64`
- **THEN** layout publishes both canonical lanes before MIR lowering

