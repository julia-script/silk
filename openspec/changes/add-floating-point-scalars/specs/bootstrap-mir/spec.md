## ADDED Requirements

### Requirement: MIR represents floating values and operations

MIR SHALL carry canonical float constants, arithmetic, comparisons, classification, total order, reinterpretation, and conversions with explicit width and provenance. Verification SHALL reject mismatched widths/types and deterministic encoding SHALL preserve exact constant bits.

#### Scenario: Verify f64 reinterpretation

- **WHEN** MIR reinterprets `f64` as `u64` with matching layouts
- **THEN** verification accepts one backend-neutral bit operation

