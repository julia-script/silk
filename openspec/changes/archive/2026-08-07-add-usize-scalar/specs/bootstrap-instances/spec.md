## ADDED Requirements

### Requirement: Usize participates in ordinary instance identity

Instance discovery SHALL include canonical `Usize` types and operations in signatures and reachable
bodies. Literal magnitude and selected target width MUST NOT create separate generic instances;
target selection belongs to the layout and lowering inputs for the same canonical instance.

#### Scenario: Reuse a generic Usize instance

- **WHEN** one generic identity function is called with several `Usize` magnitudes on one target
- **THEN** discovery produces one concrete `Usize` instance
