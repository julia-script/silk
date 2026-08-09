## ADDED Requirements

### Requirement: Evaluation indexes immutable static bytes

Evaluation SHALL resolve an indexed static view against its compiler-owned bytes, check the runtime
index against the view length, and return the selected `u8` without converting the view into an
array, allocation, or host string.

#### Scenario: Evaluate an indexed byte literal

- **WHEN** a program reads every byte of a static byte literal inside a loop
- **THEN** evaluation returns the exact decoded values and records no allocation event

#### Scenario: Trap an out-of-bounds static read

- **WHEN** the runtime index equals or exceeds the static view length
- **THEN** evaluation blocks with the ordinary indexed-read trap and the indexing span
