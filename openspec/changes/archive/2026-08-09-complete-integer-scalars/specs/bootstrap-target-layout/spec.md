## MODIFIED Requirements

### Requirement: Bootstrap scalar layouts are canonical

The layout plan SHALL retain `bool` as the existing four-byte zero-or-one scalar; fixed-width integers SHALL use their named byte width and natural alignment; `usize`/`isize` SHALL use pointer width and alignment; `()`/`never` SHALL have no runtime lane. All supported targets remain little-endian and these are private ABI facts.

#### Scenario: Plan the integer family

- **WHEN** a program reaches every fixed-width integer
- **THEN** layout fixes width, alignment, signedness, and calling lane before backend emission

#### Scenario: Plan unit and bottom

- **WHEN** unit or bottom occurs in control flow
- **THEN** layout assigns no runtime value lane

### Requirement: Usize layout and calling lanes are compiler-owned target facts

The planner SHALL represent `usize` as unsigned 64-bit on required native targets and unsigned 32-bit on Wasm, validate literals against that width, and require backends to consume the selected lane.

#### Scenario: Plan native usize

- **WHEN** a native signature contains `usize`
- **THEN** the plan publishes one unsigned 64-bit lane before MIR lowering

