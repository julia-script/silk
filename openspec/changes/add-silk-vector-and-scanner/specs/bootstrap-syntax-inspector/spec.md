## ADDED Requirements

### Requirement: Unified Labs explains the owned sequence and scanner

The unified `/labs` workbench SHALL provide coordinated presets for the vector and scanner slice:
a growing append sequence, growth failure with a preserved vector, element destruction order on
release, early drop, and the scanner acceptance program itself. Each preset SHALL render through
the existing analysis-facade layers — syntax, semantic facts, ownership, MIR, evaluation — without
inspector-private compiler access, and invalid states SHALL remain inspectable.

#### Scenario: Growth preset shows reallocation end to end

- **WHEN** a user selects the vector growth preset
- **THEN** the workbench shows the allocation, element moves, commit, and old-buffer release across the ownership, MIR, and evaluated layers

#### Scenario: Scanner preset matches the tested program

- **WHEN** a user selects the scanner preset
- **THEN** its source is identical to the driver's scanner acceptance program and evaluation reproduces the tested token outcome
