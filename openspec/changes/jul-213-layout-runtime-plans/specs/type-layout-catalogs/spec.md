# type-layout-catalogs Specification

## ADDED Requirements

### Requirement: Type layout precedes reachability

`Layout.computeTypes` SHALL compute target-relative declaration layouts without accepting or reading
an instance graph.

#### Scenario: Uninstantiated declaration

- **WHEN** a non-generic represented type is declared but unreachable
- **THEN** its type layout remains available in the catalog

### Requirement: Type layout inputs are explicit

The operation SHALL receive target, declaration facts, current literal presentation, and optional
opaque realization facts explicitly.

#### Scenario: Target changes

- **WHEN** the same declarations are computed for targets with different pointer widths
- **THEN** pointer and word layouts differ by target
