## ADDED Requirements

### Requirement: Inspect the module-closure import graph

The docs site SHALL expose a direct-link module-closure lab that loads a compilation request and
presents the loaded closure: every module in canonical identity order with its import facts,
cycle facts marked on their participating modules, and the closure's module-phase diagnostics.
The lab SHALL keep its state in browser memory only.

#### Scenario: Inspect a diamond closure

- **WHEN** a developer selects a preset whose root imports two modules sharing one dependency
- **THEN** the lab lists all four modules in canonical order, each with its resolved import facts, and reports no cycles

#### Scenario: Mark an import cycle

- **WHEN** a developer selects a preset containing mutually importing modules
- **THEN** the lab marks every module participating in the cycle and names the cycle's members in canonical order

#### Scenario: Surface closure diagnostics

- **WHEN** a preset contains an unknown import target or a self-import
- **THEN** the lab lists the module-phase diagnostics with their codes and exact spans
