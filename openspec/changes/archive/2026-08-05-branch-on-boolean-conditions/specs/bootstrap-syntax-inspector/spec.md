## ADDED Requirements

### Requirement: Inspect user-authored branching

The labs SHALL present the branching surface over the facade: the HIR view shows conditional
statements with their typed conditions and arms, the MIR CFG lab shows user-authored branch
diamonds with taken and otherwise edges and join blocks, the ownership lab shows arm-scoped
bindings and per-return exits, and the evaluation surface shows which arm a run took.

#### Scenario: Inspect a branch diamond

- **WHEN** a developer enters a program with an `if`/`else` and a trailing return
- **THEN** the MIR lab shows the entry block ending in a branch with edges to both arms and the encoded text names the branch terminator

#### Scenario: Inspect arm-scoped ownership

- **WHEN** a developer inspects a function binding a value inside one arm
- **THEN** the ownership lab shows that binding's live range inside the arm and its release on the arm's exit
