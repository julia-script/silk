## ADDED Requirements

### Requirement: Finite compatible Effects join without construction identity

A finite control-flow join SHALL admit Effect values whose success, failure, requirement, capture-access, and ownership contracts have a valid common result, even when the Effects were constructed at different source sites. The join SHALL preserve laziness and SHALL NOT allocate or erase the concrete alternatives.

#### Scenario: Join two lazy branch Effects

- **WHEN** an `if` selects between independently constructed `Effect<i32 ! never>` values
- **THEN** the expression has one usable Effect type and only the selected branch runs

#### Scenario: Join compatible channels

- **WHEN** two branch Effects contribute distinct ordinary failure members and requirement keys
- **THEN** the joined Effect carries their normalized unions and preserves the selected branch's exact outcome

### Requirement: Composite Effect realization is finite and deterministic

HIR and MIR SHALL represent the admitted alternatives as a closed finite composite whose evaluator, LLVM, and Wasm realizations select one alternative without heap allocation. A join with no finite compatible representation SHALL retain a source diagnostic.

#### Scenario: Compare all engines

- **WHEN** equivalent joined Effects are evaluated and compiled repeatedly
- **THEN** all engines produce the same typed outcome, ownership cleanup, and deterministic artifact identity
