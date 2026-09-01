## ADDED Requirements

### Requirement: Differential gates pressure pipeline composition

The continuous compiler corpus SHALL compile and execute a deterministic matrix of ordinary value
and Effect pipelines through evaluation, native LLVM, and direct WebAssembly. The matrix SHALL
cover left association and grouping, direct and stored forms, ordinary and effectful entries,
Copy and affine values, automatic and stored callables, `map`, `flatMap`, `tap`, `catch`, `retry`,
`provide`, and `provideWith`, including representative combinations rather than only isolated
operators. Equivalent source shapes SHALL produce equal observable outcomes and cleanup; repeated
fresh analyses SHALL preserve deterministic artifacts.

#### Scenario: Compare pipeline source shapes

- **WHEN** data-first, piped, grouped, and stored programs express the same valid computation
- **THEN** every supported engine returns the same result with the same logical failure and cleanup observations

#### Scenario: Exercise an effectful entry pipeline

- **WHEN** effectful `main` directly runs a mapped and provisioned Effect
- **THEN** compilation reaches every requested backend and runtime execution completes without a compiler exception or generated trap

#### Scenario: Pressure a recognizable affine program

- **WHEN** the Silk lexer maps its owned token and diagnostic result through verification before allocator provision and execution
- **THEN** evaluator, native, and WebAssembly preserve its fingerprint, allocation-failure behavior, and exactly-once cleanup

#### Scenario: Repeat the pipeline matrix

- **WHEN** equivalent pipeline fixtures are analyzed in fresh processes
- **THEN** their closure, HIR, ownership, instances, layout, MIR, traces, symbols, and backend artifacts remain identical
