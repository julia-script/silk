# bootstrap-language-pressure-programs Delta

Failure-ordinal sweeps are tiered: the evaluator and WebAssembly carry every ordinal, and native
execution carries representative boundary ordinals instead of every ordinal, keeping cross-engine
agreement on representative cases while removing one full native compile per exercised ordinal.

## MODIFIED Requirements

### Requirement: Execution and ownership evidence is cross-engine and deterministic

Representative valid and invalid lexer cases SHALL agree across evaluation, native LLVM, and
direct WebAssembly execution. Allocation failure at every exercised growth ordinal SHALL preserve
typed `OutOfMemory`, release every acquired allocation exactly once, and leave subsequent runs
deterministic; the evaluator and WebAssembly SHALL carry every exercised ordinal, and native
execution SHALL carry representative boundary ordinals including at least the first failing
ordinal, one mid-growth ordinal, and unrestricted completion.

#### Scenario: Engines agree on a representative valid case

- **WHEN** the valid acceptance case is evaluated, compiled and run natively, and instantiated as WebAssembly
- **THEN** every engine reports the same deterministic lexer fingerprint and successful cleanup

#### Scenario: Engines agree on a representative invalid case

- **WHEN** the invalid acceptance case runs on all three engines
- **THEN** every engine reports the same deterministic token-and-diagnostic fingerprint and successful cleanup

#### Scenario: Allocation failure rolls back cleanly

- **WHEN** allocation is rejected at any token or diagnostic vector growth ordinal exercised by the acceptance cases
- **THEN** the typed failure is preserved and every earlier acquisition is released exactly once without double-dropping initialized records, with the evaluator and WebAssembly checking every ordinal and native execution checking the boundary ordinals

### Requirement: Stack VM resource behavior is cross-engine and deterministic

Representative valid and malformed VM programs SHALL agree across evaluation, native LLVM, and
direct WebAssembly execution. Allocation failure at every exercised trace or diagnostic growth
ordinal SHALL preserve typed `OutOfMemory`, release every acquired allocation exactly once, and
leave subsequent executions deterministic; the evaluator and WebAssembly SHALL carry every
exercised ordinal, and native execution SHALL carry representative boundary ordinals including at
least the first failing ordinal, one mid-growth ordinal, and unrestricted completion.

#### Scenario: Engines agree on VM fingerprints

- **WHEN** representative valid and malformed programs run on all three engines
- **THEN** every engine reports the same deterministic result, trace-and-diagnostic fingerprint, and cleanup outcome

#### Scenario: VM observation allocation rolls back cleanly

- **WHEN** allocation is rejected at any trace or diagnostic vector growth ordinal exercised by the acceptance programs
- **THEN** the typed failure is preserved and every earlier acquisition is released exactly once without exposing a partial result, with the evaluator and WebAssembly checking every ordinal and native execution checking the boundary ordinals
