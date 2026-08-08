## ADDED Requirements

### Requirement: Scanner acceptance proves the owned sequence vertically

The driver's continuous gates SHALL include a scanner written in Silk that borrows runtime-sized
source bytes as a slice and returns an owned `Vector<Token>`, growing across at least one
reallocation. The differential harness SHALL verify identical token results across the evaluator,
LLVM native execution, and instantiated Wasm; a failure-ordinal sweep over every allocation the
scanner performs SHALL confirm each injected `OutOfMemory` propagates typed, rolls back partial
initialization, and leaks nothing; and fresh-process artifact determinism SHALL cover the scanner
and its standard-library dependencies.

#### Scenario: Three engines agree on scanned tokens

- **WHEN** the scanner acceptance program tokenizes input long enough to force vector growth
- **THEN** the evaluator, native executable, and Wasm instance produce identical token sequences and exit values

#### Scenario: Exhaustion at every ordinal leaks nothing

- **WHEN** the harness injects allocation failure at each successive allocation ordinal of the scanner run
- **THEN** every run fails with typed `OutOfMemory` or completes, releases every live owner exactly once, and the native run reports no leaked allocation

#### Scenario: Scanner artifacts are deterministic

- **WHEN** the scanner acceptance program is compiled in two fresh processes
- **THEN** every published artifact, including those of imported standard-library modules, is byte-identical
