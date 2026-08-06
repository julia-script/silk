## ADDED Requirements

### Requirement: Exhaustive-match programs retain three-engine parity

The differential corpus SHALL cover Copy, consuming, shared, and exclusive matches; precise nominal
and union scrutinees; nested field patterns; guarded fallthrough; universal coverage; exact and union
result joins; branch cleanup; loops and mutation around matches; and invalid coverage, typing,
binding, borrow, and ownership states. Supported programs SHALL agree across MIR evaluation, native
execution, and WebAssembly execution, while invalid programs SHALL stop at their phase-owned outcome.

#### Scenario: Run a consuming match algorithm

- **WHEN** the corpus loops over aggregate-contained unions, consumes one value, matches it, and returns a bound scalar
- **THEN** every available engine agrees on selected arm, result, traps, active payload behavior, and cleanup

### Requirement: Exhaustive-match artifacts are deterministic

Repeated fresh compilation SHALL preserve match syntax, facts, coverage sets, HIR regions, ownership,
instance order, layouts, MIR, traces, symbols, and backend artifacts exactly for equivalent inputs.

#### Scenario: Repeat a guarded match corpus

- **WHEN** equivalent guarded and nested matches compile repeatedly for supported targets
- **THEN** every compiler-owned artifact and backend-private realization is byte-identical
