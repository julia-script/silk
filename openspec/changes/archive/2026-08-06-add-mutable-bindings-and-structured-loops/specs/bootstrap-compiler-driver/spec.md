## ADDED Requirements

### Requirement: Mutable-loop programs retain three-engine parity

The driver corpus SHALL cover immutable-write rejection, scalar and place assignment, Copy and
move-only replacement, zero and multiple iterations, nested loops, `break`, `continue`, `return`,
checked index traps, loop-header ownership failure, and cleanup. Supported programs SHALL agree
across MIR evaluation, native execution, and WebAssembly execution; invalid programs SHALL retain
their phase-owned outcomes before artifact construction.

#### Scenario: Run an array algorithm

- **WHEN** the corpus mutates and scans a fixed-size array through a structured loop
- **THEN** every available engine produces the same result and traceable control decisions

### Requirement: Control DAG artifacts are deterministic

Repeated compilation in fresh processes SHALL preserve semantic loop facts, HIR regions, ownership
fixed points, cleanup plans, MIR DAG nodes and topological encoding, evaluation traces, symbols, LLVM
IR and bitcode, WAT, and WebAssembly bytes exactly for equivalent inputs.

#### Scenario: Repeat nested-loop compilation

- **WHEN** one nested-loop program is compiled repeatedly for supported targets
- **THEN** every compiler-owned artifact is identical and backend-local control conversion is deterministic
