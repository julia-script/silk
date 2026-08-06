## ADDED Requirements

### Requirement: Structural-union programs retain three-engine parity

The differential corpus SHALL cover canonical normalization, nominal injection, union widening,
call/return transport, struct and array containment, mutation, loop transport, move-only cleanup,
and unavailable/invalid conversions. Supported programs SHALL agree across MIR evaluation, native
execution, and WebAssembly execution; invalid programs SHALL stop at their phase-owned diagnostic
before artifact construction.

#### Scenario: Run an aggregate union program

- **WHEN** the corpus stores, passes, widens, replaces, and drops a union of nominal aggregates
- **THEN** every available engine agrees on completion, traps, active payload behavior, and cleanup

### Requirement: Structural-union artifacts are deterministic

Repeated fresh compilation SHALL preserve source and semantic union facts, normalized identities,
HIR, ownership, instance order, layouts, calling shapes, MIR mappings, traces, symbols, LLVM IR and
bitcode, WAT, and WebAssembly bytes exactly for equivalent inputs.

#### Scenario: Repeat equivalent union compilations

- **WHEN** equivalent union programs compile repeatedly for supported targets
- **THEN** every compiler-owned artifact and backend-private realization is byte-identical

