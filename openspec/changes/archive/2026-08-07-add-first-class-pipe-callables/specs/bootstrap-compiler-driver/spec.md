## ADDED Requirements

### Requirement: Driver acceptance covers first-class callables vertically

The compiler corpus SHALL cover named function values, automatic sections, callable bindings and
returns, generic higher-order functions, Copy and borrowed captures, exclusive mutation, owned
take-once capture, Effect map, flatMap, tap and logging composition, retry rejection, grouped and
ungrouped run, cleanup, and diagnostics across evaluator, native, and Wasm where valid. Fresh runs
SHALL preserve syntax, semantic facts, HIR, ownership, instances, MIR, textual artifacts, and binary
artifacts deterministically.

#### Scenario: Compile the callable Effect milestone

- **WHEN** a canonical program maps and taps an Effect through stored reusable and consuming sections
- **THEN** evaluation, native, and Wasm agree on success, effect nesting, invocation access, ownership, and cleanup

#### Scenario: Reject invalid reuse before emission

- **WHEN** the corpus invokes a take-once section twice or supplies it to a repeatable callback contract
- **THEN** compilation emits the stable ownership or callable-mode diagnostic and no conflicting runtime artifact

#### Scenario: Preserve deterministic callable artifacts

- **WHEN** equivalent callable programs compile repeatedly in fresh processes
- **THEN** generated environment identities, instance ordering, MIR, symbols, and emitted artifacts are byte-identical
