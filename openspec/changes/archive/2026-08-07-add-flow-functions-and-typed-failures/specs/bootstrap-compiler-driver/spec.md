## ADDED Requirements

### Requirement: Flow acceptance covers both outcome branches deterministically

The compiler corpus SHALL execute flow success, propagation, exact recovery, residual-row rejection,
ownership cleanup, and trap separation through evaluation, native, and Wasm where valid. Equivalent
fresh-process compilations SHALL preserve semantic facts, layout, MIR, text, and binary artifacts.

#### Scenario: Compare success and recovery across engines

- **WHEN** a canonical fixture is compiled once for its success input and once for its handled failure input
- **THEN** all three engines agree and repeated builds are byte-identical

#### Scenario: Reject an unresolved executable failure

- **WHEN** an ordinary entry attempts to run a flow with a nonempty residual row
- **THEN** compilation rejects it before MIR emission and creates no executable artifact
