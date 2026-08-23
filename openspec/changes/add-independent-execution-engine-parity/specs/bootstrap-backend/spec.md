## ADDED Requirements

### Requirement: Native and Wasm realize verified independent execution

Native and direct-Wasm backends SHALL lower only validated independent-execution MIR and SHALL
realize execution-owned continuation storage, exact package plans, logical drive/resume dispatch,
nested transfer, external park, fixed endpoint notification, cancellation, DestroyPending, and
cleanup. Both backends SHALL keep Execution and Wake local and use no mandatory atomic operation in
the initial model. Continuation-stack exhaustion and illegal states SHALL trap under the no-unwind
contract. Backend runtime helpers, labels, physical state tags, field offsets, and segment policies
SHALL remain private and deterministic.

#### Scenario: Resume non-LIFO on native

- **WHEN** validated MIR wakes and drives two parked executions in reverse suspension order
- **THEN** native resumes each sole continuation with evaluator-equivalent results and ordered cleanup

#### Scenario: Resume non-LIFO on direct Wasm

- **WHEN** the same validated MIR is emitted to direct Wasm
- **THEN** Wasm resumes the same continuations and agrees with evaluation and native on outcomes and ownership events

#### Scenario: Keep local wake non-atomic

- **WHEN** a local-only execution and Wake program is inspected on native and Wasm
- **THEN** neither artifact introduces thread transfer, mandatory atomic instructions, or a work-stealing runtime

#### Scenario: Trap before callbacks

- **WHEN** validated test-only state reaches a Dormant/Notifying drive or stack exhaustion trap
- **THEN** both backends trap before invoking completion or suspension callbacks and promise no unwinding cleanup

#### Scenario: Preserve backend determinism

- **WHEN** equivalent validated plans are emitted repeatedly
- **THEN** runtime helper selection, resume labels, package-layout references, and artifacts are byte-identical for each target
