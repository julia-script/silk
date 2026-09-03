## ADDED Requirements

### Requirement: Foreign host functions are explicit per evaluation

Bootstrap evaluation SHALL accept an immutable host-function table keyed by foreign symbol as part
of one evaluation's options. Each binding SHALL declare the C classes of its parameters and result
and SHALL return either a value under that contract or a typed host failure. Evaluation SHALL
snapshot no ambient process symbols and SHALL validate the complete reachable foreign inventory
before executing the entry function.

#### Scenario: Keep bindings local to one evaluation

- **WHEN** two evaluations of the same program receive different exact `abs` host bindings
- **THEN** each evaluation observes only its own binding and neither changes compiler-global state

#### Scenario: Surface a host invocation failure

- **WHEN** an admitted host function returns a typed failure while executing symbol `abs`
- **THEN** evaluation returns a blocked outcome naming `abs`, the failure message, and the call provenance

#### Scenario: Reject an invalid host result

- **WHEN** a host binding declared as `() -> i32` returns a value outside the evaluator's `i32` representation
- **THEN** evaluation returns a symbol-specific blocked outcome instead of writing an invalid MIR local
