## ADDED Requirements

### Requirement: Child-process execution is an injected evaluator host

Evaluation SHALL accept an explicit child-process provider, separate from the OS filesystem and
standard-input hosts, taking one structured request of program, ordered arguments, environment
entries, and an optional working directory, and returning an exit, a signal, or a host failure. The
evaluator SHALL split the low-level NUL-terminated entry blocks into entries before calling the
provider, and MUST NOT import an ambient process implementation into browser-capable compiler cores
or run a real program.

#### Scenario: Evaluate against a scripted provider

- **WHEN** a program executes with an injected provider holding a scripted outcome
- **THEN** the evaluation observes that outcome's termination and captured bytes, and the provider observes the request's exact program, argument, and environment bytes

#### Scenario: Block a reachable execution without a host

- **WHEN** evaluation reaches a child-process execution and no provider was injected
- **THEN** it reports a blocked outcome naming the missing host rather than inventing an outcome

#### Scenario: Reject a malformed low-level request

- **WHEN** an entry block is not NUL-terminated or a program path is empty or contains NUL
- **THEN** evaluation reports the low-level invalid-path reason without calling the provider
