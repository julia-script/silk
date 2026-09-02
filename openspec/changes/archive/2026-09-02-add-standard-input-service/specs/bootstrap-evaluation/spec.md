## ADDED Requirements

### Requirement: Standard input is an injected evaluator host

Evaluation SHALL accept an explicit standard-input provider, separate from the OS filesystem host,
exposing one capacity-bounded read that returns committed bytes or a host failure. The evaluator
MUST NOT import an ambient process input implementation into browser-capable compiler cores, and
MUST NOT commit more bytes than the caller's buffer holds.

#### Scenario: Evaluate against a scripted provider

- **WHEN** a program reads with an injected provider holding scripted bytes
- **THEN** each read commits the provider's chosen prefix, leaves later buffer bytes unchanged, and reports the exact count

#### Scenario: Block a reachable read without a host

- **WHEN** evaluation reaches a standard-input read and no provider was injected
- **THEN** it reports a blocked outcome naming the missing host rather than inventing empty input
