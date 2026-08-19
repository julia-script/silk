## ADDED Requirements

### Requirement: Requirement identity is keyed independently from access

An Effect requirement SHALL be identified by its canonical service identity plus optional `at` role. Shared, exclusive, and acquired access SHALL be checked as provider compatibility and SHALL NOT create different requirement keys. Requirement union, subtraction, and diagnostics SHALL be deterministic.

#### Scenario: Distinguish two clocks by role

- **WHEN** one Effect requires `Clock at source` and `Clock at destination`
- **THEN** the row contains two keys and each provision discharges only the selected role

#### Scenario: Reject insufficient provider access

- **WHEN** an exclusive requirement is matched with only shared provider access
- **THEN** provision reports an access mismatch without changing the requirement's identity

### Requirement: Provision helpers discharge exact keys

`provide`, `provideMut`, acquisition provision, and `provideEffect` SHALL discharge only their exact selected keys and preserve all unrelated failures and requirements. `provideWith` SHALL NOT remain as an alias. `Effect.flatten` SHALL union the requirements of both layers before provision.

#### Scenario: Flatten a repeated requirement

- **WHEN** `Effect.flatten` receives `Effect<Effect<i32 ? &Clock> ? &Clock>`
- **THEN** the result is `Effect<i32 ? &Clock>` with one normalized key rather than two runtime slots

#### Scenario: Build a provider effectfully

- **WHEN** `provideEffect` obtains a provider from an Effect with its own failure and requirements
- **THEN** those channels compose normally while the selected provided key is removed from the protected Effect
