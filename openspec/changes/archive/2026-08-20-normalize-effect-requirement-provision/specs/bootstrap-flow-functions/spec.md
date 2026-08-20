## ADDED Requirements

### Requirement: Requirement identity is keyed independently from access

An Effect requirement SHALL be identified by its canonical service identity plus optional `at` role. Shared, exclusive, and acquired access SHALL be checked as provider compatibility and SHALL NOT create different requirement keys. Requirement union, subtraction, and diagnostics SHALL be deterministic.

#### Scenario: Distinguish two clocks by role

- **WHEN** one Effect requires `Clock at source` and `Clock at destination`
- **THEN** the row contains two keys and each provision discharges only the selected role

#### Scenario: Reject insufficient provider access

- **WHEN** an exclusive requirement key and conformance match but the provider offers only shared access
- **THEN** provision reports `SEM0131` without changing the requirement's identity or treating the key as absent

#### Scenario: Select one key before checking access

- **WHEN** an explicit `Clock at Primary` selector names one row key
- **THEN** provision resolves that key and its conformance before validating the helper's provider access mode

### Requirement: Provision helpers discharge exact keys

`provide`, `provideMut`, acquisition provision, and `provideEffect` SHALL discharge only their exact selected keys and preserve all unrelated failures and requirements. `provideWith` SHALL NOT remain as an alias. `Effect.flatten` SHALL union the requirements of both layers before provision.

#### Scenario: Flatten a repeated requirement

- **WHEN** `Effect.flatten` receives `Effect<Effect<i32 ? &Clock> ? &Clock>`
- **THEN** the result is `Effect<i32 ? &Clock>` with one normalized key rather than two runtime slots

#### Scenario: Build a provider effectfully

- **WHEN** `provideEffect` obtains a provider from an Effect with its own failure and requirements
- **THEN** those channels compose normally while the selected provided key is removed from the protected Effect
