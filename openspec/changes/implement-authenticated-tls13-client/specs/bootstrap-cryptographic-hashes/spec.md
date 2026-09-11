## MODIFIED Requirements

### Requirement: Every SHA actor supports one streaming lifecycle

Every SHA actor SHALL expose a pure `make() -> Self`, an inherent `update(self: &mut Self, bytes: &[u8])` method, a consuming `finish(self: Self)` method returning the actor's exact digest type, and a one-shot `hash(bytes: &[u8])` member returning the same digest type. `update` SHALL absorb bytes in call order, SHALL accept an empty slice without changing the digest, and SHALL permit any chunking that represents the same byte sequence. `finish` SHALL consume the state so a finalized state cannot be updated or finalized again. SHA-256 and SHA-384 SHALL additionally expose a non-consuming `checkpoint(self: &Self)` method that returns the digest for all bytes supplied so far while leaving the state available for subsequent updates and checkpoints.

#### Scenario: Hash incrementally through the inherent update method

- **WHEN** a mutable state receives a message through multiple `state.update(bytes)` calls and is finished
- **THEN** its digest equals a one-shot hash of the concatenated bytes in the same order

#### Scenario: Ignore empty updates

- **WHEN** empty updates occur before, between, or after non-empty updates
- **THEN** the resulting digest equals the digest produced without those empty updates

#### Scenario: Consume state at finalization

- **WHEN** source finishes an owned state and then attempts to use that state again
- **THEN** ownership analysis rejects the later use

#### Scenario: Checkpoint one transcript without consumption

- **WHEN** source checkpoints SHA-256 or SHA-384, then appends more bytes and checkpoints again
- **THEN** each digest matches its exact prefix and the original streaming state remains usable
