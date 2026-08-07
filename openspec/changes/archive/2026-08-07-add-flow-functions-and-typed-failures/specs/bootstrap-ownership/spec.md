## ADDED Requirements

### Requirement: Flow capture and failure transfer obey ordinary ownership

Flow construction SHALL retain moved and borrowed capture obligations without executing the body.
Running transfers or borrows captures according to the original call, `fail move` consumes its
payload, propagation transfers it once, and recovery gives one owned payload to the matching
handler. Cleanup SHALL occur exactly once for values in every region actually exited.

#### Scenario: Reject a second run after a taken capture

- **WHEN** a closed flow captures an affine argument by move and is run twice
- **THEN** ownership rejects the second run and identifies the consumed capture

#### Scenario: Clean before propagation

- **WHEN** a flow fails after constructing a live affine local
- **THEN** cleanup leaves the exited region before the owned failure reaches its caller
