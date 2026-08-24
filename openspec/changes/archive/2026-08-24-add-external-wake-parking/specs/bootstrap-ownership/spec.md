## ADDED Requirements

### Requirement: Parking and wake retain exact generation obligations

Ownership SHALL create one affine Wake obligation per park generation and transfer it to the
registration callback. It SHALL retain returned `G`, endpoint state `O`, reusable endpoint callback
`R`, and live frame values in the Dormant Execution. Wake consumption or drop SHALL discharge the
Wake obligation once. Resumption SHALL clean `G` immediately before source continuation; dormant
destruction SHALL cancel first and then clean `G`, endpoint, body, and frames in dependency order.
Notification SHALL borrow `O` and `R` under a transient retain; reentrant destruction MUST defer
their cleanup until that borrow ends. An internal loan SHALL end before its owned referent on every
destroy path.

#### Scenario: Transfer Wake into registration state

- **WHEN** park invokes registration and the callback stores Wake in source state
- **THEN** the callback source ends, source state owns the sole Wake obligation, and the Execution retains exactly one `G`

#### Scenario: Consume Wake once

- **WHEN** source calls `Intrinsic.wake(move wake)`
- **THEN** the binding ends and no second live Wake obligation exists for that generation

#### Scenario: Resume cleanup order

- **WHEN** an Eligible execution is driven through the saved park continuation
- **THEN** ownership cleans `G` once, ends any loans held by `G`, and only then continues source after park

#### Scenario: Dormant cleanup order

- **WHEN** a Dormant execution is dropped while Wake remains external
- **THEN** cancellation precedes endpoint/frame cleanup, internal loans end before referents, and only inert Allocation authority remains with Wake

#### Scenario: Protect endpoint borrows during reentrant destruction

- **WHEN** endpoint invocation borrows `O` and reentrant source destroys the Execution
- **THEN** ownership records deferred cleanup and does not end the endpoint borrow or clean `O` or `R` until invocation returns
