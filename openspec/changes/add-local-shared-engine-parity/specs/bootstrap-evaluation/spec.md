## ADDED Requirements

### Requirement: Evaluation is the deterministic local shared ownership oracle

The evaluator SHALL execute verified local-shared layout, initialization, clone, callback access,
conflict, and drop operations using logical block identity rather than JavaScript object identity or
garbage collection. It SHALL retain the concrete target layout, bounded strong count, independent
available-or-active access state, initialized `T`, and private active reclaim authority. Clone SHALL
check before mutation and SHALL fatally trap at the selected target maximum without wrapping,
saturating, storing a count, or returning a partial handle; conflict SHALL leave active access unchanged; normal access SHALL end its
borrow before restoring availability; last drop SHALL clean `T` before releasing storage.

Evaluation SHALL expose bounded deterministic events sufficient to distinguish initialization,
clone, access acquisition, conflict, access restoration, non-last decrement, payload cleanup, and
allocation release. Strong cycles SHALL remain retained without synthesized collection. Fatal traps
SHALL preserve the language's no-unwind contract.

#### Scenario: Evaluate sequential shared mutation

- **WHEN** two handles sequentially inspect, mutate, and inspect one local shared counter
- **THEN** evaluation returns the expected values and records one access acquisition and restoration per successful callback

#### Scenario: Observe conflict without releasing access

- **WHEN** an active callback attempts reentrant access through an alias and then continues using its original borrow
- **THEN** evaluation selects conflict, records no access-state transition for it, and keeps the outer borrow valid until its normal return

#### Scenario: Clone and drop during access

- **WHEN** a callback clones another live alias and drops a non-last handle
- **THEN** evaluation changes only the strong count while access remains active and performs no payload cleanup

#### Scenario: Trap clone before overflow mutation

- **WHEN** evaluation observes the selected target's maximum strong count
- **THEN** it returns the fatal no-unwind trap before count mutation and publishes no new or partial handle

#### Scenario: Clean the last handle in order

- **WHEN** the final handle to an affine payload is dropped
- **THEN** evaluation records payload cleanup exactly once before one allocation release

#### Scenario: Retain a strong cycle

- **WHEN** every external handle to a two-block strong cycle is dropped
- **THEN** evaluation records the external decrements but no payload cleanup or release for either retained block
