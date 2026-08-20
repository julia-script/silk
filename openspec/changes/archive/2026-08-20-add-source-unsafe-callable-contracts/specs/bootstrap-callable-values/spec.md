## ADDED Requirements

### Requirement: Source callables may carry an unsafe caller contract

Ordinary and effectful function declarations MAY be marked `unsafe`. Calling such a declaration SHALL require one lexical unsafe acknowledgement at the call site while preserving every ordinary type, Effect, ownership, requirement, target, and cleanup check.

#### Scenario: Call an unsafe source wrapper

- **WHEN** source calls an `unsafe fn` inside an unsafe acknowledgement
- **THEN** the call is accepted if all ordinary checks succeed

#### Scenario: Reject an unacknowledged call

- **WHEN** safe source directly calls an unsafe function
- **THEN** analysis reports the missing acknowledgement at the call

### Requirement: Unsafe qualification survives callable composition

Callable values, generic substitution, partial application, storage, returns, and interface operation contracts SHALL preserve unsafe qualification. A safe implementation MAY satisfy an unsafe operation contract, but an unsafe implementation SHALL NOT satisfy a safe contract.

#### Scenario: Partially apply an unsafe function

- **WHEN** source supplies a leading argument to an unsafe multi-parameter function
- **THEN** the resulting callable remains unsafe and its later invocation requires acknowledgement

#### Scenario: Keep checks active inside unsafe code

- **WHEN** an acknowledged unsafe call also violates borrowing or Effect requirements
- **THEN** analysis reports those ordinary violations rather than treating unsafe as a checking bypass
