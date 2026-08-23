## ADDED Requirements

### Requirement: Independent executions retain exact affine ownership and loan boundaries

Ownership SHALL treat every available `Intrinsic.Execution<A>` as one affine, non-Copy,
non-thread-transferable obligation independent of `A`. Moving it SHALL end the source and transfer
the same obligation; completion or ordinary drop SHALL discharge it exactly once. Construction
MUST reject an executable or fixed endpoint that retains an external lexical or provider loan.
Loans created after activation MAY cross parking only when their referents are owned inside the same
Execution and retain stable logical locations; cleanup SHALL end each loan before its referent.
Completion MUST reject an `A` that borrows body, frame, endpoint, or package storage that completion
will clean.

#### Scenario: Move one execution owner

- **WHEN** an Execution moves from a source binding into owner storage
- **THEN** the source ends and the destination retains exactly one non-Copy execution obligation

#### Scenario: Reject duplicate execution use

- **WHEN** source attempts to drive or drop an Execution after it was moved into a prior drive
- **THEN** ordinary ownership reports use-after-move and publishes no second activation obligation

#### Scenario: Retain an internal loan across parking

- **WHEN** an activated execution creates a loan into a value it owns, parks, and later resumes
- **THEN** the loan keeps a stable logical referent and dormant cleanup ends it before cleaning the owned referent

#### Scenario: Reject an external loan at construction

- **WHEN** an Effect or endpoint passed toward Execution construction retains a caller lexical or provider loan
- **THEN** ownership preserves the loan cause and the Detached obligation is unsatisfied before erasure

#### Scenario: Reject a completion result borrowing package state

- **WHEN** `A` would contain a loan into the body environment, continuation frames, endpoint, or combined package
- **THEN** ownership rejects the escaping result before construction or drive can erase it

#### Scenario: Reject thread transfer

- **WHEN** a future or unsafe-adjacent operation attempts to transfer an Execution across local execution domains without a parallel-memory contract
- **THEN** the canonical local-affinity fact prevents the transfer and no atomic semantics are implied
