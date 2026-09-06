## ADDED Requirements

### Requirement: Receiver loans preserve stored borrowed access modes

Implicit and written receiver borrows SHALL establish equivalent concrete wrapper loans and retain the access modes of borrowed data stored inside the wrapper. Exclusive access to a wrapper SHALL NOT imply exclusive access to a shared slice's backing owner. Genuine overlapping wrapper or backing-owner conflicts SHALL remain rejected.

#### Scenario: Repeatedly advance a shared slice holder

- **WHEN** a mutable holder of a shared slice advances its index through repeated receiver calls
- **THEN** the calls are accepted exactly as their qualified explicit-borrow forms are
- **AND** a live conflicting borrow of the wrapper or mutation of its retained backing owner is rejected

### Requirement: Binding initializer array borrows retain lexical hidden owners

A borrowed array temporary evaluated within a local binding initializer SHALL materialize a hidden local owner with the ownership behavior of an equivalently named array at the same evaluation point. The array SHALL be evaluated exactly once in source order with unchanged element inference. Storage SHALL remain valid through dependent local views, aggregates and delayed computations, including suspension. The hidden owner SHALL belong to the originating block, selected branch or loop iteration and SHALL use ordinary initialization and cleanup on normal completion, early structured exits and interruption. Affine elements SHALL be cleaned exactly once after their dependent loans end. This rule SHALL NOT permit references to function-local storage to escape or change fatal-trap no-unwind semantics.

#### Scenario: Construct a holder from a temporary array

- **WHEN** let mut stream = SliceStream.make(&[1, 2, 3]) is followed by consumed stream calls
- **THEN** the hidden array remains valid for those calls and uncontextualized elements retain their ordinary i32 inference

#### Scenario: Preserve evaluation and lexical boundaries

- **WHEN** an initializer contains earlier side effects or a borrowed array in a selected branch or repeated loop body
- **THEN** the producer runs exactly once at its original position only when selected and its storage is cleaned within that branch or iteration

#### Scenario: Suspend and interrupt a dependent holder

- **WHEN** a holder of hidden array storage remains live across suspension and execution completes or is interrupted
- **THEN** the backing storage survives all retained uses and its affine cleanup runs exactly once in ordinary dependency order

#### Scenario: Reject hidden local escapes

- **WHEN** a function returns a slice, aggregate or retained Effect borrowing its hidden array storage
- **THEN** lifetime checking rejects the escape as it would for an explicitly named local array
