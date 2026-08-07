## ADDED Requirements

### Requirement: MIR represents slice loans in the structured control DAG

Concrete monomorphic slice types SHALL remain logical shared or exclusive slice types in MIR. Slice
formation SHALL identify one stable backing place, loan identity, access mode, element type, and
lexical region; loan endings SHALL be explicit ordered facts on every structured exit. These
operations SHALL remain inside the existing acyclic operation and region structure, including when
loops later repeat through lexical outcomes, and MUST NOT expose a source-level raw pointer.

#### Scenario: Lower a call-scoped borrow

- **WHEN** HIR passes one whole-array borrow to an ordinary function
- **THEN** MIR orders slice formation before the call and the matching loan end after the call in the same structured region

#### Scenario: End an iteration-local loan before repetition

- **WHEN** a loop body forms a call-scoped slice and reaches `continue`
- **THEN** the loan ends before the loop's lexical repeat outcome without introducing a cyclic MIR edge

### Requirement: MIR slice places derive bounds from one slice value

Length, check, read, projection, and write operations for a slice SHALL derive the backing address,
runtime length, access mode, and element type from the same verified slice local. Runtime indexing
MUST use unsigned `I32` comparison semantics so negative values and values at or above length trap.
An exclusive write SHALL validate its destination before evaluating the replacement and SHALL
commit only after displaced-value cleanup.

#### Scenario: Verify one runtime-bounded place

- **WHEN** MIR reads `slice[index].field`
- **THEN** verification proves that the check and place projection use the same slice local and canonical element-field path

#### Scenario: Reject mismatched slice bounds

- **WHEN** malformed MIR attempts to check against one slice but address through another slice or a fixed constant
- **THEN** MIR verification reports the inconsistency before evaluation or backend emission

### Requirement: MIR verifies loan conflicts and cleanup order

MIR verification SHALL reject an owner move, direct access, write, or drop that conflicts with a
live loan; duplicate exclusive access; a missing or duplicate loan end; and cleanup scheduled before
the last applicable loan end. Shared and exclusive slices SHALL have the same runtime shape even
though their verified access permissions differ.

#### Scenario: Reject owner cleanup during a loan

- **WHEN** malformed MIR drops an array root before ending its live slice loan
- **THEN** verification identifies the owner, loan, and invalid operation without delegating borrow safety to a backend
