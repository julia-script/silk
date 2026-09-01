## ADDED Requirements

### Requirement: Tuple-backed and anonymous aggregates obey ordinary struct ownership

Named tuples and anonymous aggregate values SHALL use the existing whole-value nominal struct
ownership rules. Reads, moves, borrows, partial-move rejection, Copy evidence, mutation, and
structured-exit cleanup MUST NOT depend on whether the nominal declaration was written in source or
synthesized from a literal. Anonymous aggregate creation MUST NOT synthesize `Copy` evidence merely
because every current member is Copy.

Tuple cleanup order SHALL follow ordinal declaration order, and anonymous record cleanup order SHALL
follow the canonical source field order recorded by its synthesized declaration. A move of the
whole aggregate SHALL transfer exactly one recursive cleanup obligation; separate fields or
positions MUST NOT become independently movable unless ordinary struct ownership later admits that
operation for all nominal structs.

#### Scenario: Move one anonymous record as a whole

- **WHEN** a local anonymous record is moved into an owning generic call
- **THEN** the source binding becomes dead and the callee receives its one declaration-ordered cleanup obligation

#### Scenario: Refuse a positional partial move

- **WHEN** source requests a consuming move from one position of an affine named tuple
- **THEN** ownership rejects the partial move and retains the complete tuple owner's state

#### Scenario: Avoid implicit Copy for anonymous aggregates

- **WHEN** every field of an anonymous record is Copy but no nominal Copy evidence can be declared for its generated type
- **THEN** the record remains affine while non-consuming reads of its Copy fields follow ordinary struct rules
