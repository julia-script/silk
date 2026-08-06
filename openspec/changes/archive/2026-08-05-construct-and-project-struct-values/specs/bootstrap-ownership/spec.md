## ADDED Requirements

### Requirement: Nominal struct bindings are move-only owners

Ownership checking SHALL classify every user-defined struct as move-only in this slice and SHALL
track whole-value ownership independently on each structured control-flow path. An explicit move of
a whole parameter or local SHALL transfer its cleanup obligation and end the source's liveness on
that path. A later use SHALL retain the existing use-after-move diagnostic behavior.

#### Scenario: Move one aggregate binding

- **WHEN** `let next = move current` transfers a struct value
- **THEN** `next` owns the value, `current` is dead after the move, and only `next` appears in later cleanup

#### Scenario: Preserve ownership across branch paths

- **WHEN** a struct is moved in one returning branch and remains live in another branch
- **THEN** each exit records the correct path-local owner without globally consuming the other path

### Requirement: Partial struct moves are rejected

Ownership checking MUST reject a consuming access whose subject is a field projection, because this
slice has neither complete destructuring nor a replacement operation that could restore a valid
whole value. Non-consuming reads of Copy scalar fields SHALL leave the enclosing owner live.

#### Scenario: Read then move the whole struct

- **WHEN** code reads a scalar field and later moves the complete struct
- **THEN** the field read leaves ownership unchanged and the later whole move succeeds

#### Scenario: Refuse a field move

- **WHEN** code evaluates `move value.field`
- **THEN** ownership produces one partial-move violation at that access and retains the whole owner's state

### Requirement: Aggregate cleanup is recursive and exact

The target-neutral cleanup plan SHALL represent one whole-value release for each live struct owner
and SHALL retain the canonical declaration-defined field cleanup order recursively. Lowering SHALL
materialize that plan exactly once on every return and arm exit; moved sources and Copy-only field
reads MUST NOT cause duplicate or omitted cleanup.

#### Scenario: Plan cleanup for a nested struct

- **WHEN** a nested aggregate remains live at return
- **THEN** the cleanup facts identify the outer owner and its recursive declaration-defined field order exactly once

#### Scenario: Omit a moved source from cleanup

- **WHEN** a parameter is moved into the returned aggregate
- **THEN** the parameter source has no exit release and the returned owner carries the obligation across the call boundary
