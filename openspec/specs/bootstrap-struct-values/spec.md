# bootstrap-struct-values Specification

## Purpose

Define complete runtime nominal struct values with module-owned construction, typed field access,
whole-value affine ownership, aggregate calls and returns, and deterministic cleanup.

## Requirements

### Requirement: Raw struct construction belongs to the defining module

A raw struct literal SHALL name one canonical nominal struct and SHALL be legal only in that
struct's defining module. Other modules MUST construct the value through ordinary visible functions.
Every literal SHALL initialize every declared field exactly once; source field order MAY differ from
declaration order, but the resulting value SHALL use canonical declaration order. Unknown,
duplicate, missing, inaccessible, and mistyped field initializers SHALL remain explicit invalid
states without creating a partially initialized value.

#### Scenario: Construct every field out of order

- **WHEN** a defining module constructs `Pair { right: 2, left: 1 }` for fields declared as `left` then `right`
- **THEN** construction succeeds and the semantic value records `left` then `right` under `Pair`'s canonical nominal identity

#### Scenario: Construct an empty marker

- **WHEN** a defining module evaluates `End {}` for an empty struct
- **THEN** it creates one complete zero-field nominal value

#### Scenario: Refuse external raw construction

- **WHEN** another module attempts `Token { kind: 1 }` for an imported public `Token`
- **THEN** construction is unavailable with a stable defining-module-boundary diagnostic even if every field is public

#### Scenario: Preserve an invalid field set

- **WHEN** a literal omits one field, repeats another, or names an unknown field
- **THEN** each supplied initializer and its exact failure remain queryable and no complete runtime value is produced

### Requirement: Field projection is a canonical typed place

A field projection SHALL resolve its subject's nominal type and one field identity from that
struct's completed declaration. Projection SHALL associate left-to-right so nested fields remain a
chain of canonical place facts. A field SHALL be readable outside its defining module only when it
is public. Projecting a non-struct value, an unknown field, a private external field, or a subject
whose type is unavailable SHALL retain an explicit failed projection and exact cause.

#### Scenario: Read a scalar field

- **WHEN** `pair.left` projects an `I32` field from an available `Pair`
- **THEN** the projection identifies `Pair.left` canonically and produces an `I32` value without consuming `pair`

#### Scenario: Project through nested structs

- **WHEN** `token.span.start` follows two available nominal fields
- **THEN** both projection steps retain their canonical field identities and the final expression has the declared scalar type

#### Scenario: Refuse a private external field

- **WHEN** another module projects a default-private field of a public struct
- **THEN** the field candidate and use-site provenance remain visible but no readable place or value is produced

### Requirement: Bootstrap structs move only as whole values

Every user-defined struct value SHALL be move-only in this slice. Moving a whole struct SHALL
consume its source binding, and a later read or move SHALL be rejected. Reading a Copy scalar field
SHALL copy that field without consuming the owner. Moving an individual field out of a struct MUST
be rejected as a partial move; struct patterns, replacement operations, and user-declared `Copy`
remain outside this capability. A consuming let binding, call argument, or return of a bound struct
SHALL require explicit `move`; a fresh literal or call result MAY flow directly because it has no
source binding that could be copied accidentally.

#### Scenario: Move a complete struct through a call

- **WHEN** a caller passes `move token` to an owning nominal parameter
- **THEN** the callee receives the complete value and every later use of the caller's `token` is a use-after-move

#### Scenario: Return a newly constructed struct

- **WHEN** a factory returns a complete struct literal
- **THEN** the value crosses the return boundary as one owned nominal value without an intermediate partial state

#### Scenario: Refuse an implicit nominal copy

- **WHEN** a bound struct is passed to an owning parameter without `move`
- **THEN** ownership rejects the transfer and leaves the source binding live

#### Scenario: Refuse moving one nested field

- **WHEN** code attempts `move outer.inner`
- **THEN** ownership rejects the partial move while leaving `outer`'s whole-value state explicit

### Requirement: Struct cleanup follows declared ownership

A live owned struct SHALL clean up its fields in the language's declaration-defined cleanup order,
recursively using each field type's cleanup behavior, exactly once on every structured exit. A moved
whole value SHALL transfer that obligation to its destination and SHALL not be cleaned up at the
source. Empty and scalar-only structs SHALL still retain explicit complete cleanup facts even when
they require no runtime release operation.

#### Scenario: Transfer cleanup with a whole move

- **WHEN** a live struct binding is moved into another binding before return
- **THEN** only the destination owns the declaration-ordered field cleanup obligation at the exit

#### Scenario: Clean up a nested aggregate

- **WHEN** a live outer struct owns an inner struct at a structured exit
- **THEN** cleanup visits the outer fields and recursively the inner fields in their specified declaration-defined order exactly once
