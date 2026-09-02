## ADDED Requirements

### Requirement: Box is canonical ordinary Silk source

Canonical standard-library source SHALL define `Box<T>`, one owned heap indirection holding exactly
one value of `T`, as ordinary Silk with no compiler privilege. No compiler phase SHALL know `Box` by
name: it MUST NOT appear in the intrinsic nominal registry, gain a layout branch, gain a cleanup
plan node, or be recognized by spelling in semantic analysis, HIR, MIR, evaluation, or a backend. It
SHALL be built from the existing typed raw storage and slot primitives, mirroring how `Vector<T>` is
built.

`Box<T>` SHALL be move-only for every `T` through the ordinary nominal ownership rule, with no
ownership special case. Its construction SHALL allocate through the `Allocator` service and SHALL
fail with `OutOfMemoryError`. Its `Drop` hook SHALL take exclusive `self`, return unit, and declare no
failures and no requirements, dropping the held value before its storage releases. A boxed value
SHALL be reachable by shared borrow, exclusive borrow, and consuming move without unsafe code at the
call site.

`Box<T>` SHALL give trees only. It MUST NOT provide shared ownership, reference counting, or a way
to construct a cyclic graph.

#### Scenario: Hold a recursive type

- **WHEN** a struct declares a field of `Box` applied to the struct's own type
- **THEN** the declaration is accepted and the layout is finite, without the compiler recognizing `Box` by name

#### Scenario: Report allocation exhaustion

- **WHEN** the allocator cannot satisfy a box construction
- **THEN** the caller receives the ordinary `OutOfMemoryError` typed failure and no partially initialized box exists

#### Scenario: Release the held value with the box

- **WHEN** a box holding a value that owns resources leaves scope
- **THEN** the held value is dropped before the box's storage releases, and the release count equals the acquire count

#### Scenario: Move a box without copying

- **WHEN** a box is assigned or passed by value
- **THEN** ownership transfers under the ordinary move-only rules and the source is no longer live
