# Bootstrap Scalar Enums Specification

## Purpose

Define closed nominal scalar enums, their fixed-width representations, member values, conversions,
operators, matching behavior, ownership, diagnostics, and runtime invariants across Silk targets.

## Requirements

### Requirement: Scalar enums declare one closed nominal value set

A scalar enum SHALL declare one non-empty, source-ordered set of uniquely named, fieldless members.
Only a qualified path `EnumName.Member` SHALL construct one allocation-free value of that enum's
canonical nominal type; an unqualified member name SHALL NOT resolve as enum construction. Enum types SHALL remain distinct even when their representations, member
names, and discriminants are identical. The enum declaration's visibility SHALL govern every member;
members SHALL NOT have independent visibility.

#### Scenario: Construct nominally distinct members

- **WHEN** two visible enums each declare a member named `Ready` with discriminant `0`
- **THEN** each qualified member path has its own enum type and neither value is accepted where the other enum is required

#### Scenario: Reject an empty enum

- **WHEN** an enum declaration contains no members
- **THEN** analysis reports the dedicated empty-enum diagnostic at the declaration and retains unrelated declarations

#### Scenario: Reject a duplicate member name

- **WHEN** one enum declares the same member name twice
- **THEN** analysis reports the duplicate at the later name with the first name's span related

### Requirement: Scalar enum representations are fixed-width integers

An omitted representation SHALL mean exactly `u8`. An explicit representation SHALL be one of `u8`,
`u16`, `u32`, `u64`, `i8`, `i16`, `i32`, or `i64`; no target-width integer, alias, nominal wrapper,
or non-integer type SHALL be accepted. The representation SHALL NOT be inferred from member values.

#### Scenario: Default to u8 without inference

- **WHEN** `enum Direction { North, East }` is analyzed
- **THEN** its representation fact is exactly `u8` and remains `u8` as long as every discriminant fits

#### Scenario: Require explicit widening

- **WHEN** an enum with omitted representation requires a discriminant greater than `255`
- **THEN** analysis reports a `u8` range or implicit-overflow diagnostic rather than inferring a wider representation

#### Scenario: Reject a target-width representation

- **WHEN** an enum representation is `usize` or `isize`
- **THEN** analysis reports the dedicated unsupported-representation diagnostic at that type span

### Requirement: Discriminants form one checked declaration-order sequence

The first implicit member discriminant SHALL be `0`. Every subsequent implicit discriminant SHALL be
the immediately preceding member's discriminant plus one, including after an explicit discriminant.
An explicit discriminant SHALL initially be an optionally negative decimal integer literal. Every
value and implicit successor SHALL be checked with host-independent integer arithmetic against the
selected representation.

#### Scenario: Continue after an explicit discriminant

- **WHEN** an enum declares `First`, `Second = 5`, and `Third` in that order
- **THEN** their discriminants are `0`, `5`, and `6`

#### Scenario: Reject a negative unsigned discriminant

- **WHEN** an enum represented by `u8` declares a member with discriminant `-1`
- **THEN** analysis reports the dedicated unsigned-negative diagnostic at the signed literal

#### Scenario: Reject explicit range overflow

- **WHEN** an `i8` enum explicitly declares discriminant `128`
- **THEN** analysis reports the dedicated explicit out-of-range diagnostic at that literal

#### Scenario: Reject implicit successor overflow

- **WHEN** a `u8` enum member with discriminant `255` is followed by an implicit member
- **THEN** analysis reports the dedicated implicit-overflow diagnostic at the implicit member

#### Scenario: Reject a duplicate discriminant

- **WHEN** explicit or implicit assignment gives two members the same numeric discriminant
- **THEN** analysis reports the duplicate at the later member with the first member declaration's span related

### Requirement: Scalar enum layout exactly matches its representation

Every valid scalar enum SHALL have exactly the size, alignment, and calling shape of its selected
representation integer, with no hidden metadata. This rule SHALL hold for default enums, explicit
representations, and one-member enums. Logical and source-facing facts SHALL retain the nominal enum
type despite the shared physical layout.

#### Scenario: Lay out a one-member default enum

- **WHEN** a valid default enum declares one member
- **THEN** its layout and calling shape equal `u8`, including a size of one byte, while its logical type remains the enum

#### Scenario: Lay out a signed wide enum

- **WHEN** a valid enum selects `i64`
- **THEN** every target uses the canonical `i64` size, alignment, and calling lane without extra tag storage

### Requirement: Scalar enums are sealed Copy values

Every valid scalar enum SHALL be `Copy`, SHALL carry no cleanup obligation, and SHALL NOT admit a
user `Copy` or `Drop` implementation or conformance. Copying or moving an enum SHALL follow the
existing rules for compiler-proved Copy scalars.

#### Scenario: Reuse a copied enum binding

- **WHEN** a function reads one enum binding into another value and then reads the original again
- **THEN** ownership analysis accepts both reads and schedules no cleanup for either enum value

### Requirement: Value exposes the exact backing integer

For an enum `E` with representation `R`, the declaration-generated wrapper `E.value(value)` SHALL
accept exactly a value of `E` and return its declared discriminant as `R` through the sealed
`Intrinsic.enumValue` primitive. The operation SHALL be total, allocation-free, failure-free, and
requirement-free. Silk SHALL provide no built-in integer-to-enum conversion and
SHALL NOT implicitly convert in either direction.

#### Scenario: Read a signed discriminant

- **WHEN** `Status.Unknown` has discriminant `-1` under `enum(i8)`
- **THEN** `Status.value(Status.Unknown)` has type `i8` and evaluates to `-1`

#### Scenario: Reject an integer where an enum is required

- **WHEN** an integer of the representation type is passed where the enum type is expected
- **THEN** analysis reports the dedicated integer-to-enum type diagnostic and does not construct an enum value

#### Scenario: Reject an enum where an integer is required

- **WHEN** an enum value is used as an integer without `EnumName.value`
- **THEN** analysis reports the dedicated enum-to-integer type diagnostic

### Requirement: Enum equality requires one nominal enum type

`==` and `!=` SHALL accept scalar enum operands only when both operands have the same canonical enum
type and SHALL compare their member identities. No enum SHALL implicitly compare with an integer or a
different enum. `<`, `<=`, `>`, and `>=` SHALL reject enum operands; numeric ordering SHALL require
explicit conversion through `value`.

#### Scenario: Compare two members of one enum

- **WHEN** equality compares two values of the same enum type
- **THEN** it evaluates true exactly when both values name the same declared member

#### Scenario: Reject equality between distinct enums

- **WHEN** equality receives operands from two different enum declarations with identical representations
- **THEN** analysis reports the dedicated cross-enum equality diagnostic

#### Scenario: Reject direct enum ordering

- **WHEN** an ordering operator receives enum operands
- **THEN** analysis reports the dedicated enum-ordering diagnostic and requires explicit backing-value comparison

### Requirement: Enum matching covers canonical member identity

A match over a scalar enum SHALL accept only qualified member patterns from that exact canonical enum
and `_`; an unqualified member name SHALL NOT select an enum member. Coverage SHALL begin with the enum's complete ordered member set. Each unguarded member arm SHALL
remove that member; `_` SHALL remove every remaining member. A match without `_` SHALL be accepted
only when no member remains. Patterns SHALL bind no payload and SHALL NOT narrow the scrutinee to an
integer or member subtype.

#### Scenario: Exhaust all members explicitly

- **WHEN** a three-member enum match has one unguarded qualified arm for each member
- **THEN** coverage is complete without `_` and the scrutinee retains its enum type

#### Scenario: Cover remaining members with a wildcard

- **WHEN** one member arm is followed by `_`
- **THEN** the wildcard covers every other enum member and every following arm is diagnosed as unreachable

#### Scenario: Report missing members

- **WHEN** a match has neither `_` nor an arm for every member
- **THEN** analysis reports the dedicated non-exhaustive diagnostic with the canonical missing members and match span

#### Scenario: Reject a duplicate member arm

- **WHEN** the same enum member appears in two unguarded arms
- **THEN** analysis reports the later arm as unreachable with the first arm's span related

#### Scenario: Reject a foreign member pattern

- **WHEN** a match over one enum names a qualified member of another enum
- **THEN** analysis reports the dedicated foreign-enum-pattern diagnostic at that member path

#### Scenario: Reject an integer enum pattern

- **WHEN** a match over an enum uses an integer literal pattern equal to one member's discriminant
- **THEN** analysis reports the dedicated integer-pattern-against-enum diagnostic

### Requirement: Only declared members inhabit safe scalar enums

Every enum value produced by well-typed Silk source SHALL correspond to exactly one declared member.
HIR, MIR, and LLVM lowering SHALL preserve or verify that member identity. Physical
integer lowering SHALL NOT create a source-level path for arbitrary representation values to inhabit
the enum.

#### Scenario: Preserve member identity through execution

- **WHEN** a member value is copied, passed through a function, compared, converted with `value`, and matched
- **THEN** analysis, native execution, and LLVM-generated WebAssembly agree on the same declared member and discriminant
