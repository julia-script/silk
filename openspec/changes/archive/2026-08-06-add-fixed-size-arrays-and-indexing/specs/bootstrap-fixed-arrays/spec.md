## Purpose

Define fixed-size inline arrays as canonical Silk values with complete initialization, checked indexed places, element-derived ownership, and deterministic runtime behavior.

## ADDED Requirements

### Requirement: Array identity includes element type and length

`Array<T, N>` SHALL identify one inline array type by the canonical element type `T` and one
non-negative decimal length `N`. Length SHALL be part of type equality, function contracts, struct
fields, instance keys, and deterministic encodings. Zero-length and recursively nested arrays SHALL
remain valid when their element type is valid and their target layout is finite.

#### Scenario: Distinguish lengths

- **WHEN** one contract names `Array<I32, 3>` and another names `Array<I32, 4>`
- **THEN** they resolve to distinct canonical types and are not assignment- or call-compatible

#### Scenario: Retain a zero-length element type

- **WHEN** a contract names `Array<Token, 0>`
- **THEN** the canonical type retains `Token` and length zero even though the value has no elements

### Requirement: Array literals create complete values

An array literal SHALL evaluate each written element exactly once from left to right and create one
complete value in ascending index order. A non-empty literal SHALL infer one common available element
type and its written length when no contextual array type is required. A contextual array type SHALL
require the exact element type and length. An empty literal SHALL require contextual element type.
Invalid elements or length disagreement SHALL preserve each element fact but produce no partial value.

#### Scenario: Infer a non-empty literal

- **WHEN** `let values = [10, 20, 30]` has three compatible `I32` elements
- **THEN** `values` has canonical type `Array<I32, 3>` and stores the values at indices zero through two

#### Scenario: Construct an empty contextual array

- **WHEN** a function returning `Array<Token, 0>` returns `[]`
- **THEN** the literal constructs the complete zero-length value with canonical element type `Token`

#### Scenario: Refuse a contextual length mismatch

- **WHEN** an `Array<I32, 3>` position receives `[1, 2]`
- **THEN** semantic facts retain both elements and report the expected and actual lengths without constructing a value

### Requirement: Indexing is a checked place projection

`subject[index]` SHALL require an available array subject and an `I32` index and SHALL produce a
canonical indexed place whose element type comes from the subject type. A statically known negative or
out-of-range literal SHALL be a semantic failure. Every dynamic index SHALL be checked at runtime and
trap before projection when it is negative or not less than the canonical length. Index and field
projections SHALL compose left-to-right without fabricating an intermediate copied aggregate.

#### Scenario: Read a dynamic scalar element

- **WHEN** `values[index]` indexes `Array<I32, 4>` with a runtime `I32`
- **THEN** execution checks `0 <= index < 4` and returns exactly the selected scalar or traps at that projection

#### Scenario: Project a Copy leaf through an aggregate element

- **WHEN** `pairs[index].left` indexes `Array<Pair, 4>` and `left` is `I32`
- **THEN** the place chain reads the selected Copy field without copying or consuming the `Pair` or array owner

### Requirement: Array ownership and cleanup derive from elements

An array SHALL be Copy exactly when its element type is Copy; otherwise it SHALL be a move-only
whole value. Moving or returning one indexed non-Copy element SHALL be rejected as a partial move,
while moving the complete array SHALL transfer one ownership obligation. Cleanup SHALL visit live
elements exactly once in ascending index order and recursively use each element's canonical cleanup.

#### Scenario: Copy a scalar array

- **WHEN** a bound `Array<I32, 3>` is passed to an array parameter
- **THEN** the value is copied and the source remains live

#### Scenario: Reject moving one struct element

- **WHEN** code requests `move values[index]` from `Array<Token, 3>`
- **THEN** ownership rejects the partial move and retains the whole array's explicit ownership state

#### Scenario: Clean up nested elements

- **WHEN** a live `Array<Token, 3>` exits its scope
- **THEN** cleanup visits indices zero, one, and two exactly once using `Token`'s declaration-defined cleanup
