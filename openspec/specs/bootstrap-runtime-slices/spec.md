# bootstrap-runtime-slices Specification

## Purpose

Define explicit lexical views over runtime-length contiguous input without transferring ownership,
exposing raw pointers, or requiring dynamic allocation.

## Requirements

### Requirement: Slice types make borrowed access explicit

An ordinary function parameter SHALL accept the shared slice type `&[T]` or the exclusive slice
type `&mut [T]`, where `T` is one available concrete or generic element type. Slice type identity
SHALL include the canonical element type and access mode but SHALL NOT include the source array
length. Slice parameters MUST NOT be accepted on lazy or capturing function forms in this bootstrap
slice.

#### Scenario: Declare shared and exclusive slice parameters

- **WHEN** ordinary functions declare `values: &[i32]` and `values: &mut [i32]`
- **THEN** both parameter types resolve with canonical element `i32`, distinct access modes, and no fixed length

#### Scenario: Reject a slice on a lazy function boundary

- **WHEN** a lazy or capturing function form declares or captures a slice
- **THEN** analysis rejects the escaping boundary without inventing ownership for the borrowed storage

### Requirement: Whole-array borrowing is explicit and call-scoped

A call argument `&array` SHALL create a shared slice and `&mut array` SHALL create an exclusive
slice over the complete stable fixed-array root. The operand MUST be a direct live array binding
whose lifetime encloses the complete call, and exclusive formation MUST additionally require a
mutable root. There SHALL be no implicit fixed-array-to-slice conversion. Standalone slice local
bindings, borrowing array subplaces or temporaries, returning slices, and storing slices in owned
values SHALL remain unsupported.

#### Scenario: Borrow two array lengths for one function

- **WHEN** one slice-taking function is called as `fold(&short)` and `fold(&long)` for live fixed arrays with different lengths
- **THEN** both explicit borrows satisfy the same slice parameter type without converting either owner

#### Scenario: Reject implicit array decay

- **WHEN** a fixed array is passed directly to a slice parameter without `&` or `&mut`
- **THEN** the call remains incompatible and reports the missing explicit borrow

#### Scenario: Reject a standalone slice binding

- **WHEN** a binding initializer attempts `let view = &values`
- **THEN** analysis rejects the unsupported lifetime-bearing local while retaining the borrow syntax and source place

### Requirement: Slice parameters support compatible call-scoped reborrows

A shared slice parameter SHALL be forwardable only as a shared call-scoped reborrow. An exclusive
slice parameter SHALL support shared or exclusive call-scoped reborrowing; the parent exclusive
access SHALL be suspended for the complete nested call and restored when that call returns. An
exclusive slice MUST NOT be copied or forwarded as an independent alias.

#### Scenario: Reborrow exclusive storage through a helper

- **WHEN** a function holding `values: &mut [i32]` invokes an ordinary helper with a compatible exclusive reborrow
- **THEN** the helper writes the same backing array, the parent slice is inaccessible during the call, and parent access resumes afterward

#### Scenario: Refuse an access-strengthening reborrow

- **WHEN** a shared slice parameter is used where `&mut [T]` is required
- **THEN** analysis rejects the call without producing an exclusive view

### Requirement: Slice length and indexing are safe runtime operations

Slice `length` and indices SHALL use target-selected `usize`. Access SHALL check `index < length` and trap before projection or replacement evaluation. Zero-length and zero-sized-element slices SHALL retain their logical lengths.

#### Scenario: Traverse a slice

- **WHEN** a `usize` cursor indexes a runtime-length slice
- **THEN** each in-range access selects the corresponding element without signed bounds logic

#### Scenario: Trap before replacement

- **WHEN** an exclusive index equals or exceeds length
- **THEN** execution traps before evaluating the replacement

### Requirement: Borrowed element access preserves ownership

Indexing a shared slice SHALL produce a shared element place, and indexing an exclusive slice SHALL
produce an exclusive element place. Reading SHALL materialize a value only when the selected value
is Copy; field projection SHALL preserve the place access mode. Exclusive assignment MAY replace a
complete element or writable projection, but moving a non-Copy element or field out of either slice
MUST be rejected because it would leave borrowed storage partially initialized.

#### Scenario: Inspect a Copy field of a move-only element

- **WHEN** a shared `&[Token]` indexes one `Token` and projects its `i32` kind field
- **THEN** the field is copied without copying or moving the borrowed `Token`

#### Scenario: Replace through an exclusive slice

- **WHEN** `values: &mut [Token]` assigns a complete new `Token` to an in-range index
- **THEN** the original owner observes the replacement and the displaced element is cleaned exactly once

#### Scenario: Reject moving from borrowed storage

- **WHEN** source attempts to move one non-Copy element or field out of a slice
- **THEN** ownership rejects the partial move and keeps the backing owner's cleanup obligation intact
