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
bindings and stable projected or materialized temporary roots SHALL use the same lexical loan
tracking as call arguments. Storing slices in owned values SHALL remain unsupported. Ordinary
functions MAY return one-source lexical views under the returned-view contract below.

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

### Requirement: Ordinary functions may return one-source lexical views

An ordinary function SHALL be permitted to return a shared or exclusive reference or slice view
only when the result is proven to originate from exactly one borrowed parameter. A shared returned
view MAY originate from `&T` or `&mut T`; an exclusive returned view MUST originate from `&mut T`.
Effect, service, interface, and other owned result contracts MUST NOT return borrowed views.

#### Scenario: Return a shared subview

- **WHEN** an ordinary function takes one shared slice parameter and returns a shared subview of it
- **THEN** the caller receives a lexical view whose origin and maximum lifetime are that parameter's source owner

#### Scenario: Reborrow an exclusive parameter as shared

- **WHEN** an ordinary function takes one exclusive slice parameter and returns a shared view of it
- **THEN** the returned shared view is accepted without granting exclusive access

#### Scenario: Reject exclusive strengthening

- **WHEN** an ordinary function takes only a shared parameter and attempts to return an exclusive view
- **THEN** analysis rejects the result because no exclusive origin exists

#### Scenario: Reject multiple possible origins

- **WHEN** a returned view may originate from either of two borrowed parameters
- **THEN** analysis rejects the function without inventing lifetime parameters or a merged origin

#### Scenario: Return a nominal reference through a pipeline

- **WHEN** `&mut counter |> increment` invokes a function whose exclusive reference result derives from that parameter
- **THEN** the pipeline result retains `counter` as its exact source root and the loan remains active through the result's last use

#### Scenario: Return a captured view from an exact callable section

- **WHEN** a known section captures the declaration's one returned-borrow parameter and a later exact application produces the result
- **THEN** the result retains that capture's loan rather than ending it at application

#### Scenario: Do not guess through an opaque callable

- **WHEN** only a structural callable contract with a borrowed result is known and no exact function item or section identifies its source
- **THEN** analysis does not infer provenance from arbitrary supplied arguments or captures

### Requirement: Returned views remain lexical and non-storable

A returned view SHALL be usable as a local lexical binding and as a compatible call-scoped reborrow.
Its lifetime MUST NOT exceed the lexical lifetime of its source owner. Lifetime-bearing references
and slices MUST remain forbidden in structs, arrays, unions, Effect success or failure values,
captures, and other owned storage.

#### Scenario: Use and release a returned local view

- **WHEN** a caller binds a returned view, reads it, and makes no later use of the view
- **THEN** the view's live range ends at its last use and the source owner becomes available under the ordinary borrow rules

#### Scenario: Reject escape from the owner

- **WHEN** control could preserve a returned view after its source owner's lexical scope ends
- **THEN** ownership rejects the escape at the boundary that would outlive the owner

#### Scenario: Reject storing a returned view

- **WHEN** source attempts to place a returned borrowed view in a struct field or array element
- **THEN** analysis retains the stored-borrow prohibition and reports that the lifetime-bearing value is not an owned field value

### Requirement: Value borrows preserve stable field projections

An ordinary call-scoped value borrow MAY project through resolved nominal fields rooted in a stable
local, pattern binding, or borrowed parameter. The borrow SHALL retain that field path rather than
copying the projected value, and evaluator, native, and direct-Wasm execution MUST address the same
projected storage. Exclusive projection through a parameter SHALL require an exclusive reference.

#### Scenario: Forward a shared field view

- **WHEN** an ordinary wrapper borrows one field through `&T` and forwards a returned shared view
- **THEN** the view remains tied to the wrapper owner and reads the field's underlying storage without copying

#### Scenario: Mutate through an exclusive field view

- **WHEN** an ordinary wrapper borrows one field through `&mut T` and forwards an exclusive view
- **THEN** mutations update only the projected field storage and preserve adjacent narrow scalar elements

#### Scenario: Reject exclusive projection through shared access

- **WHEN** source attempts an exclusive field borrow rooted in `&T`
- **THEN** semantic analysis rejects the borrow without synthesizing stronger access

### Requirement: Text-derived views use ordinary borrow provenance

References to `string`, slices of `string`, and UTF-8 byte views derived from a runtime `string`
SHALL use the same stable place, access, reborrow, last-use, return-origin, and escape rules as other
ordinary values and slices. A text-derived byte view SHALL remain `&[u8]` and SHALL retain the
backing text storage provenance without allocation or a text-specific lifetime exception.

#### Scenario: Borrow a string value

- **WHEN** source passes `&string`, `&mut string`, or `&[string]` through a compatible ordinary boundary
- **THEN** analysis applies the ordinary reference or slice rules without a text-specific diagnostic

#### Scenario: Return text bytes from one borrowed source

- **WHEN** an ordinary function returns a UTF-8 byte view derived from exactly one borrowed string parameter
- **THEN** the caller receives a lexical `&[u8]` tied to that parameter's source owner

#### Scenario: Reject escaping runtime text bytes

- **WHEN** a byte view derived from a local runtime string would outlive its backing owner
- **THEN** ownership reports the ordinary escaping-borrow diagnostic
