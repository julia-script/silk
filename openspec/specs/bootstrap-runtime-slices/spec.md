# bootstrap-runtime-slices Specification

## Purpose

Define explicit lexical views over runtime-length contiguous input without transferring ownership,
exposing raw pointers, or requiring dynamic allocation.

## Requirements

### Requirement: Slice types make borrowed access explicit

Slice types SHALL accept shared `&'a [T]` and exclusive `&'a mut [T]` forms and their elided forms. Semantic identity SHALL retain the lifetime, canonical element type, and access mode without a fixed source-array length. Lazy or capturing functions SHALL admit lifetime-bearing captures only when their environment bounds preserve source validity and access; new borrowed outcomes remain gated until outcome checking is admitted.

#### Scenario: Declare shared and exclusive slice parameters

- **WHEN** ordinary functions declare `values: &[i32]` and `values: &mut [i32]`
- **THEN** both parameter types resolve with canonical element `i32`, distinct access modes, and no fixed length

#### Scenario: Preserve a slice on a lazy function boundary

- **WHEN** a lazy or capturing function form declares or captures a slice
- **THEN** analysis retains its environment lifetime and rejects execution or escape beyond the borrowed storage's validity

#### Scenario: Reject a slice on a lazy function boundary

- **WHEN** a lazy computation retains a slice beyond its source validity
- **THEN** analysis rejects the escape while preserving its environment lifetime witness

### Requirement: Whole-array borrowing is explicit and call-scoped

An explicit `&array` SHALL create a shared slice and `&mut array` an exclusive slice over initialized stable fixed-array storage. Formation SHALL require sufficient source validity for all retained uses, and exclusive formation SHALL require mutable access. Direct bindings, stable projected places, and materialized temporary owners SHALL follow the same lifetime and loan rules. There SHALL be no implicit fixed-array-to-slice conversion. Shared slice locals, ordinary aggregate storage, generic propagation, and declared ordinary-function results SHALL preserve semantic lifetime arguments; views spanning a missing element SHALL be rejected.

#### Scenario: Borrow two array lengths for one function

- **WHEN** one slice-taking function is called as `fold(&short)` and `fold(&long)` for live fixed arrays with different lengths
- **THEN** both explicit borrows satisfy the same slice parameter type without converting either owner

#### Scenario: Reject implicit array decay

- **WHEN** a fixed array is passed directly to a slice parameter without `&` or `&mut`
- **THEN** the call remains incompatible and reports the missing explicit borrow

#### Scenario: Retain a standalone slice binding

- **WHEN** a binding initializer attempts `let view = &values`
- **THEN** analysis admits the local view and retains its source place and lifetime through every dependent use

#### Scenario: Reject a standalone slice binding

- **WHEN** a standalone slice would span a missing array element or outlive its backing storage
- **THEN** analysis rejects the invalid view while retaining its source place and reason

### Requirement: Slice parameters support compatible call-scoped reborrows

A shared slice parameter SHALL be forwardable only as a shared call-scoped reborrow. An exclusive
slice parameter SHALL support shared or exclusive call-scoped reborrowing; the parent exclusive
access SHALL be suspended for the complete nested call and restored only when that call and every retained child dependent have ended. An
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

### Requirement: Ordinary functions return declared lifetime-bearing values

An ordinary function SHALL return a shared or exclusive reference, slice, or shared borrowed aggregate when its declared lifetime relationships are satisfied by its body and concrete callers. Explicit relationships SHALL support multiple possible sources and independently named stored lifetimes; shared results SHALL derive only shared permission and exclusive results SHALL require exclusive permission. Structural callable and interface operation contracts SHALL carry those same relationships without requiring an exact implementation body at application. Effect success and failure borrows SHALL remain gated until outcome checking is admitted. References into invalid locals, expired temporaries, or by-value inline parameter storage SHALL be rejected; forwarding external data retained by a by-value wrapper SHALL remain valid.

#### Scenario: Return a shared subview

- **WHEN** an ordinary function takes one shared slice parameter and returns a shared subview of it
- **THEN** the caller receives a lexical view whose origin and maximum lifetime are that parameter's source owner

#### Scenario: Reborrow an exclusive parameter as shared

- **WHEN** an ordinary function takes one exclusive slice parameter and returns a shared view of it
- **THEN** the returned shared view is accepted without granting exclusive access

#### Scenario: Reject exclusive strengthening

- **WHEN** an ordinary function takes only a shared parameter and attempts to return an exclusive view
- **THEN** analysis rejects the result because no exclusive origin exists

#### Scenario: Admit declared multiple possible origins

- **WHEN** a returned view may originate from either of two borrowed parameters whose explicit signature shares one result lifetime
- **THEN** analysis accepts the function and requires every possible source to remain valid for the returned view's required uses

#### Scenario: Return a nominal reference through a pipeline

- **WHEN** `&mut counter |> increment` invokes a function whose exclusive reference result derives from that parameter
- **THEN** the pipeline result retains `counter` as its exact source root and the loan remains active through the result's last use

#### Scenario: Return a captured view from an exact callable section

- **WHEN** a known section captures the declaration's one returned-borrow parameter and a later exact application produces the result
- **THEN** the result retains that capture's loan rather than ending it at application

#### Scenario: Use a structural callable lifetime contract

- **WHEN** a structural callable contract declares its borrowed input/output lifetime relationships without an exact function item or section
- **THEN** analysis instantiates the declared relationships against supplied arguments and preserves the callable environment bound without inspecting a body

#### Scenario: Return stored data independently of wrapper storage

- **WHEN** a getter returns an explicitly named data lifetime from a borrowed holder, while another getter returns a reference to the holder's own index
- **THEN** the data view can survive moving or dropping the wrapper while the index reference prevents invalidating its wrapper place

#### Scenario: Reject multiple possible origins

- **WHEN** a result can derive from two independent borrowed inputs but its signature declares only one input as supporting its lifetime
- **THEN** the body fails its declared relationship rather than manufacturing a merged public source contract

#### Scenario: Do not guess through an opaque callable

- **WHEN** a structural callable has an ambiguous undeclared result lifetime and no deterministic elision default
- **THEN** analysis rejects the incomplete contract without inspecting hidden bodies or captures

### Requirement: Returned views preserve validity through ordinary storage

A returned view SHALL be usable in local bindings, compatible reborrows, shared borrowed aggregate storage, generic payloads, and valid callable or Effect captures. Every nested lifetime SHALL remain visible to compatibility, ownership, and escape checking. Retained uses MUST NOT exceed referent validity. Exclusive stored borrows, dependent user Drop, and borrowed Effect success or failure values SHALL remain explicitly gated until their respective proofs are implemented.

#### Scenario: Use and release a returned local view

- **WHEN** a caller binds a returned view, reads it, and makes no later use of the view
- **THEN** the view's live range ends at its last use and the source owner becomes available under the ordinary borrow rules

#### Scenario: Reject escape from the owner

- **WHEN** control could preserve a returned view after its source owner's lexical scope ends
- **THEN** ownership rejects the escape at the boundary that would outlive the owner

#### Scenario: Store a shared returned view

- **WHEN** source attempts to place a shared returned view in a struct field or array element
- **THEN** analysis accepts the lifetime-bearing payload and retains the view's loans through its containing value's uses

#### Scenario: Reject storing a returned view

- **WHEN** storing a view would permit a later use after its source storage becomes invalid
- **THEN** analysis rejects the escape and identifies the retaining aggregate path

### Requirement: Value borrows preserve stable field projections

An ordinary call-scoped value borrow MAY project through resolved nominal fields rooted in a stable
local, pattern binding, or borrowed parameter. The borrow SHALL retain that field path rather than
copying the projected value, and native and LLVM-generated WebAssembly execution MUST address the same
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

### Requirement: Value-reference parameters support compatible call-scoped reborrows

Borrowing a value-reference parameter for a nested call SHALL form a call-scoped reborrow. A shared
parent SHALL yield only a shared child. An exclusive parent SHALL yield a shared or exclusive child,
suspend conflicting use of the parent for the complete call, and restore it afterward. Reborrowing
SHALL preserve the original backing identity and SHALL NOT strengthen access.

#### Scenario: Reborrow an exclusive Formatter repeatedly

- **WHEN** an exclusive Formatter parameter is passed by exclusive reborrow to sequential helpers
- **THEN** each child borrow ends with its call
- **AND** the parent Formatter is available for the next helper

#### Scenario: Share an exclusive parent temporarily

- **WHEN** an exclusive reference parameter is shared-reborrowed for a nested call
- **THEN** the parent is suspended during that call and restored afterward

#### Scenario: Reject access strengthening

- **WHEN** source requests an exclusive child from a shared reference parameter
- **THEN** ownership analysis rejects the reborrow
