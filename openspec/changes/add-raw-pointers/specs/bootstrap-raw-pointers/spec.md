## Purpose

Define raw pointers: the `*const T` and `*mut T` types that hold one machine address with no
ownership, borrow, or validity guarantee, the sealed primitives that form and dereference them, and
the rules that make native writes through them observable.

## ADDED Requirements

### Requirement: Raw pointer types hold one un-owned address

`*const T` and `*mut T` SHALL be types for any concrete pointee `T`, including a struct with no
fields used as an opaque handle. Pointer identity SHALL include the canonical pointee and the
mutability. A raw pointer value SHALL be Copy, MAY be null, SHALL own nothing, SHALL hold no loan,
and SHALL carry no guarantee that its address is valid, aligned, initialized, or still allocated.
`*mut T` SHALL convert to `*const T` at an immediate expected-type boundary; no other implicit
conversion, arithmetic, comparison, or cast SHALL exist on pointers.

#### Scenario: Copy a pointer freely

- **WHEN** a binding of type `*mut i32` is read twice and passed to two calls
- **THEN** analysis accepts both uses without a move and records no cleanup for the binding

#### Scenario: Widen mutability at a boundary

- **WHEN** a `*mut u8` value is passed to a `*const u8` parameter
- **THEN** the call is accepted; passing `*const u8` to `*mut u8` is rejected with the ordinary type mismatch

#### Scenario: Point at an opaque type

- **WHEN** a module declares `struct Opaque {}` and uses `*mut Opaque` in a foreign signature
- **THEN** analysis accepts the pointer without requiring the pointee to be admitted by the foreign ABI

### Requirement: Pointer primitives are sealed and split by safety

The compiler SHALL expose exactly these pointer primitives through `Intrinsic`, and the module
`silk/pointer` SHALL expose them as the ordinary `Pointer` API: `null<T>() -> *mut T`,
`isNull(pointer: *const T) -> bool`, `fromRef(value: &T) -> *const T`, `fromMutRef(value: &mut T)
-> *mut T`, `fromSlice(values: &[T]) -> *const T`, `fromMutSlice(values: &mut [T]) -> *mut T`,
`offset(pointer: *const T, count: usize) -> *const T` and `offsetMut(pointer: *mut T, count:
usize) -> *mut T` advancing by `count` elements of `T`, `read(pointer: *const T) -> T`, and
`write(pointer: *mut T, value: T) -> ()`. `null`, `isNull`, and the four formation primitives SHALL
be safe. `offset`, `offsetMut`, `read`, and `write` SHALL be unsafe and SHALL each state their
caller invariant. The `Pointer` API SHALL bound `read` and `write` to a Copy pointee so a
move-only pointee is rejected at the call, and MIR verification SHALL reject a read or write
operation whose pointee is not Copy. Every primitive SHALL be available on the evaluator, LLVM,
and Wasm.

#### Scenario: Form a pointer safely

- **WHEN** safe code calls `Pointer.fromMutRef(&mut value)`
- **THEN** analysis accepts the call without an unsafe boundary and the result type is `*mut` of the referent type

#### Scenario: Require unsafe to dereference

- **WHEN** safe code calls `Pointer.read(pointer)`
- **THEN** analysis reports the existing unsafe-acknowledgement diagnostic

#### Scenario: Reject a move-only read

- **WHEN** unsafe code calls `Pointer.read` on a `*const Vector<i32>`
- **THEN** analysis rejects the call at the `Pointer.read` bound naming the Copy requirement

#### Scenario: Read what was written

- **WHEN** unsafe code writes `7` through `Pointer.fromMutRef(&mut x)` and reads `x`
- **THEN** every execution surface observes `7`

### Requirement: A dangling pointer is an unsafe obligation, not a compile error

Forming a pointer SHALL NOT keep its root alive or be rejected when the root's scope ends; the
ownership rules for formation are stated in the ownership capability. Dereferencing after the root
is gone is an unsafe-contract violation.

#### Scenario: Return a pointer to a local

- **WHEN** a function returns `Pointer.fromRef(&local)` for one of its own locals
- **THEN** analysis accepts the function; dereferencing the result is the caller's unsafe obligation

### Requirement: Native writes through a formed pointer are observable

A place from which a pointer has been formed SHALL have authoritative memory storage for the rest
of its live range, as every borrowed root has today, and every foreign call SHALL reload such
places afterwards exactly as synchronous Silk calls do: a subsequent Silk read of the place SHALL
observe bytes written through the pointer by native code during the call. The compiler MUST NOT
cache the place's value across a foreign call.

#### Scenario: Observe a C write through a scalar pointer

- **WHEN** Silk forms `Pointer.fromMutRef(&mut result)` where `result: i32`, passes it to a C function that stores `42`, and then reads `result`
- **THEN** the native executable observes `42`

#### Scenario: Observe a C write through a slice pointer

- **WHEN** Silk passes `Pointer.fromMutSlice(&mut bytes)` and `bytes.length` to a C function that fills the buffer, then reads `bytes[0]`
- **THEN** the native executable observes the filled byte

### Requirement: The evaluator models pointers as logical addresses

The evaluator SHALL represent a raw pointer as null, as a logical address naming a frame, cell,
place path, and element offset, or as an allocation ticket and element offset for pointers formed
from a raw-buffer-backed view, and SHALL execute every pointer primitive with the same observable
results as the native backends for programs that reach no foreign call. A `read`, `write`, or
`offset` through null or through an address whose frame has ended SHALL stop execution as an
unsafe-contract violation reported as data.

#### Scenario: Evaluate a pointer round trip

- **WHEN** the evaluator runs a program that forms a pointer from a mutable array slice, offsets it by two, writes, and reads `array[2]`
- **THEN** the read observes the written value

#### Scenario: Evaluate a pointer into a raw buffer

- **WHEN** the evaluator forms `Pointer.fromSlice(RawBuffer.view(&buffer, 0, 3))`, offsets by one, and reads
- **THEN** the read observes the buffer's second element

#### Scenario: Detect a dangling dereference

- **WHEN** the evaluator dereferences a pointer to a cell whose frame has returned
- **THEN** execution stops with an unsafe-contract violation naming the primitive, not a host exception
