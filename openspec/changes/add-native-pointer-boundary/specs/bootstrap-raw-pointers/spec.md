## MODIFIED Requirements

### Requirement: Raw pointer types hold one un-owned address

Raw pointers SHALL follow the qualified forms in native-pointer-boundary: non-null single `*const T`/`*mut T`, non-null many `[*]const T`/`[*]mut T`, explicit nullable `?` prefixes, minimum alignment and ordinary data address space zero. Identity SHALL retain all axes and invariant pointee identity. A pointer SHALL be Copy, own nothing and hold no loan. Nullability and alignment SHALL state representational guarantees without proving initialization, liveness or ownership. Implicit conversions SHALL only weaken access, nullability or proven alignment and SHALL preserve pointee and extent.

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

The compiler SHALL expose minimal sealed primitives for null construction/testing, reference/slice/raw-slot address formation, qualified pointer conversion, many-item indexed address calculation and Copy reads/writes. Ordinary source wrappers SHALL own null-to-Option checking and output initialization-state policy. Null construction SHALL only produce nullable pointers. Reference and raw-slot formation SHALL produce non-null single pointers; slice formation SHALL produce explicit many pointers without retaining length. Indexed access SHALL require a non-null many pointer and an unsafe bounds/liveness proof and SHALL produce a single-object pointer. Dereference SHALL require non-null pointers, explicit unsafe acknowledgement and Copy pointees. Unaligned operations SHALL accept byte-aligned pointers and preserve the LLVM alignment guarantee. Unsafe qualifier strengthening SHALL preserve pointee and address space and state caller proof obligations. Every admitted primitive SHALL remain available through LLVM for native and WebAssembly targets.

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
