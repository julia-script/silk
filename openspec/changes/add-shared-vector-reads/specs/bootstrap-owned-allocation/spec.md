## MODIFIED Requirements

### Requirement: Raw typed storage is narrow and unsafe

The language SHALL expose `RawBuffer<T>` as an affine typed view over one `Allocation` and one
compiler-validated repeated-element layout. Qualified unsafe operations SHALL construct a buffer,
project one bounds-checked lexical exclusive `Slot<T>`, initialize an uninitialized slot, move from
or destroy an initialized slot, inspect the logical count, and copy one initialized non-union Copy
element through a shared buffer borrow. The shared copy operation MUST NOT expose a Slot, move or
mutate the buffer, change initializedness or cleanup state, or allocate. The compiler SHALL validate
canonical type/layout provenance, checked index ordering, slot non-escape, exclusive owner liveness,
shared access, and legal element operations. Unsafe Silk code remains responsible for the runtime
initializedness and aliasing invariants; the runtime and compiler MUST NOT add a collection-shaped
initialization bitmap.

#### Scenario: Reject a slot after its buffer moves

- **WHEN** unsafe code retains a projected slot and attempts to move the backing raw buffer
- **THEN** ownership rejects the move before MIR or backend emission

#### Scenario: Reject mismatched typed provenance

- **WHEN** unsafe code attempts to form `RawBuffer<Token>` from a repeated layout planned for another canonical type
- **THEN** semantic analysis rejects the construction and publishes no usable buffer or slot fact

#### Scenario: Keep initializedness an unsafe obligation

- **WHEN** unsafe code reads or takes a value from a slot that its own runtime state has not initialized
- **THEN** the program violates the unsafe operation contract without gaining a compiler-promised initialization bitmap or safe behavior

#### Scenario: Read through two shared aliases

- **WHEN** unsafe code reads one initialized non-union Copy element through each of two live shared borrows of the same raw buffer
- **THEN** both reads return the stored value while buffer ownership, initializedness, and cleanup state remain unchanged

#### Scenario: Reject a shared read of an unsupported element

- **WHEN** unsafe code requests a shared raw-buffer read for a move-only or structural-union element type
- **THEN** compiler verification rejects the intrinsic instance before evaluation or backend emission
