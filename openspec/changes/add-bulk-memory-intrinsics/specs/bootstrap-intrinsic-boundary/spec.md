## ADDED Requirements

### Requirement: Two unsafe bulk raw-storage primitives are admitted

The sealed `Intrinsic` namespace SHALL expose one unsafe operation that moves a caller-proven
initialized range of elements into raw storage, taking an exclusive `RawBuffer<T>` borrow, a
destination element offset, a shared `&[T]` source range, and an element count; and one unsafe
operation that sets a caller-proven byte range of a `RawBuffer<u8>` to one repeated byte value. Both
SHALL be callable only inside an `unsafe` block, and the compiler MUST NOT admit a resize, a public
release, or a safe public bulk API alongside them.

#### Scenario: Move a range between two buffers

- **WHEN** source calls the copy primitive with a source range viewed from one buffer and an exclusive borrow of another
- **THEN** every element in the range is transferred in one operation and no per-element primitive is required

#### Scenario: Set a byte range

- **WHEN** source calls the set primitive with a byte offset, a byte length, and a byte value
- **THEN** exactly the selected bytes hold that value and the surrounding bytes are unchanged

### Requirement: A bulk copy moves ownership and defines overlap

The copy primitive SHALL transfer ownership of every element in the range. A structurally Copy
element type SHALL leave the moved-from range readable, and a move-only element type SHALL give up
its moved-from slots. An overlapping source range and destination range SHALL produce the result of
copying through an intermediate buffer, and the compiler MUST NOT emit a runtime overlap check or a
diagnostic for one.

#### Scenario: Give up move-only source slots

- **WHEN** a range of move-only elements is copied out of raw storage
- **THEN** the moved-from slots hold no value and reading one traps

#### Scenario: Copy an overlapping range

- **WHEN** the source range and the destination range of one copy overlap
- **THEN** the result equals the result of copying the whole range through an intermediate buffer
