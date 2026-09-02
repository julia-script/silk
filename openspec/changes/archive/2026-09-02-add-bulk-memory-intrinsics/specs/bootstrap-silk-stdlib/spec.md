## ADDED Requirements

### Requirement: The standard library exposes and uses the bulk primitives

`RawBuffer` SHALL expose the bulk move as `copy` and the bulk byte set as `fill`. `Vector` growth
SHALL migrate its initialized elements with one bulk move per migration, and `Bytes.append` SHALL
copy its borrowed byte sequence with one bulk move through a `u8`-concrete `Vector.appendBytes`
rather than one append per byte. The library MUST NOT expose a generic bulk append, which would let
safe code move a move-only element out of a borrowed slice.

#### Scenario: Grow a vector

- **WHEN** a vector grows past its capacity
- **THEN** its initialized elements migrate in one bulk move and the released buffer holds none of them

#### Scenario: Append a borrowed byte sequence

- **WHEN** `Bytes.append` is given a borrowed byte sequence
- **THEN** the bytes are appended in one bulk move and the caller's sequence is unchanged
