## ADDED Requirements

### Requirement: Raw buffers expose only minimal unsafe initialized views

The sealed `Intrinsic` namespace SHALL provide target-neutral unsafe operations that form shared and
exclusive byte slices from a live `RawBuffer`, an offset, and a length within its initialized
extent. The operations MUST NOT recognize `Vector` or any other source-defined owner, allocate,
copy, resize, or decide public collection policy. Ordinary Silk wrappers SHALL establish the
initialized-range and aliasing preconditions before invoking them.

#### Scenario: Form a shared initialized view

- **WHEN** an ordinary source wrapper proves that an offset and length are within a live raw buffer's initialized extent
- **THEN** the shared intrinsic returns a view over exactly that range without allocation or copying

#### Scenario: Form an exclusive initialized view

- **WHEN** an ordinary source wrapper holds exclusive access to a live raw buffer and supplies a valid initialized range
- **THEN** the exclusive intrinsic returns the corresponding exclusive view without naming the wrapper type

#### Scenario: Keep collection policy out of the compiler

- **WHEN** a source-defined owner uses the raw-buffer view operations
- **THEN** semantic analysis and lowering depend only on `RawBuffer` and slice invariants, not on the owner's declaration spelling
