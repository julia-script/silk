## ADDED Requirements

### Requirement: Every local shared core handle is one affine obligation

For every available `Intrinsic.SharedCore<T>` specialization, ownership SHALL classify the handle as
affine regardless of whether `T` is Copy. A whole-handle move SHALL transfer exactly one live
`LocalSharedStrong` obligation and end the source; ordinary reads or structural derivation MUST NOT
duplicate the handle. The contained `T` SHALL keep its ordinary ownership category and MUST NOT be
copied, moved, or cleaned merely because a handle moves. Ownership facts SHALL retain the
`LocalExecution` affinity established by semantic analysis. An unavailable element specialization
SHALL retain its causal diagnostic and unavailable ownership verdict rather than fabricate a Copy,
unrestricted, or satisfied result. Aggregate ownership SHALL retain one distinct obligation for
each structurally live core handle; a structural union SHALL retain only the obligations of its
active member.

#### Scenario: Move one core handle

- **WHEN** a local shared core handle moves from one binding to another in the same local execution
- **THEN** the source becomes dead, the destination owns the same single `LocalSharedStrong` obligation, and no operation on `T` is planned

#### Scenario: Reject a non-consuming handle read in ownership

- **WHEN** source attempts a non-consuming read that would duplicate a local shared core handle
- **THEN** ownership publishes an `OWN0003` violation at the attempted read, retains the affine handle fact, and publishes no duplicated obligation

#### Scenario: Reject Copy conformance before ownership

- **WHEN** source declares `impl Copy` for a nominal containing a local shared core handle
- **THEN** conformance validation publishes `SEM0083` at the implementation declaration, admits no Copy evidence, and ownership continues to classify available values of that nominal as affine

#### Scenario: Keep a Copy element behind an affine handle

- **WHEN** the core element type is `i32`
- **THEN** the core retains one `LocalSharedStrong` obligation and moving it does not copy the stored integer

#### Scenario: Specialize generic ownership independently of the element

- **WHEN** one generic wrapper over `Intrinsic.SharedCore<T>` is specialized with a Copy `T` and with an affine `T`
- **THEN** each available specialization owns exactly one affine `LocalSharedStrong` obligation and neither specialization owns or duplicates `T` through the handle

#### Scenario: Retain a handle inside a local executable

- **WHEN** a handle moves into an ordinary callable or Effect that remains within one local execution
- **THEN** ownership transfers exactly one obligation into the environment, ends the source, and preserves `LocalExecution` affinity

#### Scenario: Retain every handle obligation in aggregate storage

- **WHEN** a nominal, fixed array, callable, or Effect stores two independently live local shared core handles
- **THEN** ownership retains two distinct `LocalSharedStrong` obligations, while a structural union containing such values retains only the obligations of its active member

#### Scenario: Retain a handle across local suspension and resumption

- **WHEN** a handle moves through suspension, parking, resumption, or between independently resumable frames in one same-thread local execution domain
- **THEN** the source frame ends when moved, the destination frame retains `LocalExecution` affinity and exactly one live `LocalSharedStrong` obligation, and no park or resume creates or discharges an obligation

#### Scenario: Preserve unavailable element ownership

- **WHEN** ownership receives `Intrinsic.SharedCore<Missing>` with causal element-resolution diagnostics
- **THEN** it retains an unavailable verdict and those causes without publishing a Copy category, unrestricted affinity, satisfied verdict, or live handle obligation
