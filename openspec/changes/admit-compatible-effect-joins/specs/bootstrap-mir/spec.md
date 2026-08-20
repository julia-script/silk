## ADDED Requirements

### Requirement: MIR realizes and verifies finite Effect composites

MIR SHALL carry a closed composite Effect layout, an operation that packs one exact selected
alternative, selected-run dispatch, and active-alternative cleanup. Verification SHALL reject an
unknown alternative, a mismatched source representation, an incompatible normalized contract, or
cleanup metadata that can release an inactive alternative.

#### Scenario: Pack one selected alternative

- **WHEN** lowering reaches a branch that constructs one member of a finite Effect join
- **THEN** MIR packs that member with its canonical tag and does not construct the other members

#### Scenario: Clean only the active member

- **WHEN** a composite holding an affine capture is dropped or finishes running
- **THEN** MIR applies exactly the selected alternative's cleanup plan once

#### Scenario: Encode the composite deterministically

- **WHEN** equivalent joined Effects are lowered repeatedly
- **THEN** MIR layouts, alternative tags, operations, cleanup plans, and text are identical
