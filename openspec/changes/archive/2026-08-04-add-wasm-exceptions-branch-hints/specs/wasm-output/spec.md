# wasm-output Delta

## ADDED Requirements

### Requirement: Exception and branch-hint encodings

The system SHALL encode the tag section and tag import/export descriptors, `try_table` with its
catch-clause list, `throw`/`throw_ref`, the `exnref` type, and tag names in the `name` custom
section; and SHALL emit the `metadata.code.branch_hint` custom section, immediately before the
code section, carrying each hinted instruction's function index and byte offset. All new forms
SHALL satisfy the same determinism, oracle-validation, and text-to-binary round-trip guarantees
as the baseline output.

#### Scenario: Exception forms round-trip

- **WHEN** a module using tags, `try_table`, and `throw` is rendered as text and assembled by
  the pinned oracle
- **THEN** the resulting bytes equal the builder's binary encoding

#### Scenario: Branch hints round-trip

- **WHEN** a module with hinted branches is rendered as text with hint annotations and
  assembled by the pinned oracle
- **THEN** the resulting bytes equal the builder's binary encoding, including the hint custom
  section

#### Scenario: No hints, no section

- **WHEN** a module contains no hinted instructions
- **THEN** no `metadata.code.branch_hint` section is emitted
