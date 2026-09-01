## ADDED Requirements

### Requirement: Nonprimitive operation modules expose importable scope actors

Each canonical nonprimitive standard-library operation module SHALL export an ordinary public
zero-data actor under the qualifier used to present that module's complete operation surface when
no existing declaration already provides that scope. Selecting the scope actor SHALL expose the
same public module operations under that qualifier as a namespace import, without compiler
privilege or a runtime representation.

#### Scenario: Select the RawBuffer scope actor

- **WHEN** source imports `silk.raw_buffer { RawBuffer }` and calls `RawBuffer.from<T>`
- **THEN** name resolution reaches the canonical ordinary-source `from` operation and reports no missing-member diagnostic

#### Scenario: Preserve an example qualifier

- **WHEN** a documented example replaces a redundant namespace import with a selected scope-actor import
- **THEN** every operation qualifier in the example remains unchanged and resolves to the same canonical module operation

#### Scenario: Keep primitive modules as namespaces

- **WHEN** source uses operations from `silk.u8`, `silk.u32`, or `silk.usize`
- **THEN** the canonical import is the unaliased module import and the lowercase primitive qualifier remains available

#### Scenario: Scope actors remain ordinary source

- **WHEN** tooling navigates an imported standard-library scope actor
- **THEN** it reaches a public zero-data declaration in canonical Silk source with no compiler-known actor or module-origin exception
