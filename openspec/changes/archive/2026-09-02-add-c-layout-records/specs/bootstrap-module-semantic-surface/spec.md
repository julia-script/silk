## ADDED Requirements

### Requirement: Module surfaces preserve C-layout record contracts

A module semantic surface SHALL encode a public struct's physical-layout contract, including ABI `C` when present. Encoding, decoding, exact equality, and dependency invalidation SHALL distinguish an ordinary struct from a C-layout struct with the same name and fields.

#### Scenario: Round-trip an imported C-layout record

- **WHEN** a module exports a C-layout record and a dependent imports it through a serialized semantic surface
- **THEN** the dependent observes the same C-layout ABI and ordered field contract

#### Scenario: Invalidate after changing the layout promise

- **WHEN** a public ordinary struct changes to an otherwise identical C-layout struct
- **THEN** its semantic surface changes and direct dependents are selected for recomputation
