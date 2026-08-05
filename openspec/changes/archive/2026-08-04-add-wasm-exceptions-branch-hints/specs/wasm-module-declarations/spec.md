# wasm-module-declarations Delta

## ADDED Requirements

### Requirement: Tags
The system SHALL declare exception tags referencing an interned function type with an empty
result sequence, SHALL support importing and exporting tags like any other entity, and SHALL
reject a tag whose type has results.

#### Scenario: Tag declared and exported
- **WHEN** a caller declares a tag with type `[i32] -> []` and exports it
- **THEN** emission produces a tag section entry and a tag export

#### Scenario: Tag with results rejected
- **WHEN** a caller declares a tag whose function type has a non-empty result sequence
- **THEN** the declaration fails with `WasmError`

#### Scenario: Imported tag is throwable
- **WHEN** a caller imports a tag and references its handle in a committed `throw`
- **THEN** emission resolves the throw to the imported tag's index, imports first
