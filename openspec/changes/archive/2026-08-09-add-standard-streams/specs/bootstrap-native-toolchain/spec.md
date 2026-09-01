## ADDED Requirements

### Requirement: Finalization preserves stream requirements

Native finalization SHALL connect an explicit process-stream provider. WebAssembly finalization SHALL retain and expose the required host import in inspection data. Neither path SHALL add an implicit Logger or console dependency.

#### Scenario: Run native output

- **WHEN** a native program is finalized with the process provider
- **THEN** running it emits the exact requested bytes to the selected destination

#### Scenario: Inspect a Wasm requirement

- **WHEN** a Wasm program requires `StandardStreams`
- **THEN** finalization preserves the import required for instantiation
