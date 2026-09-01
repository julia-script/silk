## ADDED Requirements

### Requirement: Backends consume validated reachable intrinsic inventories

Each backend SHALL receive the exact intrinsic inventory retained by executable planning for its
selected target. A reachable unsupported intrinsic MUST be rejected before partial artifact
construction. An unreachable restricted intrinsic MUST NOT cause the backend to link a runtime
symbol, emit an import, or bundle a host adapter.

#### Scenario: Reject before constructing an artifact

- **WHEN** executable planning finds one reachable operation unsupported by the selected backend target
- **THEN** backend execution is not entered and no partial native or Wasm artifact is returned

#### Scenario: Omit unreachable native runtime support

- **WHEN** LLVM or direct Wasm receives a validated inventory without a native-only operation
- **THEN** the emitted artifact and link plan contain no runtime symbol or import for that operation

#### Scenario: Preserve explicit backend selection

- **WHEN** the same target is supported by more than one backend
- **THEN** availability validation uses the explicitly selected backend request without silently selecting another implementation
