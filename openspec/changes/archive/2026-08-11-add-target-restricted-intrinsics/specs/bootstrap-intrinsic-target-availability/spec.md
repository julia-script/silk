## Purpose

Define generic, reachable-only target availability for sealed compiler intrinsics so portable
programs pay only for the platform primitives they actually execute.

## ADDED Requirements

### Requirement: Every intrinsic declares an enforced supported-target set

Each sealed intrinsic operation SHALL declare the evaluator and backend targets on which it is
available. Availability metadata SHALL be part of the intrinsic catalog contract and MUST be
validated consistently before evaluation or artifact emission. An intrinsic with no restriction
SHALL retain its existing all-target behavior.

#### Scenario: Admit an all-target intrinsic

- **WHEN** a reachable operation supports the requested evaluator or emission target
- **THEN** executable planning accepts the operation and continues normally

#### Scenario: Diagnose an unsupported reachable intrinsic

- **WHEN** a target-restricted operation is reachable for a target outside its supported set
- **THEN** planning emits one stable diagnostic naming the intrinsic and requested target before execution or emission

### Requirement: Availability is checked after executable reachability

Target validation SHALL inspect only intrinsic calls retained by the selected program's executable
closure. Merely parsing, loading, importing, or type-checking a declaration that mentions an
unsupported intrinsic MUST NOT reject an otherwise portable executable.

#### Scenario: Ignore an unreachable target-specific declaration

- **WHEN** the loaded module graph contains a function calling a native-only intrinsic but the function is absent from executable closure
- **THEN** a direct-Wasm request succeeds without a target-unavailable diagnostic

#### Scenario: Reject the same declaration when reachable

- **WHEN** the entry closure changes so that the native-only call becomes executable on direct Wasm
- **THEN** target validation rejects it with the stable intrinsic availability diagnostic

### Requirement: Unsupported intrinsics are pay-for-use in artifacts

An unreachable target-specific intrinsic SHALL contribute no runtime symbol, host import, adapter,
or backend support module to an emitted artifact. Equivalent closure and target inputs SHALL produce
deterministic availability results and artifact inventories.

#### Scenario: Emit portable Wasm without native imports

- **WHEN** a direct-Wasm program reaches no native-only intrinsic although canonical source contains such declarations
- **THEN** its module contains no imports or symbols for those operations

#### Scenario: Repeat target validation

- **WHEN** the same executable closure is planned repeatedly for the same target
- **THEN** diagnostics and retained intrinsic inventories are byte-for-byte deterministic
