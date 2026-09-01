# bootstrap-intrinsic-target-availability Specification

## Purpose

Define generic, reachable-only target availability for sealed compiler intrinsics so portable
programs pay only for the platform primitives they actually execute.

## Requirements

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

Target validation SHALL inspect only intrinsic calls retained after each reachable concrete
application has been statically specialized and the resulting executable closure has been closed.
Merely parsing, loading, importing, indexing, or retaining an uncalled declaration that mentions an
unsupported intrinsic MUST NOT reject an otherwise portable executable. A call in an unselected
`static if` arm MUST NOT enter semantic call facts, the executable closure, target validation, or a
backend inventory.

#### Scenario: Ignore an unreachable target-specific declaration

- **WHEN** the loaded module graph contains a function calling a native-only intrinsic but the function is absent from the executable closure
- **THEN** a direct-WebAssembly request succeeds without a target-unavailable diagnostic

#### Scenario: Ignore an inactive target-specific arm

- **WHEN** a reachable function places a native-only intrinsic in the arm not selected for direct WebAssembly
- **THEN** specialization omits that call and target validation succeeds without retaining native support

#### Scenario: Reject the same declaration when reachable

- **WHEN** the selected static arm and executable closure retain a native-only intrinsic for direct WebAssembly
- **THEN** target validation rejects the concrete residual specialization with the stable intrinsic-availability diagnostic

#### Scenario: Keep residual availability deterministic

- **WHEN** identical source, target, generic arguments, evidence, and static arguments are compiled in fresh processes
- **THEN** the retained intrinsic inventory and target-availability diagnostics are byte-for-byte identical

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

### Requirement: Selective catch is executable on every supported target

`Intrinsic.catchFailure` SHALL belong to the ordinary executable intrinsic inventory for the
evaluator, WebAssembly, and native targets. A valid direct or wrapped selective-catch dependency
SHALL pass target availability and reach the same specialized MIR semantics on every supported
target. No catch-specific `AnalysisOnly` state or `SEM0098` availability diagnostic SHALL remain.

Target availability SHALL remain derived from the canonical intrinsic operation rather than from
standard-library wrapper spelling. Invalid syntax, kind, inference, or constraint diagnostics SHALL
prevent invalid calls from reaching target selection or lowering.

#### Scenario: Admit a reachable wrapper on every target

- **WHEN** a reachable ordinary `Effect.catch` wrapper expands to `Intrinsic.catchFailure`
- **THEN** evaluator, WebAssembly, and native target selection all admit the dependency

#### Scenario: Admit the direct intrinsic on every target

- **WHEN** a valid reachable call names `Intrinsic.catchFailure` directly
- **THEN** it has the same executable target set as the ordinary wrapper

#### Scenario: Reject invalid calls before target selection

- **WHEN** selective catch has invalid syntax, generic kinds, inference, or singleton membership
- **THEN** the corresponding semantic diagnostic is reported and no catch-specific availability diagnostic is emitted
