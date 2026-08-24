## ADDED Requirements

### Requirement: Backend artifacts expose independent-execution pay-for-use evidence

Native and direct-Wasm inspection SHALL report deterministic structural presence or absence of
direct lowering, nested suspension runtime, explicit owner/package support, dormant continuation
support, wake-control support, notification support, and atomic/thread support for each complete
specialization. Evidence SHALL follow static reachability and explicit construction rather than
runtime branch outcomes or source actor names. The evidence MUST NOT prescribe byte counts,
instruction counts, field offsets, or a stable runtime ABI.

#### Scenario: Omit all suspension support

- **WHEN** a complete artifact reaches no suspension and constructs no Execution
- **THEN** inspection reports direct lowering and absence of every suspension and execution runtime slice

#### Scenario: Retain only nested suspension support

- **WHEN** a complete artifact reaches nested transfer but no explicit Execution or park
- **THEN** inspection reports the nested runner and absence of package, dormant owner, Wake, notification, and atomic support

#### Scenario: Retain explicit ownership without Wake

- **WHEN** an artifact constructs a statically non-parking Execution
- **THEN** inspection reports exact package and drive support while reporting no wake-control or external-park support

#### Scenario: Retain external parking statically

- **WHEN** an explicit Execution specialization can reach park on any path
- **THEN** inspection reports independent continuation, wake-control, and notification support even when the observed test path completes without parking

#### Scenario: Keep the local tier non-atomic

- **WHEN** all reachable Execution and Wake values remain in one local execution domain
- **THEN** inspection reports no atomic or cross-thread runtime support

## MODIFIED Requirements

### Requirement: Suspension runner ABIs remain private and pay for use

Coroutine frame headers, resume discriminants, step results, driver loops, target function
references, and execution-stack layouts SHALL remain backend-private and unreachable from Silk
source. A compiled program whose reachable MIR contains no suspension operation and no explicit
Execution construction MUST NOT emit or link those forms, a coroutine-frame or execution-stack
path, an Execution package/drive path, or a complete-versus-pending branch, and its established
synchronous entry and Effect-call artifact shape SHALL remain unchanged. Explicitly constructing a
non-suspending Execution SHALL retain its purpose-bound erased-body package and drive lifecycle while
omitting suspension frames, dormant continuation, Wake, notification, and atomic support.

#### Scenario: Inspect a non-suspending native artifact

- **WHEN** a closed synchronous Effect program with no explicit Execution construction is compiled to native release bitcode
- **THEN** structural inspection finds its established direct Effect calls and no suspension driver, coroutine frame, resume dispatch, execution-stack helper, Execution package, drive path, or pending branch

#### Scenario: Inspect a non-suspending Wasm artifact

- **WHEN** the same closed synchronous Effect program with no explicit Execution construction is compiled to direct WebAssembly
- **THEN** structural and linkage inspection finds no suspension table, driver, coroutine-frame path, execution-stack helper, Execution package, drive path, or pending branch

#### Scenario: Preserve explicit non-suspending ownership

- **WHEN** a statically non-suspending body is explicitly constructed as an Execution
- **THEN** structural inspection finds the owned erased-body package and drive lifecycle but no nested runner, dormant continuation, Wake, notification, or atomic support
