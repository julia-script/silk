## ADDED Requirements

### Requirement: Initial test operations are evaluator-only and pay for use

The sealed inventory, metadata, invocation, and StackPath inspection operations SHALL declare
evaluation support for every semantic target the evaluator accepts and no native LLVM or direct
WebAssembly artifact-emission support in the initial slice. A reachable call during native or
direct-WebAssembly executable planning SHALL receive the existing stable intrinsic-availability
diagnostic before lowering or emission. An unreachable call or imported ordinary Test wrapper MUST
NOT reject an otherwise portable artifact or retain an adapter, runtime symbol, or host import.
Availability metadata SHALL be deterministic and MUST NOT change the target-neutral marker,
canonical ID, inventory order, handle, Outcome, path, cleanup, or trap semantics.

#### Scenario: Admit evaluation without an engine mode

- **WHEN** a test-aware project request evaluates any reachable inventory, metadata, invocation, or path-inspection operation for a semantic target supported by evaluation
- **THEN** availability accepts it and the evaluator applies the closed inventory and invocation contract

#### Scenario: Reject reachable artifact emission

- **WHEN** native LLVM or direct WebAssembly executable planning reaches a test inventory, metadata, invocation, or path-inspection operation
- **THEN** planning emits the stable intrinsic-availability diagnostic before MIR/backend lowering and produces no partial test artifact

#### Scenario: Ignore an unreachable testing wrapper

- **WHEN** an ordinary artifact build loads canonical or user source that mentions testing operations but none is retained by the executable closure
- **THEN** the build receives no availability diagnostic and retains no inventory, adapter, test runtime symbol, or host import

#### Scenario: Repeat evaluator-only availability

- **WHEN** the same operation catalog and executable closure are planned repeatedly for evaluation and artifact emission
- **THEN** supported-target metadata, diagnostics, and retained intrinsic inventories are byte-for-byte deterministic
