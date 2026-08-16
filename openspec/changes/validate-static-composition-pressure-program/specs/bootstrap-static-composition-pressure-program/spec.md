## Purpose

Define the executable ordinary-source integration and characterization gates that prove static
representation-dependent composition works across the complete Silk compiler.

## ADDED Requirements

### Requirement: A complete static-composition fixture uses final syntax

The repository SHALL contain one formatter-stable `static-composition-acceptance.silk` program using
final representation, opaque-result, conditional-conformance, and complete-interface syntax. It
SHALL define mapped reusable schemas, static subcommands, help traversal, branch normalization to one
application union, and handler invocation without compiler-known CLI or schema spelling.

#### Scenario: Decode one selected leaf
- **WHEN** the fixture selects and decodes one valid subcommand
- **THEN** only that leaf executes and the common application handler runs exactly once

#### Scenario: Avoid the handler on non-success paths
- **WHEN** the fixture requests help or encounters selection or decode failure
- **THEN** it traverses inspectable data as required and never invokes the application handler

### Requirement: Branch normalization precedes convergence

The fixture SHALL use supported named callables or sections so different representation-dependent
branch values are consumed before their common result joins. It MUST NOT rely on anonymous functions,
implicit erasure, heterogeneous executable collections, or runtime interface dispatch.

#### Scenario: Converge on an application action
- **WHEN** distinct leaf decoders produce different domain actions
- **THEN** each branch maps to the shared application union before the following pipeline node

### Requirement: Every engine agrees on static composition

Evaluator, native LLVM, and direct WebAssembly SHALL produce equal observable results and typed
failure outcomes. The evaluator SHALL expose handler, failure, suspension, selected-target, and
cleanup trace counts; native LLVM and direct WebAssembly artifacts SHALL prove the selected direct
target and a causal cleanup path for the corresponding execution. WebAssembly output MUST contain
no function table or `call_indirect` introduced by this capability.

#### Scenario: Run the acceptance matrix
- **WHEN** success, help, selection failure, decode failure, uncalled cleanup, and called cleanup
  cases run through all engines
- **THEN** their results agree, evaluator traces satisfy the exact contract, and backend artifacts
  retain the corresponding static targets and cleanup witnesses

### Requirement: Static-tree growth is characterized deterministically

Generated left-associated and balanced command trees at `1`, `8`, `32`, `64`, and `128` distinct
leaves SHALL include per-leaf transforms and normalization to one application-action union. They
SHALL record semantic, representation, instance, layout, MIR, canonical-byte, phase-time,
phase-boundary sampled-heap, LLVM-bitcode-size, and Wasm-size metrics in two fresh processes. Counts
and artifacts MUST be deterministic, and every unexplained superlinear semantic expansion MUST fail
the characterization.

#### Scenario: Establish a baseline before thresholds
- **WHEN** the generated suite completes for both shapes and every size
- **THEN** it publishes measured trends and recommendations without inventing a pre-baseline language limit

### Requirement: Integration findings preserve language boundaries

A failed vertical slice or pressure gate SHALL return the relevant proposal to design review. It MUST
NOT authorize compiler recognition of `Command`, `Cli`, `Schema`, `Decoder`, or `Encoder`, implicit
allocation or erasure, a runtime interface dictionary, or weaker ownership.

#### Scenario: Detect a compiler-known actor dependency
- **WHEN** source spelling changes while its ordinary contracts remain equivalent
- **THEN** compiler behavior remains unchanged or the gate fails as a release-blocking violation
