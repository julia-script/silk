# bootstrap-independent-execution-packaging Specification

## Purpose

Defines exact caller-funded construction, erased-body ownership, driving, logical stack roots, and
cleanup for independently resumable Effect executions.

## Requirements

### Requirement: Execution construction consumes one exact combined package

For concrete `A`, Detached body `F`, Detached endpoint state `O`, and reusable Detached NonParking
callback `R: fn(&O) -> ()`, `executionLayout<A,F,O,R>()` SHALL return the exact target Layout for
one indivisible Execution package. A matching `executionFromAllocation` SHALL consume exactly one
active Allocation plus `F`, `O`, and `R`, SHALL run no body code, and SHALL return one Initial
`Execution<A>`. The package SHALL own the owner record, erased body, exact invoke/drop metadata,
fixed endpoint, stable wake-control storage only when external parking is reachable, and any initial
continuation segment required by the static summary. A statically non-parking wrapper that supplies
a zero-sized no-op `O` and function item `R` SHALL add no readiness storage. Intrinsic operations
MUST NOT allocate and MUST NOT mention Allocator or allocation failure.

#### Scenario: Construct one initial execution

- **WHEN** ordinary source supplies one matching active Allocation and valid exact body and endpoint values
- **THEN** initialization consumes every input, runs no body code, and returns one Initial Execution owning the complete package

#### Scenario: Keep allocation failure before initialization

- **WHEN** the ordinary source allocator refuses the exact Layout
- **THEN** the initializer is not called, no Execution or partial package exists, and ordinary Effect cleanup retains and cleans `F`, `O`, and `R` exactly once

#### Scenario: Omit readiness storage for a non-parking body

- **WHEN** `F` cannot reach external parking and the wrapper supplies a zero-sized no-op endpoint
- **THEN** the exact combined Layout contains no active wake cell or retained readiness state while still owning the erased body and lifecycle metadata

#### Scenario: Reject mismatched package provenance

- **WHEN** unsafe source passes an Allocation whose concrete type, target, size, alignment, or layout provenance does not match `executionLayout<A,F,O,R>()`
- **THEN** validated compilation rejects the initializer before runtime publication

### Requirement: Drive transfers one affine branch to exactly one outcome

`Intrinsic.drive` SHALL consume one Initial or Eligible `Execution<A>`, one affine branch state `D`,
one NonParking completion callback `C: once fn(D,A) -> ()`, and one NonParking suspension callback
`S: once fn(D,Execution<A>) -> ()`, and SHALL return unit. Completion SHALL invoke
only `C`; external parking SHALL invoke only `S`; the unused callback SHALL be cleaned exactly once.
Nested `Effect.suspend` SHALL remain internal to the same drive activation and MUST NOT invoke `S`.
The first drive SHALL root one execution-local logical stack; later drives SHALL restore that root
without treating owner frames as logical ancestors.

#### Scenario: Complete one drive

- **WHEN** an Initial or Eligible Execution completes with `A`
- **THEN** exactly the completion callback receives `D` and `A`, the suspension callback is cleaned once, and drive returns unit without an Execution

#### Scenario: Relinquish one drive

- **WHEN** an Initial or Eligible Execution reaches external parking
- **THEN** exactly the suspension callback receives `D` and the Dormant Execution, the completion callback is cleaned once, and drive returns unit

#### Scenario: Consume affine callback captures once

- **WHEN** completion and suspension callbacks each own affine captures and drive selects one outcome
- **THEN** the selected `once fn` is invoked exactly once and the unselected callback plus its captures are cleaned exactly once without a reusable-call requirement

#### Scenario: Keep nested transfer inside one activation

- **WHEN** a driven Effect reaches nested `Effect.suspend` and its known child completes
- **THEN** the direct child resumes its parent inside the same drive and neither owner callback observes a suspension outcome until final completion or external parking

#### Scenario: Preserve execution-local call depth

- **WHEN** an owner alternates drives of two executions in non-LIFO order
- **THEN** each execution restores its own logical root and call-depth history without inheriting the owner's scheduling frames

### Requirement: Package cleanup is exact and post-construction growth is fatal

Dropping an Initial Execution SHALL clean its unrun body and endpoint exactly once without invoking
any callback. Completion SHALL transfer `A`, clean remaining live package values, and release the
Allocation. Dormant or Eligible drop SHALL clean every live body, frame, endpoint, and registration
value exactly once, subject to the Wake-retention rules added by the parking slice. Cleanup SHALL
end internal loans before owned referents. Dynamic continuation-stack growth after construction
SHALL use the compiler/runtime execution-stack policy and exhaustion SHALL be fatal under Silk's
no-unwind rule; it MUST NOT enter the Effect failure channel.

#### Scenario: Drop before first drive

- **WHEN** an owner drops an Initial Execution
- **THEN** body captures, endpoint state, endpoint callback, metadata, and Allocation are cleaned exactly once and no body or drive callback runs

#### Scenario: Complete after nested growth

- **WHEN** an execution grows its private continuation stack and later completes
- **THEN** every live frame is cleaned in logical structured-exit order before package release

#### Scenario: Exhaust post-construction stack storage

- **WHEN** a running execution exceeds the selected target/runtime continuation capacity
- **THEN** execution terminates through the defined fatal no-unwind path and no recoverable Effect failure is fabricated

#### Scenario: Preserve reified typed failure

- **WHEN** ordinary source constructs an Execution around `Effect.result(body)` and `body` fails with `E`
- **THEN** drive completes normally with ordinary `Result<A,E>` data and the intrinsic error channel remains empty
