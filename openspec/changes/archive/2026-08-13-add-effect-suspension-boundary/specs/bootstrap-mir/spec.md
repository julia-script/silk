## ADDED Requirements

### Requirement: MIR represents suspension and continuation state target-neutrally

MIR SHALL first classify each specialized runner and give every explicit suspension origin and
potential suspendable-run relay a stable target-neutral identity and control-flow form. MIR
normalization SHALL preserve those forms for suspendable or unknown runners before continuation
liveness is computed. Final continuation descriptors SHALL then name the exact specialized MIR
locals live after transfer, including source values and compiler-generated temporaries, together
with the deferred Effect runner, arguments, typed outcome, resume points, capture access, provider
references, and cleanup obligations needed after resumption. Descriptors SHALL use canonical
logical types and compiler-planned layouts while omitting native addresses, WebAssembly table
indexes, target blocks, branch depths, allocator implementations, scheduler objects, and public
pending values. Each function's structured control regions SHALL remain acyclic even when the
module call graph contains suspended self- or mutual-recursion cycles.

`SuspendEffect` SHALL be the only form permitted to originate a fresh transfer identity and
deferred child. `RunSuspendableEffect` SHALL have distinct synchronous-Complete and relay-Transfer
success/failure control. Its Complete paths SHALL allocate no caller continuation and enter no
resume region. Its Transfer path SHALL preserve the incoming child, origin, and typed-outcome
identity, and MAY prepend only the caller continuation described for that run. A tail relay with no
resume state MAY relay without a descriptor. Reachable provisional control MUST be finalized before
evaluation or backend emission.

#### Scenario: Retain state after a suspended child

- **WHEN** a non-tail Effect keeps an affine owner and a scalar local across a suspended recursive run
- **THEN** MIR names both live values, their ownership transfer, the child outcome, the resume point, and the exact cleanup obligations without choosing a target ABI

#### Scenario: Retain a compiler-generated temporary

- **WHEN** a source expression computes `left + run child` and `child` transfers before completing
- **THEN** the finalized continuation descriptor retains the specialized MIR local containing `left` even when no source binding directly names that temporary

#### Scenario: Distinguish transfer origin from relay

- **WHEN** an explicit suspension is reached through an ordinary source combinator whose selected runner can suspend
- **THEN** the explicit suspension form originates transfer while the combinator's suspendable-run form either completes synchronously or relays that transfer and retains caller state only on the transfer path

#### Scenario: Normalize before planning continuation state

- **WHEN** concrete normalization folds or retains an Effect construction and its run
- **THEN** the finalized descriptor names exactly the surviving post-normalization MIR locals and contains no stale pre-normalization local

#### Scenario: Encode mutual suspended recursion deterministically

- **WHEN** equivalent mutually recursive Effects are lowered in repeated fresh processes
- **THEN** their suspension operations, continuation descriptors, resume identities, logical layouts, and cleanup plans encode byte-identically

### Requirement: MIR verifies suspension completeness and ownership

MIR verification SHALL reject a suspension whose deferred runner or typed outcome disagrees with
the call contract; whose resume point is missing or ambiguous; whose post-normalization live local
is omitted, duplicated, or assigned incompatible access; whose continuation layout is incomplete;
or whose allocation-refusal, success, and typed-failure plans do not preserve ownership, cleanup,
loan endings, and propagation. Verification SHALL also reject an orphan continuation descriptor or
suspendable-run form in a program whose reachable MIR contains no suspension origin. Final MIR
verification SHALL reject a stale pre-normalization local, an unclassified live temporary,
incomplete initialization-prefix or path plans, reachable provisional control, a Complete path that
allocates or enters resume control, an ordinary run that originates transfer, or a relay that
changes the incoming child, origin, or typed-outcome identity.

#### Scenario: Reject a missing live owner

- **WHEN** hand-built MIR suspends while an affine local remains needed after resumption but omits that local from its continuation descriptor
- **THEN** verification reports the missing continuation ownership before evaluation or backend emission

#### Scenario: Reject suspension machinery without suspension

- **WHEN** a MIR module contains a continuation descriptor or suspendable-run form but no reachable suspension operation
- **THEN** verification rejects the unused machinery instead of allowing a hidden runtime cost

#### Scenario: Reject an ordinary run that originates transfer

- **WHEN** hand-built MIR gives `RunSuspendableEffect` a fresh deferred child or transfer identity instead of relaying one produced by `SuspendEffect`
- **THEN** verification rejects the invalid origin before evaluation or backend emission

#### Scenario: Reject allocation on synchronous completion

- **WHEN** a `RunSuspendableEffect` Complete path contains caller-frame initialization, publication, or resume-region entry
- **THEN** verification rejects the path because synchronous completion remains in the current activation
