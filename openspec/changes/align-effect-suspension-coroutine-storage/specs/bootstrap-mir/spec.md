## MODIFIED Requirements

### Requirement: MIR represents suspension and continuation state target-neutrally

MIR SHALL classify each specialized runner and give every explicit suspension origin and potential
suspendable-run relay a stable target-neutral identity and control-flow form. MIR normalization
SHALL preserve those forms for suspendable or unknown runners before coroutine-state liveness is
computed. Each concrete suspendable invocation SHALL have one coroutine frame descriptor with a
statically known maximum logical layout over all of its resume states. Every state SHALL name the
exact specialized MIR locals live after transfer, including source values and compiler-generated
temporaries, together with the deferred Effect runner, arguments, typed outcome, resume point,
capture access, provider references, and cleanup obligations needed after resumption. Descriptors
SHALL use canonical logical types and compiler-planned layouts while omitting native addresses,
WebAssembly table indexes, target blocks, branch depths, source allocator implementations,
scheduler objects, and public pending values. Each function's structured control regions SHALL
remain acyclic even when the module call graph contains suspended self- or mutual-recursion cycles.

`SuspendEffect` SHALL be the only form permitted to originate a fresh deferred-child transfer.
`RunSuspendableEffect` SHALL have distinct synchronous-Complete and relay-Transfer success/failure
control. Its Complete paths SHALL enter no resume state. Its Transfer path SHALL preserve the
incoming child, origin, and typed-outcome identity and SHALL transition the current invocation into
the exact resume state needed after the child. Repeated transfers by one invocation SHALL reuse its
frame rather than creating separately owned continuation records. Reachable provisional control
MUST be finalized before evaluation or backend emission.

#### Scenario: Retain state after a suspended child

- **WHEN** a non-tail Effect keeps an affine owner and a scalar local across a suspended recursive run
- **THEN** MIR names both live values, their unique frame fields and state ownership, the child outcome, the resume point, and exact cleanup obligations without choosing a target ABI

#### Scenario: Retain a compiler-generated temporary

- **WHEN** a source expression computes `left + run child` and `child` transfers before completing
- **THEN** the finalized coroutine state retains the specialized MIR local containing `left` even when no source binding directly names that temporary

#### Scenario: Reuse one invocation frame

- **WHEN** one concrete Effect invocation can suspend at multiple source points or revisit one point
- **THEN** MIR describes one maximum frame layout with distinct states rather than a newly allocated continuation owner for each transfer

#### Scenario: Normalize before planning coroutine state

- **WHEN** concrete normalization folds or retains an Effect construction and its run
- **THEN** the finalized frame states name exactly the surviving post-normalization MIR locals and contain no stale pre-normalization local

#### Scenario: Encode mutual suspended recursion deterministically

- **WHEN** equivalent mutually recursive Effects are lowered in repeated fresh processes
- **THEN** their suspension operations, frame descriptors, state identities, logical layouts, and cleanup plans encode byte-identically

### Requirement: MIR verifies suspension completeness and ownership

MIR verification SHALL reject a suspension whose deferred runner or typed outcome disagrees with
the call contract; whose resume state is missing or ambiguous; whose post-normalization live local
is omitted, duplicated, or assigned incompatible access; whose maximum frame layout is incomplete;
or whose success and typed-failure plans do not preserve ownership, cleanup, loan endings, and
propagation. Verification SHALL also reject an orphan frame descriptor or suspendable-run form in a
program whose reachable MIR contains no suspension origin. Final MIR verification SHALL reject a
stale pre-normalization local, an unclassified live temporary, incomplete state initialization,
reachable provisional control, a Complete path that enters resume control, an ordinary run that
originates transfer, a relay that changes the incoming child, origin, or typed-outcome identity, or
any suspension path that introduces source allocator access or typed storage failure.

#### Scenario: Reject a missing live owner

- **WHEN** hand-built MIR suspends while an affine local remains needed after resumption but omits that local from its frame state
- **THEN** verification reports the missing ownership before evaluation or backend emission

#### Scenario: Reject suspension machinery without suspension

- **WHEN** a MIR module contains a coroutine frame descriptor or suspendable-run form but no reachable suspension operation
- **THEN** verification rejects the unused machinery instead of allowing a hidden runtime cost

#### Scenario: Reject an ordinary run that originates transfer

- **WHEN** hand-built MIR gives `RunSuspendableEffect` a fresh deferred child or transfer identity instead of relaying one produced by `SuspendEffect`
- **THEN** verification rejects the invalid origin before evaluation or backend emission

#### Scenario: Reject storage channels in suspension MIR

- **WHEN** hand-built suspension MIR adds an allocator requirement or an `OutOfMemory` outcome solely for coroutine-frame storage
- **THEN** verification rejects the contract before evaluation or backend emission
