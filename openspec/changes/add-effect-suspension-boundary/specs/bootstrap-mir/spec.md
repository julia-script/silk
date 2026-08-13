## ADDED Requirements

### Requirement: MIR represents suspension and continuation state target-neutrally

MIR SHALL represent an explicit suspension boundary with the deferred Effect runner, arguments,
typed outcome, stable resume point, live logical values, capture access, provider references, and
cleanup obligations needed after resumption. Continuation descriptors SHALL use canonical logical
types and compiler-planned layouts while omitting native addresses, WebAssembly table indexes,
target blocks, branch depths, allocator implementations, scheduler objects, and public pending
values. Each function's structured control regions SHALL remain acyclic even when the module call
graph contains suspended self- or mutual-recursion cycles.

#### Scenario: Retain state after a suspended child

- **WHEN** a non-tail Effect keeps an affine owner and a scalar local across a suspended recursive run
- **THEN** MIR names both live values, their ownership transfer, the child outcome, the resume point, and the exact cleanup obligations without choosing a target ABI

#### Scenario: Encode mutual suspended recursion deterministically

- **WHEN** equivalent mutually recursive Effects are lowered in repeated fresh processes
- **THEN** their suspension operations, continuation descriptors, resume identities, logical layouts, and cleanup plans encode byte-identically

### Requirement: MIR verifies suspension completeness and ownership

MIR verification SHALL reject a suspension whose deferred runner or typed outcome disagrees with
the call contract; whose resume point is missing or ambiguous; whose live value is omitted,
duplicated, or assigned incompatible access; whose continuation layout is incomplete; or whose
success and failure resumes do not preserve cleanup and propagation. Verification SHALL also reject
private continuation forms in a program whose reachable MIR contains no suspension operation.

#### Scenario: Reject a missing live owner

- **WHEN** hand-built MIR suspends while an affine local remains needed after resumption but omits that local from its continuation descriptor
- **THEN** verification reports the missing continuation ownership before evaluation or backend emission

#### Scenario: Reject suspension machinery without suspension

- **WHEN** a MIR module contains a continuation descriptor or private suspension entry but no reachable suspension operation
- **THEN** verification rejects the unused machinery instead of allowing a hidden runtime cost

