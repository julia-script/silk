## ADDED Requirements

### Requirement: Execution packaging admits only three target-neutral operations

The sealed intrinsic catalog SHALL admit `executionLayout`, unsafe `executionFromAllocation`, and
safe unit-returning `drive` with the exact generic and static-property contracts selected by
SLP-0001. The first two SHALL expose only Layout, Allocation, exact executable values, and opaque
Execution; drive SHALL expose only Execution, one affine branch state, and two NonParking outcome
callbacks whose callable types are `once fn`. The compiler MUST NOT recognize Allocator,
OutOfMemoryError, Execution safe wrappers,
Scheduler, Fiber, Deferred, timer, ready queue, or Coroutine declarations by spelling. The catalog
MUST NOT add a compiler-owned step-result sum, explicit destroy, per-drive endpoint replacement,
general callable erasure, or implicit program-entry owner.

#### Scenario: Audit exact packaging signatures

- **WHEN** the intrinsic inventory is compared with semantic, HIR, MIR, evaluator, and backend dispatch
- **THEN** all phases agree on exactly the layout, initializer, and drive powers and their safety and static-property metadata

#### Scenario: Admit affine outcome callbacks

- **WHEN** each drive outcome callback owns an affine capture
- **THEN** the intrinsic inventory accepts both callbacks as NonParking `once fn` values and publishes no reusable-call contract

#### Scenario: Build a safe wrapper in ordinary Silk

- **WHEN** ordinary source queries the Layout, allocates through its chosen Allocator, and calls the unsafe initializer
- **THEN** the wrapper exposes its own failure and requirement rows without compiler knowledge of its declaration name

#### Scenario: Rename every source policy actor

- **WHEN** the safe wrapper and its owner actors are renamed while intrinsic calls and semantics remain unchanged
- **THEN** compiler behavior and artifacts remain identical apart from ordinary source identities

#### Scenario: Reject broader lifecycle privilege

- **WHEN** an implementation proposes a step sum, explicit destroy, Scheduler token, or implicit owner to realize the same slice
- **THEN** the intrinsic audit rejects the additional source-callable power as outside the accepted boundary
