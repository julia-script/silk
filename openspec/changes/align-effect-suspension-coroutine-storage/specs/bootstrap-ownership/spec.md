## MODIFIED Requirements

### Requirement: Suspension transfers one ownership obligation per live value

After concrete specialization and suspendability-aware MIR normalization, ownership SHALL derive
the exact MIR-local live set needed after a deferred child transfers and later completes. The set
SHALL include compiler-generated temporaries as well as locals corresponding to source bindings.
Copy values MAY be copied; affine values SHALL occupy one field in exactly one state of the
invocation's reusable coroutine frame; and shared or exclusive borrows and provider references
SHALL retain their exact root, access, and loan dependencies until resumption or exit. A referent
that remains borrowed across suspension SHALL retain a stable logical location for the borrow's
lifetime regardless of private frame placement or relocation. A value MUST NOT remain independently
owned by both the running state and suspended state.

#### Scenario: Hold one owner per recursive level

- **WHEN** every level of a suspended recursive Effect creates one affine owner used after its child completes
- **THEN** ownership places each owner in exactly one active invocation frame state and rejects any duplicate use from another state

#### Scenario: Retain an exclusive provider dependency

- **WHEN** source code intentionally holds an ordinary exclusive provider reference across its deferred child
- **THEN** ownership keeps that provider immovable and exclusively borrowed until the parent resumes and ends the loan

#### Scenario: Preserve a borrow across private frame growth

- **WHEN** a valid source borrow remains live while the private execution stack grows, segments, or relocates implementation storage
- **THEN** the borrow continues to identify the same referent with unchanged access and lifetime

### Requirement: Continuation cleanup preserves structured-exit semantics

On successful resumption, ordinary return, fallthrough, explicit structured exit, or typed failure,
each coroutine-frame state SHALL move or clean every live source value exactly once in the existing
lexical order, then release its completed private frame through the execution owner without
replacing the original typed outcome. A state transition SHALL finish moving every live obligation
before the driver starts the deferred child, and no obligation may appear in two simultaneously
owned states. A source trap or target defect that cannot return to the runner SHALL retain the
existing no-unwind guarantee: it MUST NOT report that source `Drop` ran or duplicate an obligation.

#### Scenario: Complete one frame-state transition

- **WHEN** a parent invocation suspends while affine values remain live after its child
- **THEN** every retained value belongs to the completed parent frame state before the child begins and no prior state retains a duplicate owner

#### Scenario: Clean deep success in order

- **WHEN** suspended recursion succeeds while every level retains one owner
- **THEN** owners release exactly once in the same inner-to-outer order as the equivalent unsuspended execution

#### Scenario: Clean deep typed failure in order

- **WHEN** an inner suspended level fails with a typed payload while outer levels retain owners
- **THEN** every exited level releases its owner exactly once before the unchanged failure reaches its handler

#### Scenario: Preserve trap semantics

- **WHEN** a resumed suspended computation reaches a source trap or exhausts private execution-stack storage
- **THEN** the runner exposes no typed failure or successful Drop trace and makes no claim that normal source cleanup ran
