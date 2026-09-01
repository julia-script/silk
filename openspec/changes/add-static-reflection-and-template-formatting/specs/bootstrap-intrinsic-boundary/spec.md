## ADDED Requirements

### Requirement: Static reflection uses the minimum sealed primitive seam

The sealed `Intrinsic` namespace SHALL expose only the irreducible operations required to obtain a
concrete static type's aggregate metadata, construct and inspect immutable static sequences, and
residualize an authorized static field descriptor as an ordinary shared field projection. Metadata
and sequence operations SHALL be unavailable at runtime and SHALL have no evaluator, WebAssembly,
or LLVM runtime target. The projection bridge MUST consume its descriptor during specialization and
MUST NOT survive as a runtime intrinsic call.

The projection bridge SHALL use an explicit mixed intrinsic contract: its shared owner-reference
parameter remains a runtime lane, its `Field<Owner, Value>` parameter is a required static lane, and
specialization emits the ordinary runtime `&Value` projection after consuming the descriptor. The
intrinsic catalog, calling-shape verification, and residualizer MUST reject any surviving descriptor
lane or projection-intrinsic call. Mixed parameter phases remain sealed intrinsic metadata and MUST
NOT become a privilege inferred for ordinary functions.

Template grammar, placeholder parsing, aggregate-kind policy, visibility policy, field matching,
`Display` selection, Writer composition, and reusable reflection wrappers SHALL remain ordinary
source. No source module, actor, operation, or descriptor spelling outside `Intrinsic` SHALL receive
compiler privilege.

#### Scenario: Copy the public reflection wrapper

- **WHEN** user source defines an equivalent safe wrapper over the admitted reflection intrinsics
- **THEN** it receives the same static descriptors and residual field projections without compiler registration

#### Scenario: Reject reflection at runtime

- **WHEN** a metadata or sequence intrinsic would remain in a runtime calling shape or residual body
- **THEN** specialization reports a static-phase violation and every backend inventory remains reflection-free

#### Scenario: Consume one mixed projection descriptor

- **WHEN** specialization receives `&Owner` in the runtime lane and an authorized `Field<Owner, Value>` in the static lane
- **THEN** it publishes one ordinary `&Value` field projection and no descriptor parameter or intrinsic call

### Requirement: Reflection primitives cannot expose compiler identity or host state

Reflection and static-sequence primitives SHALL return canonical source-semantic data only. They
MUST NOT expose compiler addresses, host objects, allocation capacity, cache identities, backend
layouts, mangled names, private inaccessible names, filesystem state, environment values, time, or
randomness.

#### Scenario: Repeat reflection in fresh processes

- **WHEN** equivalent concrete aggregate types are reflected in two fresh compiler processes
- **THEN** their public descriptor encodings and source provenance are identical and contain no host-specific value
