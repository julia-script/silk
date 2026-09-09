## Context

See [proposal.md](proposal.md) for motivation and [evidence.md](evidence.md) for the
instrumented census. The existing source-owned hosted runtime and parser remain in scope
as workloads, not as code to simplify to improve the benchmark.

`NativeType.valueLanesFor` currently supplies the general local representation as well as
boundary materialization. `NativeFunction` allocates lane slots for mutable locals;
`NativeStorage` eagerly reloads full vectors at joins and after possible alias writes.
Addressable roots now share their canonical backing bytes, but still maintain an SSA lane
cache. Block-local reference intervals limit which roots are refreshed, not which fields.

`ElementsResult` contains a 42-lane parser State plus a 10-lane element Vector. Across the
CLI it receives 2,354 whole-root refreshes, producing 122,408 loads. Its 116,636 lane-store
emissions through the instrumented storage helpers are another amplification point.
These observations justify targeting representation; they do not prove that all copies
can disappear or predict a wall-time reduction.

The existing `bootstrap-backend` and `bootstrap-target-layout` specifications remain
authoritative. In particular, layouts, calling shapes, union carrier/variant mappings,
and suspension-frame facts belong to compiler planning, not a second backend model.

The first apply inventory exposed a missing prerequisite: Effect outcomes have
`OutcomeShape` calling lanes, but `Layout.plan` deliberately adds only their success/failure
storage entries. `NativeType.addressLayout` therefore returns undefined for EffectOutcome.
Represented composite Effects can also have only total-size Aggregate entries with empty
fields, which are insufficient as typed carrier views. The user approved adding the missing
planner-owned storage facts before migrating local representations. The original pause in
`implementation-notes.md` records that investigation; this revision resolves the planning
decision, not the implementation itself.

## Goals / Non-Goals

**Goals:** Make local aggregate movement and field access proportional to the operation
being performed rather than automatically proportional to the flattened aggregate width.
Preserve source value/ownership semantics and target facts while reducing generated work
in both native and LLVM-to-Wasm emission. Use a model expressible as tagged values and
storage/projection IDs in the eventual self-hosted compiler.

**Non-Goals:** Change the current calling convention, re-layout nominal unions, add source
features, rewrite the parser to pass references, replace Effect boundary APIs, tune JS
allocation/BigInt behavior, introduce a general optimizer, or redesign the LLVM builder.
Scalarization at an existing ABI boundary is intentional, not an obsolete fallback.

## Decisions

### 0. Plan physical value storage before constructing typed places

Add a backend-neutral value-storage actor and a deterministic storage-view collection owned
by target planning. Do not repurpose an erased Effect type's ordinary layout entry to mean
both its result outcome and its captured environment. A view's identity includes its role
and the appropriate canonical identity: the concrete flow contract for an outcome, the
represented type for a composite carrier, and the exact concrete environment for captures.
Target identity belongs to the containing plan. No LLVM type or handle enters these facts.

Each available view records total byte size, alignment, tag placement when present, typed
physical slot placements, and logical member/field mappings. Ordinary aggregates and
environments refer to their existing layout authority rather than creating independent
offset decisions. Each slot's offset and extent must fit the view and satisfy its selected
scalar alignment. Detect overflow, unavailable dependencies, and incomplete mappings in
planning/verification before emitting a place. Zero-byte payloads retain their logical
initialization/drop obligations; an outcome's discriminant can still require physical bytes.

For an outcome, reuse the existing OutcomeShape's tag identities, ordered payload carriers,
success shape, and failure shapes. Plan tag and carrier slots in deterministic calling-lane
order, aligning each slot to its target scalar alignment and rounding the final extent to
the maximum alignment. Retain complete mappings between logical success/failure fields and
carrier slots, including existing coercions. A payload value's ordinary field layout is
not necessarily the same as its carrier slots: only the selected member may be materialized
or cleaned up, and a raw pointer into a carrier is not automatically a pointer to that member.
Do not substitute the largest member's byte size for the actual unified carrier extent.

For a represented composite, distinguish its existing stored alternative/environment view
from its calling carrier view. Recover exact alternative sizes, alignments, capture identities,
and field placements from the already selected concrete representations; expose their tag,
payload placement, and mappings explicitly instead of treating an empty field list as a
usable field layout. Preserve existing stored sizes/offsets and call carrier types/order.
Plan conversions between the views where they differ; do not assume that an overlay of
alternative environments and a flattened ABI carrier have identical offsets. A conversion
may visit the active alternative's required fields without constructing a whole-local cache.

Call signatures and transfer records remain unchanged. Preserve valid frame offsets, but
correct proven undersized payload reservations in frame planning: borrowed slices retain
their two-word descriptors, and concrete callable captures use their environment extent,
not the invocation descriptor's size. BorrowedDependency is a lifetime classification;
only EnvironmentBorrow is intrinsically a one-pointer payload. Planning and verification
share this extent authority. This adjustment follows the apply request to fix discovered
issues with best judgment; it does not change source semantics or introduce a new ABI.
Plan transport
bindings that map value-storage slots to the existing transport offsets and lane types.
Reproduce current packing from each actual transport start offset, since padding can depend
on that offset. Whole-block transfer is allowed only when compatible extent, alignment,
initialization, and mapping are proven; otherwise use the planned slot/field conversion.
Native code consumes these bindings instead of independently choosing an outcome size or
silently replacing a frame layout with the new local-storage shape. Corrected frame extents
are published by the planner and checked by MIR verification. This explicitly permits
materialization at memory-representation boundaries as well as call boundaries.

Verification covers both 32-bit and 64-bit targets, mixed-width/address/floating carriers,
zero-payload outcomes, differing alternative layouts, and malformed/overflowing views.
Deterministic plan encoding and in-process golden assertions cover the added facts; existing
canary tests continue to own fresh-process determinism. Keep a structural regression for
the minimal `effect fn value() -> i32 { return 42 }` outcome that exposed the missing layout.

Alternatives rejected: reuse calling shapes as if lane order implied memory offsets;
derive storage ad hoc in NativeType; equate outcome storage with capture storage; or keep
Effect outcomes permanently on a separate lane-cache implementation. All would bypass the
planner ownership or coherent representation required by this change.

### 1. Separate lowered values, storage, and ABI materialization

Introduce concept-oriented internal actors for lowered values and typed places, with
data-first operations. A lowered value is one of:

- No physical payload (unit, bottom where valid, or zero-sized representation); logical
  ownership and cleanup obligations are still retained by the MIR contract.
- Direct scalar or bounded primitive descriptor (integer, float, scalar enum, pointer,
  reference, string/slice view). Descriptor components do not recursively expand their
  pointee or element type. Existing scalar mutable-storage behavior remains applicable.
- Aggregate place: storage identity/base, concrete MIR type, selected layout/view,
  alignment, and a projection path. Ordinary structs, fixed arrays, payload unions,
  concrete inline Effect/callable capture environments, and typed outcomes use this form
  when they have physical aggregate payload. Scalar-represented enums remain direct.

Typed places consume the storage views from decision 0; they are not constructible from
an unverified calling shape alone. Classify represented values by their concrete planned
storage, not only their erased
public type. Environment borrows denote borrowed places, not newly owned aggregates.
No size threshold selected from V8 performance determines this classification.

Do not retain both a complete aggregate lane vector and its canonical bytes. The local
map refers to a direct value or a place. Calling-lane materialization is an explicit
call/transport boundary operation, not a generic `readLocal` used by every consumer.

Alternative rejected: a lazy full-lane cache with dirty flags. That preserves the same
width-dependent representation and creates another alias-invalidation system. Typed places
make reads explicit and remove the need for aggregate-wide cache refresh.

### 2. Construct and project into canonical storage

Give each aggregate definition a stable destination unless a valid existing destination
can be used directly. Construction initializes fields there. Scalar field reads load only
the projected field; aggregate field reads produce a typed subplace until a value copy is
actually required. Arrays use the planned element stride; dynamic indexing retains bounds
and initialization checks. Avoid eager allocation of one LLVM slot per leaf lane.

Address-taking, ordinary access, and foreign/pointer writes refer to the same bytes.
After a possible alias write, a later field read observes memory; there is no whole-root
aggregate refresh. Do not reuse a loaded scalar across a possible mutation without proof.
Construct projected addresses on demand. Cache addresses only while base identity is stable;
frame remapping must never reuse pointers derived from the previous stack base.

At control-flow joins, keep stable root storage or have predecessors write a join result
destination. Selecting a pointer with a phi is allowed only when layout, lifetime, and
ownership are already compatible. Do not allow a pointer to escape branch-local storage
or replace a required copy with a pointer selection.

Alternative rejected: finer field-liveness tracking on the current full-lane cache as the
primary fix. It can help later but would preserve eager aggregate construction and copying.

### 3. Copy and move remain semantic operations

Using an address to represent a value does not make source values references. A Copy value
must have independent observable storage when either instance can subsequently be mutated.
Moving an owner transfers its obligation exactly once and leaves the source logically
uninitialized, including partial-move flags and dependent cleanup.

Prefer destination construction. Reuse a moved source place only when the compiler proves
compatible storage lifetime, no observable stale alias, and correct initialization/drop
state; otherwise transfer into distinct destination storage. This local proof must not
require introducing a global liveness analysis in this change.

For a required transfer, use a typed aggregate copy or block copy consistent with existing
layout and initialization facts. `memcpy` requires non-overlap; use overlap-safe movement or
a temporary when overlap is possible. Skip physical copies for zero-byte payloads without
skipping logical ownership effects. Do not invent LLVM `noalias`, `nonnull`, or alignment
guarantees, interpret padding as initialized scalar values, or initialize/drop inactive
union fields. LLVM-lowered memory helpers must participate in existing capability accounting.

Alternative rejected: globally aliasing every move/copy to the same pointer. It would
silently change value semantics and can invalidate borrowing, cleanup, and suspension.

### 4. Preserve layouts and boundary contracts

Calls and returns consume the existing compiler-planned calling shapes. Materialize an
aggregate's lanes immediately at such a boundary; place incoming aggregate payloads directly
into canonical destination storage. Keep one explicit boundary path shared by direct and
indirect calls, foreign exports/callbacks where admitted, and generated helper/thunk bodies.
Foreign C classification remains a separate authority; an internal place is not permission
to pass a pointer where the C ABI expects a value.

Union carrier storage and active-variant field storage are distinct views. Preserve the
current mapping and materialize an active variant when a canonical field address is needed;
transfer Drop-hook mutations back through the mapping before structural cleanup. A typed
place must carry enough view information to prevent treating carrier offsets as ordinary
variant offsets. This change does not replace that mapping with a raw union overlay.

Alternative deferred: an indirect aggregate calling convention. It could remove additional
boundary expansion, but changes caller/callee and planned calling-shape contracts. The
measured 2,354 local refreshes justify an independently valuable intra-function change first.
Keeping the current ABI is an explicit scope boundary, not a compatibility adapter.

### 5. Effects, diagnostics, and suspension use the same representation

Concrete capture environments and outcomes cannot retain a separate aggregate lane-cache
implementation. Materialize captures into their planned destinations, preserving borrowed
versus moved fields and exact invocation/drop contracts. Preserve failure tags, active
payload initialization, diagnostic ownership transfer, and the shared completion epilogue.

Suspension spills and resumes use planned persistent frame places. Relocate/copy live stack
payload into frame storage before parking, update base identities and projections, and
never retain stack addresses past their lifetime. Cancellation/drop releases only initialized
owners once. Transfer-only runners without retained frames remain distinct from parked
continuations; this representation does not change that runtime protocol.

Alternative rejected: implementing only ordinary structs while leaving Effect/outcome
lowering permanently lane-based. The census shows those values wrap the same large parser
state; such a split would leave both the amplification and duplicate representations.

### 6. Keep evidence and correctness separate

Structural tests prove field-only access, canonical storage identity, independent copies,
ownership transfer, mapped union accesses, and persistent suspension storage. They assert
relationships and consumed operands, not timing or fixed instruction counts. Reuse existing
test files and one analysis snapshot per source. Add target-neutral runtime distinctions
to the shared native corpus and intended WebAssembly behavior to LLVM-to-Wasm coverage.

The opt-in census records local widths, join/alias refreshes, store emissions, and final
LLVM opcode totals. The external diagnostic hook is evidence, not a proposed production API;
move the needed instrumentation into an opt-in harness during implementation without adding
normal-build traversal. Preserve per-function/operation attribution where available and
label unassigned work rather than attributing every instruction to the nearest MIR operation.

Before implementation, run a fresh uninstrumented baseline on the same sources/toolchain.
Afterward run at least three cold builds each of parser-only and full CLI with the existing
cache policy and runtime oracles. Retain all samples, phase timings, CPU/RSS, hashes, and host
load; separate instrumented census from timing. Do not overlap our builds/tests with timed
samples. Compare scalar/descriptor control programs as well as the real workloads.

Acceptance requires removal of eager whole-root aggregate refresh behavior, correct full
lowering coverage, and a measured reduction in generated aggregate traffic. A cold-build
or runtime regression must be investigated before accepting the design as an optimization;
if timing is inconclusive, report it and collect another controlled batch. No target number
of seconds or Zig-parity claim is assumed.

## Risks / Trade-offs

- More addressable storage or mandatory boundary copies can hurt runtime optimization →
  retain destination construction, compare debug and optimized artifacts, and measure
  representative runtime cases outside timing-sensitive correctness tests.
- The current ABI still flattens wide arguments/results → count this residual explicitly;
  consider a separate calling-convention proposal only after measuring the local change.
- Subplace aliasing, partially initialized owners, and suspension can expose stale storage →
  keep typed layout/view identity and MIR initialization/ownership facts on every transfer;
  cover each lifetime boundary before migrating its consumers.
- A block copy can introduce a target library dependency → use the existing LLVM helper
  capability gate and inspect native/Wasm objects, including libc-none cases where supported.
- Union carrier representation differs from canonical payload fields → retain two explicit
  views and existing bidirectional mappings; no generic raw-field projection shortcut.
- Existing unrelated `.claude/worktrees` files block root formatting/lint checks → report the
  exact gate failures; do not change unrelated worktrees or claim a clean full check.

## Migration Plan

Implement in a dedicated change while preserving current WIP. First close the type/view
inventory and implement/verify the outcome and composite storage plans and their transport
bindings. Only then construct typed places and migrate value/storage operations, call boundaries,
and Effect/cleanup/suspension consumers. Intermediate development steps are not a finished
feature: delete obsolete aggregate lane-cache paths and update every consumer before handoff.

Run the repository checks in their prescribed order and the full native shard set for the
completed change. Re-measure without diagnostic instrumentation. No public data migration or
runtime feature flag is needed. If the approach fails correctness or performance acceptance,
revise it or revert only this change's own edits; never restore the entire dirty worktree.
