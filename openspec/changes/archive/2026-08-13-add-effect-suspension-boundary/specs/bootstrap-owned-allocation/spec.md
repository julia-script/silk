## ADDED Requirements

### Requirement: Continuation storage uses explicit typed allocation

Each `RunSuspendableEffect` that observes transfer and needs distinct post-child resume state SHALL
derive one validated physical target layout from its finalized post-normalization logical payload
and request that complete size and alignment through the active exclusive `Allocator` service
requirement. Resume state includes control-only state even when no value payload is live. A tail
relay that can propagate the transferred child outcome unchanged without any later work MAY add no
frame. The explicit `SuspendEffect` origin SHALL create the transfer but SHALL NOT allocate merely
for originating it. Physical private headers MAY differ by engine. A successful request SHALL
produce one self-contained affine reclaim obligation owned privately by that continuation;
exhaustion SHALL produce the allocation-free typed failure `OutOfMemory`. A suspendable runner path
that completes synchronously or does not reach the origin MUST NOT allocate merely because it could
suspend. The compiler and backends MUST NOT use a hidden allocation or allocator, select an
allocator implementation, convert exhaustion to a trap, or add allocation failure and allocator
requirements to Effects that cannot reach suspension. A provider implementation selected to
allocate continuation storage MUST have a closed call graph that cannot itself reach suspension;
analysis SHALL reject a selected provider that would recursively require continuation storage in
order to allocate continuation storage.

An explicit suspension SHALL originate an unpublished transfer without starting its deferred child.
As that transfer returns through suspendable callers, each caller that has live post-resume state
SHALL allocate and initialize its own continuation exactly once and prepend it to the unpublished
chain. The driver SHALL publish the completed chain and start the deferred child only after every
required caller continuation is initialized. Any refusal SHALL roll back the entire unpublished
prefix and return `OutOfMemory` without starting the child. A call to a suspendable runner that
completes synchronously or takes a branch that never reaches explicit suspension SHALL allocate no
continuation merely because transfer was possible.

#### Scenario: Observe continuation allocation through a source allocator

- **WHEN** a suspended recursive Effect runs with a source-defined counting allocator
- **THEN** every successful continuation request and release is observed through that allocator's ordinary service contract

#### Scenario: Do not allocate for an untaken suspension branch

- **WHEN** a suspendable Effect runs through a branch that does not reach `Effect.suspend`
- **THEN** the selected allocator observes no continuation request for that invocation

#### Scenario: Complete a suspendable runner synchronously

- **WHEN** a runner classified suspendable takes a dynamic branch that returns typed success or failure without reaching `SuspendEffect`
- **THEN** its caller observes the unchanged outcome, ordinary current-activation cleanup, and zero continuation requests, publications, resume entries, or reclaims

#### Scenario: Relay transfer through a source combinator

- **WHEN** a suspendable child transfers through an ordinary combinator that retains mapper state after the child
- **THEN** the combinator allocates its caller continuation while relaying transfer, and a synchronous completion of the same child path allocates no such continuation

#### Scenario: Allocate one frame for every stateful relay

- **WHEN** one explicit suspension transfers through two ordinary callers that each need distinct post-child resume state
- **THEN** the selected allocator observes exactly two continuation requests in inner-to-outer relay order and observes no request for the transfer origin itself

#### Scenario: Elide a tail relay frame

- **WHEN** an ordinary suspendable caller can relay the transferred child and its typed outcome unchanged with no post-child work or retained state
- **THEN** it adds no continuation request or frame to the unpublished chain

#### Scenario: Refuse an outer relayed continuation

- **WHEN** an inner continuation is initialized but allocation of an outer caller continuation is refused before publication
- **THEN** the unpublished inner continuation rolls back exactly once, the deferred child does not start, and the caller receives unchanged `OutOfMemory`

#### Scenario: End allocator access before running the child

- **WHEN** continuation allocation succeeds and the deferred Effect also requires exclusive allocator access
- **THEN** the allocation call's provider loan ends before the deferred child starts, while the continuation retains only self-contained reclaim authority

#### Scenario: Reject one continuation request atomically

- **WHEN** the selected allocator refuses a valid continuation layout at any frame-producing relay ordinal
- **THEN** that request creates no owner, every earlier unpublished continuation rolls back and reclaims exactly once, the refusing activation retains its untransferred values, and `OutOfMemory` returns without starting the deferred child or publishing a partial chain

#### Scenario: Keep synchronous Effects allocation-free

- **WHEN** an Effect call graph cannot reach the suspension intrinsic
- **THEN** no continuation storage request is made and no `OutOfMemory` or `Allocator` row is added because suspension exists elsewhere in the program

#### Scenario: Reject a recursively suspending continuation allocator

- **WHEN** the allocator implementation selected for continuation storage can reach `Effect.suspend`
- **THEN** analysis rejects that provider selection before MIR or backend emission

### Requirement: Continuation storage releases through captured authority

A successfully initialized continuation SHALL retain its own unforgeable reclaim authority and
MUST NOT retain or rediscover the allocator provider after the allocation call ends. Its storage
SHALL release exactly once after its live values have been moved onward or cleaned, including when
the resumed computation returns normally or propagates typed failure. A target trap retains the
existing no-cleanup guarantee.

#### Scenario: Release after provider borrow ends

- **WHEN** a continuation remains active after its allocator call's provider borrow ends
- **THEN** eventual cleanup releases storage exactly once through captured reclaim authority without consulting the provider again
