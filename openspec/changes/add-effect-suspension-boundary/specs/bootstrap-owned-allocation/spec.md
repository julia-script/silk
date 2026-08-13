## ADDED Requirements

### Requirement: Continuation storage uses explicit typed allocation

Each `Effect.suspend` boundary that requires retained continuation state SHALL request a validated
compiler-planned layout through the active exclusive `Allocator` service requirement. A successful
request SHALL produce one self-contained affine reclaim obligation owned privately by the
continuation; exhaustion SHALL produce the allocation-free typed failure `OutOfMemory`. The
compiler and backends MUST NOT use a hidden allocator, select an allocator implementation, convert
exhaustion to a trap, or add allocation failure and allocator requirements to Effects that cannot
reach suspension. A provider implementation selected to allocate continuation storage MUST have a
closed call graph that cannot itself reach suspension; analysis SHALL reject a selected provider
that would recursively require continuation storage in order to allocate continuation storage.

#### Scenario: Observe continuation allocation through a source allocator

- **WHEN** a suspended recursive Effect runs with a source-defined counting allocator
- **THEN** every successful continuation request and release is observed through that allocator's ordinary service contract

#### Scenario: End allocator access before running the child

- **WHEN** continuation allocation succeeds and the deferred Effect also requires exclusive allocator access
- **THEN** the allocation call's provider loan ends before the deferred child starts, while the continuation retains only self-contained reclaim authority

#### Scenario: Reject one continuation request atomically

- **WHEN** the selected allocator refuses a valid continuation layout
- **THEN** the boundary fails with `OutOfMemory` without starting the deferred child, publishing a partial continuation, or creating a reclaim obligation

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
