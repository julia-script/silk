## ADDED Requirements

### Requirement: MIR models allocation in the structured control DAG

MIR SHALL represent established scope regions and service slots, checked layout results, fallible
allocation, unsafe lexical slot places and value operations, restricted drop hooks, explicit drop,
cleanup registration/disarm, and structured cleanup outcomes as
ordered nodes in the existing target-aware control DAG. It MUST NOT encode scope repetition as graph
back-edges or contain backend allocator objects, LLVM blocks, WebAssembly branch depths, or public
raw-pointer semantics.

#### Scenario: Lower typed allocation failure

- **WHEN** HIR contains a fallible allocation whose `OutOfMemory` may propagate
- **THEN** MIR contains explicit success and failure outcomes, with cleanup of prior live resources before the failure leaves each exited scope

#### Scenario: Verify exactly-once cleanup

- **WHEN** malformed MIR drops an allocation and also retains its cleanup on a later scope exit
- **THEN** MIR verification rejects the duplicate consumption before evaluation or backend emission

### Requirement: MIR verifies slot structure without pretending to prove unsafe invariants

MIR verification SHALL reject a slot whose element type or `SlotLayout<T>` provenance differs from
its allocation operation, a missing checked bound, an escaping slot place, an allocation move or
drop under a live slot loan, and any slot operation after its allocation has been consumed. It SHALL
NOT claim to verify runtime-indexed initializedness or aliasing promised only by unsafe source code.

#### Scenario: Reject use after allocation drop

- **WHEN** malformed MIR accesses a slot after the allocation owner was explicitly dropped
- **THEN** verification rejects the access with the allocation's prior consumption retained as provenance
