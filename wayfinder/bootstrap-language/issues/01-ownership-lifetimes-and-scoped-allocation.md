# Choose the ownership, borrowing, allocation, and cleanup model

Type: grilling
Status: resolved

## Question

What ownership, borrowing, allocation, and cleanup rules give safe code deterministic reclamation
without manual `free`, tracing garbage collection, named lifetime scopes, or a full Rust-compatible
lifetime solver—and are expressive enough to implement the bootstrap compiler?

## Answer

Silk Effect uses affine single ownership. Non-copy values have one owner; transferring ownership
consumes the entire source value and makes its old binding unusable. Bootstrap safe code rejects
partial moves, but permits consuming an aggregate through complete destructuring and extracting a
field through an operation that leaves a valid replacement.

Bindings are immutable by default. Mutation requires an explicitly mutable owner and an exclusive
borrow. User-defined types are move-only unless declared copyable; the compiler accepts that
declaration only when every field is copyable and the type has no cleanup behavior. Duplicating any
other value is an explicit actor-module operation whose function contract exposes requirements such
as allocation.

Borrows are lexical and non-escaping in the bootstrap language. Any number of shared borrows or one
exclusive mutable borrow may exist, but not both at once. A borrow may be passed across function
boundaries or retained by a first-class callable environment whose lifetime is proven shorter than
the borrow, but it may not be returned, stored in an unrelated owned struct or union, or retained by
a longer-lived callable. A shared capture permits repeated shared invocation; an exclusive capture
permits repeated sequential invocation through exclusive access. Long-lived compiler relationships
use owned collections and stable copyable identifiers rather than stored references.

Dynamic memory is represented by ordinary affine owners. The allocator service receives a validated
target-aware layout and returns a self-contained `Allocation` carrying all authority needed for its
eventual infallible release. The allocation does not borrow or retain the provider that created it,
and cleanup never resolves whichever allocator happens to be provided later. Collection growth
allocates a new block, moves or copies initialized elements, and then drops the old owner.

This self-contained-allocation contract is the bootstrap boundary. `SystemAllocator` is its first
standard-library implementation. An arena is never compiler magic: a future `ArenaAllocator` must be
ordinary Silk standard-library code implementing a general allocator contract. A traditional
resettable arena whose outputs remain tied to its backing storage may be admitted only after Silk has
a general, non-privileged way to express that validity relationship, or behind a non-escaping library
operation. Escaping arena-backed values, provider-dependent results, hidden dependency sets,
`depends on` contracts, and owned-region aggregates are therefore deferred together.

Every affine type has deterministic cleanup. The compiler derives recursive field cleanup, and an
affine nominal type may implement the restricted synchronous `Drop` hook needed to release an
external handle or destroy initialized elements hidden inside raw storage. `Drop` is infallible,
non-allocating, requirement-free, cannot move from `self`, and runs before automatic field cleanup.
Locals clean up in reverse acquisition order and fields in their fixed language-defined order on
normal block exit, explicit return, `break`, `continue`, and typed failure propagation. Explicit
consuming `drop` permits earlier cleanup without exposing manual `free`.

Cleanup whose failure matters is an explicit consuming operation such as `close`, `flush`, or
`shutdown`. Once it succeeds or otherwise consumes the resource, the infallible `Drop` fallback is
disarmed or becomes a no-op. Fallible or asynchronous cleanup policy belongs in ordinary library
APIs; bootstrap has no language-level `defer`, `errdefer`, dynamic finalizer registry, or asynchronous
exit hook.

Traps are process-aborting defects and carry no cleanup guarantee. Cancellation, interruption,
detached tasks, and concurrency are outside the bootstrap milestone. A future structured-concurrency
design must wait for child termination and cleanup before its owner exits, but that constraint does
not add runtime scope machinery to the MVP.

The bootstrap compiler does not require built-in shared ownership, reference counting, stored
borrows, named `Scope` capabilities, per-field lifetime parameters, or a full Rust-compatible
lifetime solver. Recursive and cyclic compiler data use owned collections plus stable IDs. Explicit
shared ownership, region capsules, dependent provider results, and structured task groups remain
coherent later extensions if concrete workloads demonstrate their need.

Callable environments follow the same affine cleanup rules as every other owner. Copy captures are
snapshotted at construction, borrowed captures keep their loans live until the environment is
dropped, and moved captures transfer their cleanup into the environment. A callable that transfers
an owned capture onward is consuming and may be invoked at most once; dropping it uncalled cleans
the capture exactly once. This is ordinary ownership state, not closure garbage collection or a
universal heap box.

All code shown while resolving this ticket was illustrative pseudocode. This answer fixes semantics,
not concrete syntax; syntax remains the responsibility of
[Prototype the bootstrap language syntax](08-prototype-bootstrap-syntax.md).
