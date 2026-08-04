# Choose the ownership, lifetime, and scoped-allocation model

Type: grilling
Status: resolved

## Question

What ownership, borrowing, lifetime, and scope rules give safe code deterministic reclamation and
explicit dynamic allocation without requiring manual `free`, a tracing garbage collector, or a
full Rust-compatible borrow checker—and are expressive enough to implement the bootstrap compiler?

## Answer

Silk Effect uses affine single ownership. Non-copy values have one owner; transferring ownership
consumes the entire source value and makes its old binding unusable. Bootstrap safe code rejects
partial moves, but permits consuming an aggregate through complete destructuring and extracting a
field through an operation that leaves a valid replacement. Whether concrete syntax spells a move
explicitly is deliberately unresolved.

Bindings are immutable by default. Mutation requires an explicitly mutable owner and an exclusive
borrow. User-defined types are move-only unless declared copyable; the compiler accepts that
declaration only when every field is copyable and the type has no cleanup behavior. Duplicating any
other value is an explicit actor-module operation whose function contract exposes requirements such
as allocation.

Borrows are lexical and non-escaping in the bootstrap language. Any number of read-only borrows or
one exclusive mutable borrow may exist, but not both at once. A borrow may be passed to a function
or captured by a callback proven not to escape, but it may not be returned, stored in an owned
struct or union, or captured by a longer-lived callback. Long-lived compiler relationships use
scope-owned collections and stable copyable identifiers rather than stored references.

Named `Scope<S>` capabilities define maximum lifetimes independently of allocator policy. Scopes
form a lexical outlives hierarchy: the innermost active scope is the default destination, targeting
an ancestor scope is explicit, and a value tied to a descendant cannot escape to an ancestor. A
longer-lived value may be used within a shorter scope. Simple values, structs, and unions each have
one compiler-inferred effective scope; a composite is limited to its shortest-lived component and
ordinary aggregates do not own scopes, allocators, or per-field scope parameters.

Functions create ordinary ownership boundaries for their local bindings but do not implicitly
create named scopes. Any `Scope<S>` requirement used by a function propagates through its contract
unless the function explicitly creates and closes a local scope, regardless of whether the function
returns a scoped value. Dynamic allocation likewise exposes an `Allocator` requirement and uses a
named scope as its lifetime destination. The allocator chooses acquisition and reclamation policy;
the scope chooses validity. A heap may reclaim an allocation when its owner ends, while an arena may
retain its physical storage until scope closure.

Ownership may end before its maximum scope. The compiler inserts automatic cleanup when an owner
ends, and an explicit consuming `drop` permits earlier cleanup without exposing manual `free`.
Cleanup is typed-infallible and deterministic: locals, dynamically registered finalizers, nested
scopes, and owned fields release in last-acquired, first-released order on success, typed failure,
and early return. Cleanup whose failure matters is an explicit consuming operation; once consumed,
the value is not cleaned up again.

The bootstrap compiler does not require built-in shared ownership, reference counting, stored
borrows, owned-region aggregates, or a full Rust-compatible lifetime solver. It should express
recursive and cyclic compiler data with scoped collections plus stable IDs. Explicit `Shared<T>`,
weak references, and aggregates that encapsulate a private scope remain coherent later library or
language extensions if concrete usage demonstrates their need.

All code shown while resolving this ticket was illustrative pseudocode. This answer fixes semantics,
not concrete syntax; syntax remains the responsibility of
[Prototype the bootstrap language syntax](08-prototype-bootstrap-syntax.md).
