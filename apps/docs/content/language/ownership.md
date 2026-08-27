# Ownership, borrowing, and cleanup

Silk makes the lifetime of a value visible at the point where ownership changes. Values either copy
or move, borrows retain their source, and every structured exit has a deterministic cleanup plan.
There are no named lifetimes, garbage collector, ambient heap, or implicit move.

## Copyable and move-only values

Every type falls into one of two ownership categories:

| Category | Common examples | Consuming use |
| --- | --- | --- |
| Copyable | scalar values, `bool`, `char`, borrowed `string`, shared views, and fixed arrays whose elements copy | copies the value; the binding remains usable |
| Move-only | nominal structs, owned `String`, `Bytes`, `Vector<T>`, `Box<T>`, exclusive views, Fibers, and arrays whose elements move | requires `move`; the binding becomes unavailable |

A function parameter taken by value is a consuming position. Passing a copyable value copies it.
Passing a move-only value requires `move`:

```silk
pub struct Ticket {
  pub id: i32
}

fn consume(ticket: Ticket) -> i32 {
  return ticket.id
}

fn double(value: i32) -> i32 {
  return value * 2
}

pub fn main() -> i32 {
  let ticket = Ticket { id: 3 }
  let identifier = consume(move ticket)

  let count = 2
  let first = double(count)
  let second = double(count)
  return identifier + first + second - 8
}
```

Writing `move` always consumes the binding, even when its type is copyable. This makes `move` a
statement of intent rather than harmless decoration. A later use reports `OWN0001` and points back
to the consuming site. Omitting `move` for a move-only value reports `OWN0003`.

Silk rejects partial moves such as `move value.field`: the remaining object would no longer be a
valid whole value. Use `Intrinsic.replace(place, replacement)` when an algorithm must take an owned
field while leaving the place initialized.

## Shared and exclusive access

`&value` grants shared read access. `&mut value` grants exclusive read/write access and requires a
mutable source place.

Within one call:

- any number of shared borrows may overlap;
- an exclusive borrow cannot overlap another shared or exclusive borrow of the same root;
- an exclusive parameter may be reborrowed as shared or exclusive;
- a shared parameter cannot be strengthened to exclusive access.

Borrowing a nominal field such as `&outer.inner` keeps the complete field path. Arrays form slices
from the complete array root; there is no implicit array-to-slice conversion and no borrow of an
individual array element.

## Returned views without lifetime syntax

A bare borrow cannot be bound directly: `let view = &values` is invalid. An ordinary function may
return a view when its signature has exactly one borrowed parameter and the result is proven to
derive from it. The caller may bind that returned view, and the compiler keeps it tied to the source
owner through its last use:

```silk
import silk.usize as usize

fn identity(values: &[i32]) -> &[i32] {
  return values
}

pub fn main() -> i32 {
  let mut values = [1, 2]
  let view = identity(&values)
  let first = view[usize.ZERO]
  values[usize.ZERO] = 3
  return first - 1
}
```

The mutation is legal because the shared view's last use occurs first. While a shared returned view
is live, its root cannot be mutated, moved, dropped, or borrowed exclusively. An exclusive returned
view suspends every independent access to its root.

The single borrowed parameter makes provenance unambiguous without named lifetimes. A function
cannot return a view chosen from two borrowed inputs. A returned view also cannot be stored in a
struct, array, union, ordinary generic wrapper, global, constant, or Effect success/failure value.

## Mutation keeps places initialized

Assigning to a place first evaluates the replacement, then releases the previous value, then commits
the new value. If replacement evaluation fails, the old value remains. This transactional rule
prevents a failed assignment from leaving a hole in an owned structure.

Use `let mut` for a writable local. An exclusive borrow temporarily suspends direct access to its
root and restores it when the call or returned view ends. The compiler analyzes moves through
conditionals conservatively: a value moved in one branch is unavailable afterward even when a
runtime path might avoid that branch.

## Deterministic cleanup

Every structured exit—fallthrough, `return`, or typed `fail`—releases live owned bindings in reverse
acquisition order. A moved binding is not released again. `drop value` consumes and releases a value
early.

A `Drop` implementation cannot fail or require a service. Cleanup is therefore predictable and
does not silently add a failure or capability to an API.

Fatal traps are different from typed failures. Bounds violations, division by zero, trapping
overflow, violated unsafe contracts, and exhausted execution storage terminate outside the Effect
outcome model. Silk does not promise that lexical cleanup, `Drop`, or `Effect.ensuring` runs after a
trap. Represent a condition as data or a typed failure before it reaches a trap when recovery and
cleanup are required.

Recursive cleanup is ordinary recursion. Releasing an unbounded chain of `Box` values can exhaust
the machine stack even if the traversal itself was iterative. See
[Recursion and stack safety](./recursion.md#6-cleanup-has-the-same-limit-and-fewer-ways-out).

## Allocation is an explicit service

There is no ambient allocator. Standard-library operations that create or grow owned storage expose
both `! OutOfMemoryError` and `? &mut Allocator` in their Effect type. The application chooses a
provider; tests can supply a different one.

This separation is useful when reading an API:

- the ownership category tells you whether a value copies or moves;
- the parameter access tells you whether an operation reads, mutates, or consumes it;
- the failure row tells you whether allocation can be refused;
- the requirement row tells you which allocator access the caller must provide.

Raw storage operations additionally require an explicit `unsafe { ... }` boundary. `Vector`,
`Bytes`, `String`, `Box`, hash maps, and hash sets are ordinary Silk source built on those primitives,
not compiler-known collection types.

## Practical checklist

When an ownership diagnostic appears, ask these questions in order:

1. Is the type copyable or move-only?
2. Does this call read through `&`, mutate through `&mut`, or consume by value?
3. Was the binding already consumed on this or another branch?
4. Is a returned view still live at the conflicting access?
5. Does the algorithm need `Intrinsic.replace` to take from a place without leaving it empty?
6. Can cleanup recurse deeply enough that the data structure needs an explicit iterative teardown?

## See also

- [Getting started](./tutorial.md#structs-and-ownership)
- [Language reference: ownership and borrowing](../reference/ownership-and-borrowing.md)
- [Standard library: allocator](./stdlib/allocator.md)
- [Standard library: Box](./stdlib/box.md)
- [Standard library: Vector](./stdlib/vector.md)
- [Diagnostic index](./diagnostics.md)
