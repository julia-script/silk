# Ownership and borrowing

Silk separates long-lived responsibility from temporary access. An affine value has one owner; a
move transfers ownership; a borrow grants temporary access; cleanup follows the owner.

This page begins with the common core. Returned views, callable and Effect captures, partial
replacement, generic ownership, and suspension require later passes because existing artifacts
contain historical layering and unresolved presentation boundaries.

## OWN-001 — Values are Copy or affine

**Status:** Candidate

A `Copy` value may be duplicated by an ordinary read. An affine value has one owner and is not
duplicated implicitly. User-defined structs are affine in the current bootstrap language even when
all their fields are `Copy`; the complete set of types that qualify as `Copy` remains under review.

```silk
fn double(value: i32) -> i32 { return value * 2 }

pub fn main() -> i32 {
  let value = 21
  let doubled = double(value)
  return doubled + value - 21
}
```

**Boundary:** Passing a user-defined struct by value requires an explicit move. Cleanup-bearing
values are also affine and cannot be duplicated by an ordinary read.

**Diagnostics:** An implicit transfer of an affine value reports `OWN0003` at the use and identifies
the binding that requires `move`.

**Evidence:** [ownership decision](../../wayfinder/bootstrap-language/issues/01-ownership-lifetimes-and-scoped-allocation.md),
[ownership specification](../../openspec/specs/bootstrap-ownership/spec.md).

## OWN-002 — `move` consumes the source binding

**Status:** Candidate

`move value` transfers the value to the receiving owner and makes the source binding unavailable.
An explicit move consumes even a `Copy` binding.

```silk
struct Token { value: i32 }

fn consume(token: Token) -> i32 { return token.value }

pub fn main() -> i32 {
  let token = Token { value: 42 }
  return consume(move token)
}
```

**Boundary:** Reading `token` after the call is a use-after-move error.

```silk,ignore
let answer = consume(move token)
return answer + token.value
```

**Diagnostics:** A use after a consuming move reports `OWN0001` at the later use and relates the
earlier move location.

**Evidence:** [ownership move tests](../../packages/compiler/test/Ownership.test.ts).

## BORROW-001 — A shared borrow grants temporary read access

**Status:** Candidate

`&value` grants shared access without transferring ownership or cleanup responsibility. Multiple
compatible shared borrows may coexist. A shared borrow prevents conflicting mutation, movement, and
cleanup while it remains live.

```silk
struct Counter { value: i32 }

fn read(counter: &Counter) -> i32 { return counter.value }

pub fn main() -> i32 {
  let counter = Counter { value: 42 }
  return read(&counter) + read(&counter) - 42
}
```

**Boundary:** A shared borrow does not permit mutation through the borrowed value.

**Diagnostics:** Mutation through immutable access reports `SEM0035` at the assignment target.
Accessing or moving the owner while an overlapping shared loan remains active instead reports the
corresponding ownership conflict, currently `OWN0011` or `OWN0012`.

**Evidence:** [ownership specification](../../openspec/specs/bootstrap-ownership/spec.md),
[runtime-slice ownership tests](../../packages/compiler/test/RuntimeSliceOwnership.test.ts).

## BORROW-002 — An exclusive borrow grants temporary sole access

**Status:** Candidate

`&mut value` grants exclusive access to a mutable owner. No other access to the owner may overlap the
exclusive borrow.

```silk
struct Counter { value: i32 }

fn set(counter: &mut Counter, value: i32) -> () {
  counter.value = value
  return ()
}

pub fn main() -> i32 {
  let mut counter = Counter { value: 0 }
  set(&mut counter, 42)
  return counter.value
}
```

**Boundary:** Borrowing one owner both shared and exclusively for the same call is invalid.

**Diagnostics:** Conflicting slice loans report `OWN0010` at the later borrow. Accessing the owner
while an exclusive loan remains active reports `OWN0011`. The diagnostic relates the conflicting
loan origin rather than treating the two borrows as unrelated argument errors.

**Evidence:** [ownership specification](../../openspec/specs/bootstrap-ownership/spec.md),
[runtime-slice ownership tests](../../packages/compiler/test/RuntimeSliceOwnership.test.ts).

## CLEANUP-001 — Cleanup follows ownership

**Status:** Candidate

A live affine owner is cleaned exactly once on a structured exit. Moving the value transfers that
cleanup obligation; explicit `drop` consumes it early. Typed failure performs cleanup for exited
regions, while a process-aborting trap makes no cleanup guarantee.

**Boundary:** Cleanup that may fail is an explicit consuming operation such as `close`; it is not a
fallible `Drop` hook.

**Diagnostics:** A source `Drop` implementation outside the sealed infallible cleanup contract
reports `SEM0084`. Ordinary cleanup of a valid owner is implicit behavior and produces no
diagnostic.

**Evidence:** [ownership and cleanup decision](../../wayfinder/bootstrap-language/issues/01-ownership-lifetimes-and-scoped-allocation.md),
[ownership specification](../../openspec/specs/bootstrap-ownership/spec.md).

## Disputed boundary — returned borrows

The original ownership decision says borrows cannot be returned. The current specification permits
a conservative returned view derived from exactly one borrowed parameter and keeps the backing owner
restricted until the view's last use. This boundary remains **Disputed** until the current behavior
is presented and confirmed as part of stabilization.

**Diagnostics:** A returned-borrow signature that cannot express one supported origin reports
`SEM0091`. A body that returns a borrow from an invalid origin reports `SEM0092`. Whether those
accepted origins remain part of the stabilized language is still disputed.

**Evidence:** [original ownership decision](../../wayfinder/bootstrap-language/issues/01-ownership-lifetimes-and-scoped-allocation.md),
[current returned-view requirements](../../openspec/specs/bootstrap-ownership/spec.md).

## Pending rules

The next pass will cover partial moves, complete destructuring, replacement, returned views, Drop
hooks, closure captures, Effect captures, generic ownership, and allocation owners.
