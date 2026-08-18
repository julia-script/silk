# Language style guide

This guide records source conventions for readable and discoverable Silk APIs. Its rules do not
change whether a program is valid. The compiler treats a type according to its declaration and use,
never according to its name.

## STYLE-001 — Nominal error types use the `Error` suffix

**Status:** Confirmed

A nominal type designed primarily to be carried through an Effect failure channel should end in
`Error`. The convention is especially important in public APIs, where the name must communicate its
role without requiring callers to inspect the implementation.

```silk
pub struct NotFoundError { id: i32 }
pub struct PermissionDeniedError { id: i32 }

pub effect fn read(id: i32) -> string ! NotFoundError | PermissionDeniedError {
  fail NotFoundError { id: id }
}
```

Use the condition or domain name first and `Error` last: `NotFoundError`, not `ErrorNotFound`.
Module qualification already groups related names, while the suffix lets the type read naturally:

```text
filesystem.NotFoundError
filesystem.PermissionDeniedError
```

Use `Failure` for an unsuccessful outcome that carries an error, not for the error payload type
itself. For example, `NotFoundError` names a value while `Failure<NotFoundError>` names an outcome
containing that value.

**Boundary:** The suffix communicates API intent only. `NotFoundError` remains an ordinary value
type and may be stored, passed, returned, or inspected outside an Effect. Conversely, every valid
ordinary type—including `string` and `i32`—may be used as a failure type without an `Error` suffix.

A nominal type that primarily represents ordinary domain data does not gain the suffix merely
because one API may fail with it. Name the type for its dominant meaning.

**Tooling:** The compiler does not enforce this convention. Language tooling may offer a
non-blocking style warning or rename action when a public nominal type is introduced primarily as a
failure type. Such tooling must not imply that the name changes the type's semantics.

**Current standard library:** Inconsistent. Names such as `FileError` and `LogError` already follow
the convention. Error payloads named `ProcessFailure`, `ParseFailure`, `HostInputFailure`,
`StreamReadFailure`, `StreamWriteFailure`, and `OutOfMemory` should move to the `Error` suffix during
implementation reconciliation rather than being preserved as competing conventions. The
`Failure<F>` result outcome keeps its existing name under this rule.

**Evidence:** [ordinary failure values](typed-failures.md#fail-001--any-concrete-owned-detached-value-may-be-a-typed-failure).

## STYLE-002 — Public APIs prefer qualified data-first functions

**Status:** Confirmed

A public operation on a value should usually be an ordinary module-level function whose first
parameter is that value. Callers qualify the operation through its defining module:

```silk
pub struct Counter { value: i32 }

pub fn increment(counter: Counter, amount: i32) -> Counter {
  return Counter { value: counter.value + amount }
}

let direct = Counter.increment(Counter { value: 40 }, 1)
let piped = Counter { value: 40 } |> Counter.increment(1)
```

The direct and piped forms have the same meaning. This is not special treatment for Effect APIs.
Supplying the trailing arguments of an ordinary multi-parameter function creates a unary callable
waiting for its leading argument, so the same convention supports operations on any value:

```silk
let specialized = Effect.provide(computation, &clock)
let piped = computation |> Effect.provide(&clock)
```

Nominal data types should remain primarily data. A library may expose a small set of operations
needed to construct or preserve the type's invariants, but the bulk of its API should remain
ordinary composable functions. An `impl` declaration should exist because a type must conform to an
interface or service, not merely to collect all functions related to that type. When a conformance
operation is also a useful concrete API, prefer mapping it to an actor function; an inline
conformance body remains valid when no separately callable operation is useful.

This organization permits another module to add operations for an existing public type without
mutating the type or its method set. The added operation is qualified by the module that defines
it—or imported directly—not injected into the original module's namespace. Modules and nominal
types are not reopenable namespaces. If two imported modules or selected functions would introduce
the same local name, the import is a collision and the caller must alias at least one of them. Silk
does not select by import order or form an overload set.

**Boundary:** This is an API convention, not a validity rule. A function may place another argument
first when its domain meaning calls for that order, and a valid inline conformance is not rejected
for lacking a mapped actor function. Silk does not, however, reinterpret a qualified data-first
function as an instance method:

```silk,ignore
counter.increment(1)
```

Use `Counter.increment(counter, 1)` or `counter |> Counter.increment(1)`.

Only the module defining a nominal type may declare its conformances. Extension functions do not
retroactively make an external type conform to an interface or service; a third-party
type/interface combination requires an owned adapter type. This coherence boundary is independent
from the ability to add ordinary functions anywhere.

**Tooling:** The compiler does not require public APIs to be data-first. Language tooling and API
documentation may prefer this shape and show its direct and piped forms together. Method-call syntax
and reopening another module or type remain invalid language boundaries, not style warnings.

**Evidence:** [behavior-extension decision](../../wayfinder/bootstrap-language/issues/02-bootstrap-type-system-and-values.md),
[module and conformance coherence](../../wayfinder/bootstrap-language/issues/04-modules-visibility-and-name-resolution.md),
[automatic leading-argument sections](../../wayfinder/bootstrap-language/issues/08-prototype-bootstrap-syntax.md),
[callable pipeline specification](../../openspec/specs/bootstrap-callable-values/spec.md).
