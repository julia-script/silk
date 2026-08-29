# Language style guide

This guide records source conventions for readable and discoverable Silk APIs. Its rules do not
change whether a program is valid. The compiler treats a type according to its declaration and use,
never according to its name.

Public source documentation follows the separate
[doc comment style guide](documentation-style-guide.md). That guide defines comment coverage,
structure, examples, links, and ASD-STE100 writing rules. This page defines naming, API shape, and
source-usage conventions.

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

Use `Failure` for an unsuccessful outcome variant that carries an error, not for the error payload
type itself. For example, `NotFoundError` names a value while
`Result<A, NotFoundError>.Failure` selects an outcome containing that value.

**Boundary:** The suffix communicates API intent only. `NotFoundError` remains an ordinary value
type and may be stored, passed, returned, or inspected outside an Effect. Conversely, every valid
ordinary type—including `string` and `i32`—may be used as a failure type without an `Error` suffix.

A nominal type that primarily represents ordinary domain data does not gain the suffix merely
because one API may fail with it. Name the type for its dominant meaning.

**Tooling:** The compiler does not enforce this convention. Language tooling may offer a
non-blocking style warning or rename action when a public nominal type is introduced primarily as a
failure type. Such tooling must not imply that the name changes the type's semantics.

**Current standard library:** Consistent. Public error payloads include `FileError`, `LogError`,
`ProcessError`, `ParseError`, `HostInputError`, `StreamReadError`, `StreamWriteError`,
`OutOfMemoryError`, `StalledError`, and `TaskIdExhaustedError`. The `Result<A, F>.Failure` variant
keeps its existing name because it is ordinary result data rather than an error declaration, and
the fiber `Cancelled` outcome keeps its name for the same reason: it is the third arm of `Outcome`
alongside `Success` and `Failure`, not an error payload type.

**Evidence:** [ordinary failure values](typed-failures.md#fail-001--any-concrete-detached-value-may-be-a-typed-failure).

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
Supplying a non-empty trailing suffix of an ordinary multi-parameter function creates a callable
waiting for its remaining leading arguments, so the same convention supports operations on any
value:

```silk
import silk.effect { Effect }

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

**Evidence:** [callable pipeline specification](../../../../openspec/specs/bootstrap-callable-values/spec.md).

## STYLE-003 — Examples prefer actor imports and qualified operations

**Status:** Confirmed

When a module contains a struct, service, or interface matching its filename, documentation,
tutorials, and public API examples should normally import that actor declaration directly and
qualify its operations through the imported name:

```silk
import model.User { User }

let user = User.make(42)
let reassigned = User.withId(move user, 43)
```

This keeps the operation's owner visible where it is used. A reader can identify `make` and
`withId` as part of the `User` actor without searching the import list or relying on a globally
distinct function name. It also keeps related APIs visually grouped and matches the qualified,
data-first convention in STYLE-002.

The matching struct, service, or interface makes the module's public operations available through
the imported actor qualifier, so this form keeps both the actor type and related operations under
one binding. Prefer it in ordinary examples:

```silk
import silk.effect { Effect }

let provided = computation |> Effect.provide(&clock)
return run provided
```

Import a module namespace when no matching struct, service, or interface exists, or when the module
rather than a matching actor declaration is the subject being taught. A matching scalar enum is
not a nominal module scope: its qualifier exposes only its members and generated `value` operation.
Import other selected members when an API is conventionally read unqualified or repeated
qualification would obscure the example's actual point:

```silk
import model.User { User }

fn id(user: &User) -> i32 {
  return user.id
}
```

When two matching actors have the same default name, alias one selected actor explicitly and keep
its operations qualified:

```silk
import model.User { User }
import audit.User { User as UserAudit }

let user = User.make(42)
UserAudit.record(&user)
```

**Boundary:** This is a documentation and API-style preference, not a compiler restriction.
Namespace imports, member aliases, and hybrid imports have their ordinary language meaning. A
reference page specifically documenting those forms should show them directly even though general
examples prefer the matching actor import. For a matching service or interface, declared contract
operations take precedence over same-named top-level module members; a namespace alias performs
ordinary module-member lookup instead. A matching struct has no separate contract-operation lookup.

**Tooling:** Formatters do not rewrite between namespace and selective imports. Documentation lint
may prefer qualification in general examples but must permit selective imports where the example is
teaching that syntax or naming an imported type directly.

**Evidence:** [nominal module scopes](modules-names-and-visibility.md#name-005--a-file-named-struct-or-contract-also-scopes-that-modules-public-members),
[namespace imports](modules-names-and-visibility.md#import-001--a-namespace-import-binds-the-targets-final-path-segment),
[qualified data-first APIs](#style-002--public-apis-prefer-qualified-data-first-functions).
