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
