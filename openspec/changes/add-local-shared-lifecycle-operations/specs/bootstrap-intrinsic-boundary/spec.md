## ADDED Requirements

### Requirement: Two sealed primitives govern local shared lifecycle

The sealed `Intrinsic` namespace SHALL expose
`sharedClone<T>(self: &SharedCore<T>) -> SharedCore<T>` and
`sharedWithMut<T, A>(self: &SharedCore<T>, use: once fn(&mut T) -> A, onConflict: once fn() -> A) -> A`.
Clone SHALL allocate nothing, invoke no user code, and have no failure or requirement channel. It
SHALL trap before mutation when the target-bounded strong count cannot increment and otherwise
publish exactly one new affine handle without reading, moving, copying, or cleaning `T`.

Access SHALL invoke exactly one callback. It SHALL invoke `use` under one exclusive callback-scoped
borrow when access is available, or `onConflict` without changing the existing active access when it
is not. No intrinsic MAY expose the access bit, count, address, last-drop authority, or a
compiler-known conflict value, and no ordinary declaration may gain these contracts by spelling.

#### Scenario: Clone below the count limit

- **WHEN** `sharedClone` observes a strong count below the target maximum
- **THEN** it increments once and returns one new affine handle without allocation or an operation on `T`

#### Scenario: Trap before overflow mutation

- **WHEN** `sharedClone` observes the target maximum strong count
- **THEN** it traps before storing a count or returning a handle

#### Scenario: Select the access callback

- **WHEN** `sharedWithMut` is invoked once with available access and once reentrantly with active access
- **THEN** the first call invokes only `use`, the nested call invokes only `onConflict`, and the nested observation does not release the outer access

#### Scenario: Audit the lifecycle inventory

- **WHEN** the intrinsic catalog is inspected after lifecycle support is added
- **THEN** clone and callback access are the only new lifecycle calls and no separate reader, weak, atomic, lock, or actor-specific operation exists
