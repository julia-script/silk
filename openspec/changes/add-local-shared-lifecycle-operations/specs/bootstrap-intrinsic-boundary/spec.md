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
is not. Access SHALL allocate nothing. After the selected callback returns normally, the unselected
take-once callback environment SHALL receive ordinary callable cleanup exactly once; on successful
access, the borrow SHALL end and availability SHALL be restored before that cleanup and return. No
intrinsic MAY expose the access bit, count, address, last-drop authority, or a
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

#### Scenario: Clean the unselected callback

- **WHEN** each take-once callback owns one affine capture and access selects either success or conflict
- **THEN** the selected callback is consumed by invocation, the unselected callback's capture is cleaned exactly once after normal callback return, and access allocates no storage

#### Scenario: Share one target-selected count boundary

- **WHEN** evaluation and one backend execute clone for the same selected target at and below its planned count maximum
- **THEN** both consume the same maximum, agree on success below it, and trap before mutation at it

#### Scenario: Audit the lifecycle inventory

- **WHEN** the intrinsic catalog is inspected after lifecycle support is added
- **THEN** clone and callback access are the only new lifecycle calls; no reader, weak, atomic, lock, count, address, access-state, cleanup-authority, conflict-value, or actor-specific operation exists, and same-spelled ordinary declarations receive no privilege
