## ADDED Requirements

### Requirement: Pointer-bearing instance keys include pointee and mutability

Instance keys SHALL treat `*const T` and `*mut T` as ordinary concrete runtime types whose
canonical form includes the pointee type and mutability, and reachability SHALL NOT follow a
pointee's construction, cleanup, or conformance instances merely because a pointer to it is
reachable.

#### Scenario: Key two pointer instances distinctly

- **WHEN** a generic function is applied to `*const i32` and to `*mut i32`
- **THEN** discovery records two specializations with different canonical keys

#### Scenario: A pointer does not reach the pointee

- **WHEN** a program's only use of `Vector<i32>` is a `*mut Vector<i32>` parameter
- **THEN** discovery records no `Vector<i32>` cleanup or method instance
