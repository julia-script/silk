## MODIFIED Requirements

### Requirement: Associated functions are not value members; receiver methods bind their receiver

`receiver.member(args)` SHALL be rejected when `member` names an associated function without a
receiver, with a diagnostic stating that the member has no receiver and naming the `Owner.member`
form; the bare spelling `value.member` SHALL report the same diagnostic. A receiver method named
through a value outside callee position (`let f = value.member`) SHALL produce a callable section
that captures the receiver as parameter zero under the declared receiver mode (`&Self` a shared
loan of the receiver place, `&mut Self` an exclusive loan, `Self` a move of the place or the rvalue)
and awaits the member's remaining parameters in order. A method whose only parameter is the
receiver SHALL bind to a zero-parameter callable. A receiver that is not a place SHALL be rejected
with the ordinary borrow-operand diagnostic when parameter zero is a reference. The retired
`SEM0199` diagnostic SHALL NOT be emitted. Receiver operations reached through a generic
parameter's bounds are not bound by this capability and keep the projection diagnostic. A member
type parameter the receiver does not fix stays open exactly as a trailing section leaves it;
closing it at application is outside this capability.

#### Scenario: Reject calling an associated function on a value

- **WHEN** `impl Counter { pub fn zero() -> Self }` exists and source evaluates `counter.zero()` or names `counter.zero`
- **THEN** the compiler reports that `zero` has no receiver and suggests `Counter.zero()`

#### Scenario: Bind a consuming method

- **WHEN** source evaluates `let unwrap = option.unwrapOr` then `unwrap(0)` with `fn unwrapOr(self: Self, fallback: T) -> T`
- **THEN** `unwrap` is a take-once callable that moved `option`, its application resolves to `Option.unwrapOr` with `T` fixed by the receiver so `fallback` expects `i32`, and a later use of `option` is the ordinary use-after-move

#### Scenario: Bind a borrowing method

- **WHEN** source evaluates `let reader = counter.read` with `fn read(self: &Self) -> i32`
- **THEN** `reader` has type `fn() -> i32`, holds a shared loan of `counter` until its last use, and `counter` remains readable meanwhile

#### Scenario: Bind an exclusive method

- **WHEN** source evaluates `let bumper = counter.bump` with `fn bump(self: &mut Self)` while a shared loan of `counter` is live, or later reads `counter` while `bumper` is live
- **THEN** the compiler reports the ordinary conflicting-loan diagnostic

#### Scenario: Reject a borrowed binding of a temporary

- **WHEN** source evaluates `let reader = Counter { value: 1 }.read` with `fn read(self: &Self)`
- **THEN** the compiler reports the ordinary borrow-operand diagnostic at the receiver

#### Scenario: A generic receiver does not bind

- **WHEN** `fn show<T: Printable>(value: &T)` names `value.print` without calling it
- **THEN** the compiler reports the projection diagnostic it reports today
