# bootstrap-method-calls Specification

## Purpose

Defines receiver-syntax invocation `receiver.member(args)` as a third spelling of one statically
selected member, including receiver adaptation, precedence, and ambiguity rules.

## Requirements

### Requirement: Receiver syntax resolves to one static member

A call `receiver.member(args)` whose receiver has a static nominal type SHALL resolve `member` in
this order: an accessible field of that name (the existing callable-field application), then an
accessible inherent receiver method of the canonical owner. The compiler SHALL analyze the
receiver against parameter zero and the written arguments against the remaining parameters, with
the member's declared parameter types as the expected types, and SHALL record the same static call
target that `Owner.member(receiver, args)` records. The decision SHALL key off the analyzed
receiver being a value, never off the spelling of the receiver token. There SHALL be no runtime
method table, no dynamic lookup, and no duck typing.

#### Scenario: Call a receiver method

- **WHEN** source evaluates `option.map(addOne)` with `option: Option<i32>` and `impl<T> Option<T> { pub fn map<U>(self: Self, ...) }`
- **THEN** the call resolves to inherent member `Option.map` with `T = i32` fixed by the receiver and `U` inferred from `addOne`

#### Scenario: Three spellings share one target

- **WHEN** a program evaluates `value.M(x)`, `Type.M(value, x)`, and `value |> Type.M(x)` for one receiver method `M`
- **THEN** all three record the same canonical call target, produce the same result, and specialize once

#### Scenario: A field wins over a member

- **WHEN** a struct stores a callable in field `handler` and source evaluates `widget.handler(3)`
- **THEN** the call applies the stored callable exactly as before this capability

#### Scenario: Reject an unknown member

- **WHEN** source evaluates `counter.missing()` and `Counter` has no field or inherent method `missing`
- **THEN** the compiler reports an unknown member naming the receiver type

#### Scenario: Later arguments receive expected types

- **WHEN** `fn merge(self: &Self, other: &Self) -> i32` is called as `left.merge(&right)`
- **THEN** the borrow argument is accepted in position one exactly as in `Counter.merge(&left, &right)`

#### Scenario: A shadowing local does not become a type qualifier

- **WHEN** a local binding named `Option` of a struct type calls `Option.describe()` where `describe` is a receiver method of that struct
- **THEN** the call resolves as a method call on the local value

### Requirement: Receiver ownership follows the declared receiver parameter

The receiver expression SHALL be adapted to parameter zero's declared mode: a `&Self` receiver
takes a shared loan of a receiver place, a `&mut Self` receiver takes an exclusive loan and is
rejected on a binding that is not `mut`, and a `Self` receiver consumes a receiver place or an
rvalue under the ordinary affine rules. A receiver already of the declared reference type SHALL
pass through unchanged. The synthesized loan or move SHALL participate in ownership analysis and
lowering exactly as a written `&place`, `&mut place`, or `move place` argument does. The compiler
MUST NOT search dereferences or apply any other coercion; a receiver whose type does not match the
owner after that one adaptation SHALL be rejected with the ordinary argument diagnostic. The
explicit forms `Owner.member(&value, ...)` and `Owner.member(move value, ...)` SHALL remain valid,
and a parenthesized receiver such as `(move value).member(...)` SHALL analyze as the value it
groups.

#### Scenario: Borrow a receiver implicitly

- **WHEN** `fn value(self: &Self) -> i32` is called as `counter.value()` with `counter` a local place
- **THEN** the call takes a shared loan of `counter` for the call and `counter` remains usable afterwards

#### Scenario: Borrow a receiver exclusively

- **WHEN** `fn bump(self: &mut Self) -> ()` is called as `counter.bump()` while a shared loan of `counter` is live
- **THEN** the compiler reports the ordinary conflicting-loan diagnostic

#### Scenario: Reject an exclusive receiver on an immutable binding

- **WHEN** `fn bump(self: &mut Self) -> ()` is called as `counter.bump()` and `counter` is a `let` binding
- **THEN** the compiler reports the ordinary exclusive-borrow-requires-mutable diagnostic naming `counter` at the receiver's span

#### Scenario: A one-argument call to a two-parameter method is a call

- **WHEN** `fn map<U>(self: Self, transform: once fn(T) -> U)` is called as `option.map(addOne)`
- **THEN** the result is a call producing `Option<U>`, not a callable section awaiting a receiver

#### Scenario: Receiver and arguments are analyzed once

- **WHEN** `counter.merge(&cells)` is called with `cells` a fixed array and `counter` misspelled
- **THEN** the unknown-name diagnostic for the receiver is reported once and the array argument is typed as a reference, not a slice

#### Scenario: Pass a reference receiver through

- **WHEN** `fn value(self: &Self) -> i32` is called as `borrowed.value()` with `borrowed: &Counter`
- **THEN** the call uses the existing reference without a second loan

#### Scenario: Consume a receiver

- **WHEN** `fn map<U>(self: Self, ...)` is called as `option.map(addOne)` and `option` is used afterwards
- **THEN** the compiler reports the ordinary use-after-move diagnostic at the later use

#### Scenario: Chain on an rvalue

- **WHEN** source evaluates `Option.some(2).map(addOne)`
- **THEN** the temporary is consumed by `map` and no place is moved

#### Scenario: Refuse to dereference a receiver

- **WHEN** a `Box<Counter>` value calls `boxed.value()` where `value` is declared on `Counter`
- **THEN** the compiler reports an unknown member on `Box<Counter>` rather than dereferencing

### Requirement: Associated functions are not value members and members are not values

`receiver.member(args)` SHALL be rejected when `member` names an associated function without a
receiver, with a diagnostic stating that the member has no receiver and naming the `Owner.member`
form. A receiver method named through a value outside callee position (`let f = value.member`)
SHALL be rejected with a diagnostic naming `Owner.member` as the first-class form.

#### Scenario: Reject calling an associated function on a value

- **WHEN** `impl Counter { pub fn zero() -> Self }` exists and source evaluates `counter.zero()`
- **THEN** the compiler reports that `zero` has no receiver and suggests `Counter.zero()`

#### Scenario: Reject a bound method value

- **WHEN** source evaluates `let mapper = option.map`
- **THEN** the compiler reports that `map` must be called and suggests `Option.map`

### Requirement: Generic receivers obtain members only from declared bounds

Inside a generic body, `value.op(args)` on a receiver typed by a type parameter SHALL resolve only
when exactly one declared bound of that parameter declares receiver operation `op`; it SHALL
resolve to the same bound operation that `Bound.op(value, args)` resolves to and specialize through
the witness the instantiation admits. An unbounded parameter SHALL reject the call even when every
concrete instantiation would provide it, two bounds declaring `op` SHALL be reported as ambiguous
naming both interfaces. A bound operation that declares its own type parameters is unavailable
through both the receiver and the explicit spelling, as it is today, and SHALL report an unknown
member. Interface-backed members on a concrete receiver are outside this capability.

#### Scenario: Call a bound's operation through a value

- **WHEN** `fn show<T: Printable>(value: &T) -> string { return value.print() }` is declared
- **THEN** the body checks once against the bound and each specialization selects the admitted witness

#### Scenario: Reject an unbounded receiver

- **WHEN** `fn show<T>(value: &T) -> string { return value.print() }` is declared and only `Document` is ever passed
- **THEN** the compiler rejects the body at declaration time without consulting instantiations

#### Scenario: Report ambiguity across one parameter's bounds

- **WHEN** `fn show<T: Printable + Debug>(value: &T)` calls `value.print()` and both bounds declare `print`
- **THEN** the compiler reports an ambiguity naming `Printable` and `Debug`, and `Printable.print(value)` resolves

#### Scenario: A concrete receiver does not reach interface operations

- **WHEN** `Document` conforms to `Printable` and source evaluates `document.print()` with no inherent `print`
- **THEN** the compiler reports an unknown member on `Document`
