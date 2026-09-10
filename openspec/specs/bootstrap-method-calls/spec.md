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
member.

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
- **THEN** the concrete-receiver capability governs the call, and this bound-receiver requirement neither resolves nor rejects it

### Requirement: A concrete receiver reaches one uniquely supplied interface operation

A call `receiver.member(args)` whose receiver has a runtime-concrete nominal type and whose `member`
names no field and no inherent association SHALL resolve `member` against the receiver-bearing
operations supplied by the interface applications that type provably conforms to. An application
participates only when the ordinary conformance proof selects it — valid, coherent, and terminating —
and only when the interface declaration is visible to the calling module, so an interface the caller
cannot name never contributes a member. The compiler-sealed `Copy` and `Drop` capabilities and every
service SHALL contribute no candidate.

When exactly one participating application supplies a receiver operation of that name, the call SHALL
record the same static conformance witness, generic specialization, failure channel, and requirement
row that the qualified `Interface<Arguments>.member(receiver, args)` call records, and SHALL
introduce no runtime dispatch, dictionary, dereference, or receiver coercion. An operation declaring
its own type parameters, and an operation with no operand of the provider's type, SHALL supply no
candidate.

#### Scenario: Call a conformance operation through a concrete receiver

- **WHEN** `Document` conforms to `Printable` and source evaluates `document.print()` with no inherent `print`
- **THEN** the call resolves to the `Printable` witness for `Document` and produces the same result as `Printable.print(&document)`

#### Scenario: Adapt the receiver as the operation declares

- **WHEN** `fn advance(value: &mut Self) -> i32` is supplied by the one conformance of `Range` and source evaluates `range.advance()` with `range` a `mut` binding
- **THEN** the call takes an exclusive loan of `range` exactly as `Advancing.advance(&mut range)` does

#### Scenario: Run an effectful conformance operation

- **WHEN** the one conformance of `Range` supplies `effect fn take(value: &mut Self) -> i32`
- **THEN** `range.take()` carries the same failure and requirement rows as the qualified call

#### Scenario: An invisible interface contributes nothing

- **WHEN** a loaded module declares a private interface whose operation name matches and the calling module cannot name that interface
- **THEN** the compiler reports an unknown member rather than resolving or reporting ambiguity

#### Scenario: A generic receiver is unaffected

- **WHEN** `fn show<T>(value: &T) -> i32 { return value.print() }` is declared and `Document` conforms to `Printable`
- **THEN** the unbounded parameter still rejects the call without consulting conformances

### Requirement: Inherent lookup resolves or fails before an interface supplies a member

Interface fallback SHALL be consulted only when the receiver's own type genuinely has no member of
that name. An accessible field, an accessible inherent receiver method, an inaccessible inherent
member, a duplicate inherent declaration, and an inherent associated function without a receiver
SHALL each keep the outcome they have today, whether that outcome is a resolved call or a
diagnostic. A conformance SHALL NOT rescue a name that an inherent declaration has already claimed
and failed.

#### Scenario: An inherent member wins over a conformance

- **WHEN** `Document` declares inherent `print` and also conforms to `Printable`
- **THEN** the call resolves to the inherent member and the conformance supplies nothing

#### Scenario: An inaccessible inherent member still fails

- **WHEN** `Document` declares a private inherent `print`, conforms to `Printable`, and another module evaluates `document.print()`
- **THEN** the compiler reports the ordinary inaccessible-member diagnostic rather than the conformance operation

#### Scenario: A receiver-less inherent member still fails

- **WHEN** `impl Document { pub fn print() -> i32 }` exists alongside a `Printable` conformance and source evaluates `document.print()`
- **THEN** the compiler reports that `print` has no receiver and suggests `Document.print()`

#### Scenario: A callable field still wins

- **WHEN** a struct stores a callable in field `print` and also conforms to `Printable`
- **THEN** the call applies the stored callable

### Requirement: A supplied operation is available only in callee position

Naming a conformance-supplied receiver operation outside callee position, as `value.operation`,
SHALL be rejected with a diagnostic stating that the operation is supplied by an interface and must
be called. A first-class value would have to carry the conformance witness the call selects
statically, which this capability does not provide. The diagnostic SHALL NOT claim the receiver has
no such member, because the call spelling resolves.

#### Scenario: Reject a supplied operation as a value

- **WHEN** `Document` conforms only to `Printable` and source evaluates `let bound = document.print`
- **THEN** the compiler reports that `print` is supplied by an interface and must be called

### Requirement: Two supplying applications are ambiguous before arguments are checked

When more than one participating interface application supplies a receiver operation of the written
name, the compiler SHALL report the call as ambiguous at the member, naming the receiver type and
every supplying application so the author can write one qualified call. The written arguments, the
expected result, source order, and declaration order SHALL NOT select among candidates, and the
ambiguity SHALL be reported before the arguments are checked against any candidate's contract.

#### Scenario: Report ambiguity across two conformances

- **WHEN** `Report` conforms to both `Printed<i32>` and `Shown<i32>`, each declaring `print`, and source evaluates `report.print()`
- **THEN** the compiler reports an ambiguity naming both applications, and `Printed<i32>.print(&report)` resolves

#### Scenario: Arguments do not disambiguate

- **WHEN** two supplying applications declare `print` with different operand types and the written argument fits exactly one
- **THEN** the compiler still reports the ambiguity rather than selecting the operation the argument fits
