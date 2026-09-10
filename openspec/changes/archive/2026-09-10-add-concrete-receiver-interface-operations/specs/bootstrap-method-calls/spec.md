## MODIFIED Requirements

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

## ADDED Requirements

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
