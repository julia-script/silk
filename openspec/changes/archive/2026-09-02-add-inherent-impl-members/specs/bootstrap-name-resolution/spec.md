## ADDED Requirements

### Requirement: Nominal qualifiers resolve associated members

A qualified path `Owner.member` whose qualifier resolves to a nominal declaration SHALL resolve
`member` through that declaration's associated-member set: its intrinsic items (union variants,
enum members, generated operations, declared contract operations) and its inherent members. The
lookup SHALL follow the canonical declaration through selected-import aliases and transparent type
aliases, SHALL apply ordinary visibility so a private inherent member is inaccessible outside its
declaring module, and SHALL report an unknown member when no associated item exists. A declared
associated member SHALL take precedence over any other projection the qualifier might otherwise
offer for the same spelling. The module basename MUST NOT participate in resolving a declared
associated member.

#### Scenario: Resolve a member through a selected type import

- **WHEN** a module imports `silk.option { Option }` and calls `Option.some(2)`
- **THEN** the call resolves to the inherent member `some` of the canonical `Option` declaration

#### Scenario: Resolve a member when the file name differs

- **WHEN** `widgets.silk` declares `pub struct Gadget` with `impl Gadget { pub fn make() -> Self }` and an importer calls `Gadget.make()`
- **THEN** the call resolves to the inherent member without any basename comparison

#### Scenario: Resolve a member through a type alias

- **WHEN** a module declares `type Maybe<T> = Option<T>` and calls `Maybe.some(2)`
- **THEN** the call resolves to `Option`'s inherent member `some`

#### Scenario: Refuse a private member across modules

- **WHEN** `impl Counter { fn secret() -> i32 }` is private and another module calls `Counter.secret()`
- **THEN** resolution reports the member as inaccessible with the private declaration as the candidate

#### Scenario: Declared member outranks any other projection

- **WHEN** a module whose basename matches `Counter` declares both a root `pub fn make` and `impl Counter { pub fn make() -> Self }`
- **THEN** `Counter.make()` resolves to the inherent member and never to the root function

#### Scenario: Resolve service and interface members through one path

- **WHEN** `Logger.inMemoryProvider()` names an inherent member of a service and `HashKey.describe()` names an inherent member of an interface
- **THEN** both resolve through the associated-member set with identical lookup outcomes and diagnostics shapes

### Requirement: Root declarations never attach to a nominal type

A top-level function SHALL remain a module declaration reachable only unqualified, through a
selected import, or through a namespace import. Neither the name nor the type of its first parameter
SHALL attach it to a nominal type, and a selective import MUST NOT select an inherent member as if
it were a root declaration.

#### Scenario: A root function with a self parameter stays free

- **WHEN** a module declares `fn transform(self: Counter) -> Counter` at the top level
- **THEN** `transform(counter)` resolves and `Counter.transform` reports an unknown member

#### Scenario: Refuse to import a member selectively

- **WHEN** a module writes `import silk.option { map }` after `map` became an inherent member of `Option`
- **THEN** the import reports an unknown member of `silk/option` with the inherent member as a related candidate
