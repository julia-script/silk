## MODIFIED Requirements

### Requirement: Inherent members join the canonical declaration index

The declaration index SHALL record every function declared inside an inherent impl as an
associated member of one canonical nominal owner. The member fact SHALL carry the owner's
canonical identity, the member name, visibility, the impl's binders the member mentions (all of
them when it names `Self`) followed by the member's own binders as its complete type-parameter
list, `Self` bound to the owner applied to the impl binders, the full parameter list with the
receiver as parameter zero when present, and a receiver classification: a member whose first
parameter is spelled `self` and typed as `Self`, a reference to `Self`, or the owner applied to the
impl binders is a receiver method; any other member, including one whose `self` parameter has
another type, is an associated function. A member MAY redeclare an owner binder to refine its
bounds for that member alone; the refined binder keeps the head's identity and is not a duplicate.
A member's binders SHALL be minted under the member's own identity so two members of one impl never
share a binder. A member's canonical identity SHALL be its owner's canonical identity plus its
name, independent of which impl block declares it. The index SHALL answer owner-plus-name lookups
without consulting the owner's module basename. A struct, union, enum, service, or interface SHALL
be an eligible owner through one path; a service owner SHALL differ from an interface owner only by
dependency eligibility.

#### Scenario: Index a receiver method

- **WHEN** the index processes `impl<T> Option<T> { pub fn map<U>(self: Self, transform: once fn(T) -> U) -> Option<U> }`
- **THEN** the member is recorded under the canonical `Option` owner as a receiver method with type parameters `[T, U]` and parameter zero typed `Option<T>`

#### Scenario: Index a member that mentions no owner binder

- **WHEN** the index processes `impl<A, E> Fiber<A, E> { pub fn cancel(canceller: CompletionCanceller) -> () }`
- **THEN** the member's type-parameter list is empty and `Fiber.cancel(move canceller)` is callable without naming an instantiation

#### Scenario: Index a member that refines an owner binder

- **WHEN** the index processes `impl<K: HashKey, V> HashMap<K, V> { pub fn get<K: HashKey + Copy, V: Copy>(self: &Self, key: K) -> Option<V> }`
- **THEN** the member is recorded with type parameters `[K, V]` carrying the refined bounds and no duplicate-binder diagnostic is reported

#### Scenario: Index an associated function

- **WHEN** the index processes `impl Counter { pub fn zero() -> Self }`
- **THEN** the member is recorded as an associated function with no receiver and `Self` resolved to `Counter`

#### Scenario: Index members of a service and an interface identically

- **WHEN** a module declares `impl Logger { pub fn inMemoryProvider() -> InMemoryLogger }` for a service and `impl HashKey { pub fn describe() -> string }` for an interface
- **THEN** both members are recorded through the same associated-member fact shape and lookup path

#### Scenario: Index a member of an enum owner

- **WHEN** the index processes `enum Status { Ready, Failed }` and `impl Status { pub fn describe(self: Self) -> string }`
- **THEN** the member is recorded under `Status` and `Status.describe(value)` and `Status.Ready` both resolve

#### Scenario: A root function never becomes a member

- **WHEN** a module declares a top-level `fn transform(self: Counter) -> Counter`
- **THEN** the index records an ordinary function and `Counter` has no associated member `transform`

### Requirement: Inherent impl heads are whole-family and owner-local

An inherent impl head SHALL name a nominal declaration of the declaring module through ordinary
module scope; a declaration of the module whose spelling matches the head SHALL be the owner even
when a builtin type shares that spelling. The head's type arguments SHALL be exactly the impl's
own binders, each used once in declaration order. The index SHALL reject with a deterministic
diagnostic at the head: an owner declared in another module, an owner that is a type alias, a head
whose arguments are concrete or repeated or fewer than the owner's parameters, and a head whose
binders carry bounds. A rejected impl SHALL publish no members, and its members SHALL still close
`Self` to the named owner so the head's diagnostic is the only one reported.

#### Scenario: Accept whole-family heads

- **WHEN** source declares `impl Widget { }`, `impl<T> Option<T> { }`, and `impl<A, B> Pair<A, B> { }` in the owners' modules
- **THEN** all three heads are accepted

#### Scenario: Accept a head whose owner shares a builtin spelling

- **WHEN** `slot.silk` declares `pub struct Slot {}` and `impl Slot { pub fn write<T>(...) }`
- **THEN** the head names the module's `Slot` declaration rather than the builtin `Slot<T>` place type

#### Scenario: Reject a specialized head

- **WHEN** source declares `impl Option<i32> { fn special() -> i32 }`
- **THEN** the index reports a specialized-inherent-head diagnostic at the head and `Option` gains no member `special`

#### Scenario: Reject a foreign owner

- **WHEN** a project module imports `silk.option { Option }` and declares `impl<T> Option<T> { fn mine(self: Self) -> Self }`
- **THEN** the index reports a foreign-owner diagnostic at the head and the member is not published

#### Scenario: Reject an alias owner

- **WHEN** a module declares `type Maybe<T> = Option<T>` and `impl<T> Maybe<T> { ... }`
- **THEN** the index reports an alias-owner diagnostic at the head and `Option` gains no member

#### Scenario: Reject a bounded inherent head

- **WHEN** source declares `impl<T: Display> Option<T> { pub fn make(value: T) -> Self { ... } }`
- **THEN** the index reports exactly the conditional-inherent-head diagnostic, publishes no member, and reports nothing about `Self`
