## MODIFIED Requirements

### Requirement: Vector ordering is stable and deterministic

`Vector<T>` SHALL support ordering its elements in place for any element type carrying an `Order`
witness, whether that witness selects a compiler-known comparison or one of the element type's own
functions. The order SHALL be total and stable: two elements that compare equal SHALL keep their
input order relative to one another, and this SHALL hold for an element type whose equal elements
stay distinguishable, so stability is observable rather than merely asserted. The order SHALL be
deterministic — the same input SHALL always produce the same output, and the evaluator, LLVM, and
Wasm SHALL agree on that output — because every comparison and every exchange is decided by run
boundaries alone and never by an address, a capacity, or an engine detail. Ordering SHALL move each
element at most once per exchange, so no element is duplicated, leaked, or dropped twice, and SHALL
NOT require the element type to be `Copy` to move an element; comparing two elements SHALL NOT
consume either, so an element type that owns a resource can be ordered. Ordering allocates a scratch
buffer and therefore SHALL carry the typed `OutOfMemoryError` failure and the allocator requirement.

`Vector<T>` SHALL support searching a sorted vector for an element through the same `Order` witness,
returning an optional index that is present only when a matching element exists. The search SHALL
return the lowest matching index when several elements compare equal, so repeated searches over one
vector answer identically.

#### Scenario: Order an unsorted vector

- **WHEN** a program orders a vector whose elements are not in order
- **THEN** every element afterward compares no greater than the element following it, and the length and every element value are unchanged

#### Scenario: Equal elements keep their input order

- **WHEN** a vector holding several elements that compare equal is ordered
- **THEN** those elements keep their input order relative to one another, and ordering the result again leaves it unchanged

#### Scenario: Observe stability through a user element type

- **WHEN** a vector of a user type that compares on one field and carries another the comparison never reads is ordered
- **THEN** elements with equal comparison fields appear in their input order, distinguished by the field the comparison ignored

#### Scenario: Order a move-only element type

- **WHEN** a vector whose element type owns an allocation, and is therefore never `Copy`, is ordered and later released
- **THEN** the elements are ordered by their witness and every allocation acquired is released exactly once

#### Scenario: Ordering an empty or one-element vector

- **WHEN** a program orders a vector holding no element or exactly one element
- **THEN** the vector is unchanged and no comparison is required

#### Scenario: Three engines agree on one order

- **WHEN** the same program orders the same input on the evaluator, on LLVM, and on Wasm
- **THEN** the three engines observe the same element at every index

#### Scenario: Ordering releases every allocation it acquires

- **WHEN** a program orders a vector and the vector is later released
- **THEN** every allocation the ordering acquired is released exactly once, and each element is destroyed exactly once

#### Scenario: Search a sorted vector

- **WHEN** a program searches a sorted vector for an element it holds and for one it does not
- **THEN** the present element yields its index and the missing element yields an absent value
