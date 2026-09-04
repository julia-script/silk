# Bootstrap Hashed Collections Specification

## Purpose

Define deterministic, ownership-safe hashed maps and sets in ordinary Silk source so programs can
perform keyed lookup without introducing compiler-known hashing behavior.

## Requirements

### Requirement: HashKey declares an equivalence and a seeded hash

The standard library SHALL declare a `HashKey<T>` interface with exactly two operations: an
equivalence over two values of the key type resulting in `bool`, and a hash of one value under one
`HashSeed` resulting in `u64`. The equivalence operation SHALL be named so that an operator spells
it, so a generic body bounded by `HashKey` can compare keys through that operator. Both operations
SHALL be declared over the interface's own type parameter and SHALL create no effect requirement, no
provider slot, and no runtime dispatch.

#### Scenario: Witness both operations from one conformance

- **WHEN** a type declares a conformance to `HashKey` mapping both operations
- **THEN** the conformance is admitted and a generic bounded by `HashKey` accepts that type as a type argument

#### Scenario: Refuse a partial witness

- **WHEN** a conformance to `HashKey` maps only one of the two operations
- **THEN** the conformance is reported, naming the operation it does not supply

#### Scenario: Reach both halves from one generic body

- **WHEN** a body bounded by `HashKey` compares two keys and hashes one of them
- **THEN** both calls resolve against the bound and evaluate through the type argument's witness

### Requirement: Equivalent keys hash equally under one seed

A `HashKey` witness SHALL compute equal hash values for any two values its own equivalence operation
reports as equivalent, for every seed. This is a requirement on the witness rather than a property
the collections verify: a witness that breaks it makes a present key unreachable, and the
collections can neither detect nor recover from that.

#### Scenario: Two equivalent keys reach one entry

- **WHEN** a map holds an entry under one key and is looked up with a second key its witness reports equivalent to the first
- **THEN** the lookup finds the existing entry rather than reporting absence or creating a second one

#### Scenario: Insert under an equivalent key replaces rather than duplicates

- **WHEN** a map holding an entry is inserted into under a key equivalent to the existing one
- **THEN** the map's length is unchanged and the entry's value is the newly inserted one

### Requirement: HashMap requires a HashKey witness for its key type

`HashMap<K, V>` SHALL bound `K` by `HashKey` and SHALL support insert, lookup, and removal. Insert
SHALL report whether an entry already existed under an equivalent key. Lookup SHALL distinguish
presence from absence. Removal SHALL report whether an entry was removed. Insert SHALL fail only
with the typed `OutOfMemoryError` failure of the underlying allocator requirement, and a failed insert
SHALL leave the map's prior entries, length, and capacity intact.

#### Scenario: Insert, look up, and remove

- **WHEN** entries are inserted, looked up, and removed from a map
- **THEN** each lookup before removal finds the value most recently inserted under an equivalent key, and each lookup after removal reports absence

#### Scenario: Refuse a key type with no witness

- **WHEN** a program instantiates `HashMap` with a key type that has no `HashKey` conformance
- **THEN** the instantiation is reported rather than accepted

#### Scenario: Preserve the map when growth fails

- **WHEN** an insert requires growth and the replacement allocation fails
- **THEN** the insert fails with `OutOfMemoryError` and every prior entry remains present at its own key

### Requirement: HashSet requires a HashKey witness for its element type

`HashSet<T>` SHALL bound `T` by `HashKey` and SHALL support insert, membership, and removal. Insert
SHALL report whether the element was already present, and SHALL NOT store a second element
equivalent to one already held.

#### Scenario: Insert a duplicate element

- **WHEN** an element equivalent to one already held is inserted
- **THEN** the insert reports that it was already present and the set's length is unchanged

#### Scenario: Test membership and remove

- **WHEN** a set is tested for an element it holds and that element is then removed
- **THEN** membership holds before the removal and does not hold after it

### Requirement: One seed fixes one iteration order

A hashed collection SHALL be constructed with a `HashSeed`, and the order in which it presents its
entries SHALL be a function of that seed and the sequence of operations performed on it alone. It
MUST NOT depend on allocation addresses, on wall-clock or monotonic time, or on any ambient source
of entropy. Two runs of one program constructing one collection with one seed and performing one
sequence of operations SHALL observe one order on every supported target.

#### Scenario: Two runs over one seed agree

- **WHEN** one program builds a collection with a fixed seed and records the order of its entries, and is run twice
- **THEN** the two runs record the same order

#### Scenario: The order is the seed's, not the address's

- **WHEN** one program builds two collections with one seed and one sequence of insertions, with unrelated allocation between them
- **THEN** both collections present their entries in the same order

### Requirement: A hashed collection owns what it stores

A hashed collection SHALL take ownership of the keys and values inserted into it and SHALL release
each exactly once: when an entry is removed, when an insert replaces the value held under an
equivalent key, and when the collection itself is dropped, including a collection dropped while
non-empty. A move-only key or value SHALL be usable without being copied.

#### Scenario: Drop a non-empty map holding move-only values

- **WHEN** a map holding move-only values goes out of scope without being emptied
- **THEN** every value it holds is released exactly once, and acquires equal releases

#### Scenario: Overwrite a move-only value

- **WHEN** an insert replaces the value held under an equivalent key
- **THEN** the replaced value is released exactly once and the new value is the one the map holds

#### Scenario: Remove transfers ownership out

- **WHEN** a move-only value is removed from a map
- **THEN** ownership passes to the caller and the map does not also release it

### Requirement: No engine contains a hash operation

Hashing SHALL be ordinary Silk source. Semantic analysis, the HIR, the MIR, and LLVM lowering MUST
NOT contain any operation that computes a hash,
recognize a hashed collection by spelling, or treat `HashKey` as anything other than an ordinary
interface. Every hash a program computes SHALL be a function some witness declared in Silk.

#### Scenario: The MIR names no hash operation

- **WHEN** a program using `HashMap` is lowered
- **THEN** its MIR contains no hash operation, and every hash it computes appears as an ordinary call to a witness's own function

#### Scenario: The collections are ordinary library source

- **WHEN** tooling resolves a `HashMap` operation
- **THEN** it navigates to canonical Silk source, and only the enclosed allocation calls resolve to `Intrinsic`

### Requirement: The collections are built over the owned-allocation substrate

`HashMap` and `HashSet` SHALL be implemented in ordinary Silk over the allocator requirement and
typed storage, using the same substrate `Vector` uses. They MUST NOT require a new compiler
primitive, and their behavior SHALL be identical in LLVM-generated native and WebAssembly artifacts.

#### Scenario: Identical behavior across engines

- **WHEN** one program using hashed collections runs in LLVM-generated native and WebAssembly artifacts
- **THEN** both produce the same result and the same iteration order
