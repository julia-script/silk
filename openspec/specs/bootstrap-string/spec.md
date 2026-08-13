# bootstrap-string Specification

## Purpose

Define Silk's first-class immutable UTF-8 view and its relationship to ordinary owned string
storage without turning allocation, Unicode policy, or collection behavior into compiler policy.

## Requirements

### Requirement: Lowercase string is a distinct text value

The language SHALL provide lowercase `string` as a compiler-known immutable view of valid UTF-8.
`string` SHALL be semantically distinct from `&[u8]`, SHALL carry a lexical lifetime when it views
borrowed storage, and SHALL preserve its text identity through calls, returns, bindings, and
tooling. A `string` value MUST NOT expose mutation, capacity, ownership, or storage identity.

#### Scenario: Preserve text through a call

- **WHEN** a text literal flows through a parameter and return value typed as `string`
- **THEN** every semantic and runtime consumer continues to identify the result as valid text rather than a byte slice

#### Scenario: Keep binary data distinct

- **WHEN** a byte view happens to contain well-formed UTF-8
- **THEN** its type remains `&[u8]` until source explicitly forms a `string`

### Requirement: String access names its unit

The language and standard library SHALL expose explicitly named byte-length, immutable UTF-8 byte
view, Unicode-scalar traversal, and future grapheme traversal operations. `string` MUST NOT support
direct indexing or a generic `length` operation whose unit is unstated. Returning a UTF-8 byte view
MUST NOT allocate, copy, or permit mutation through that view.

The language SHALL provide lowercase `char` as the compiler-known scalar that names one Unicode
scalar value: a value from `0` to `0x10ffff` that is not a surrogate in `0xd800` to `0xdfff`.
`char` SHALL occupy exactly 32 bits with a 4-byte alignment, SHALL expose the equality and the
ordering operations, and SHALL order by Unicode scalar value. `char` MUST NOT expose arithmetic,
because every arithmetic operation can leave that range, and MUST NOT convert to or from any
integer without an explicit source operation.

Scalar traversal SHALL state the unit in its own type once `char` values can be built. The
traversal surface still reports a scalar as `u32` today, so `ScalarStep.scalar` and
`scalarValue` remain typed `u32` until a follow-up adds the checked `u32`-to-`char` conversion
and the representation of the surrogate hole that the conversion has to test. That follow-up
owns retyping them; until it lands, the compiler cannot state that a traversal result is a
Unicode scalar value.

#### Scenario: Reject ambiguous indexing

- **WHEN** source applies the ordinary index operator to a `string`
- **THEN** semantic analysis rejects the expression and requires an explicitly selected byte, scalar, or grapheme operation

#### Scenario: Inspect encoded length

- **WHEN** source requests the byte length and UTF-8 bytes of a valid `string`
- **THEN** it observes the exact encoded byte count and one immutable allocation-free view

#### Scenario: Keep a scalar value distinct from a count

- **WHEN** source supplies a `u32` where a `char` is expected, or a `char` where a `u32` is expected
- **THEN** semantic analysis rejects the expression rather than converting between the two

#### Scenario: Refuse arithmetic on a scalar value

- **WHEN** source applies an arithmetic or bitwise operator to two `char` operands
- **THEN** semantic analysis reports the operand-type diagnostic rather than selecting a `char` operation

### Requirement: String conversions are explicit

Text literals SHALL have type `string`. Converting `string` to owned `String`, owned `String` to a
borrowed `string`, `string` to UTF-8 bytes, or bytes to `string` SHALL require an explicit source
operation. No conversion SHALL allocate implicitly, and no contextual expected type SHALL silently
change a text literal into owned storage or binary data.

#### Scenario: Refuse an implicit owned copy

- **WHEN** a context expects `String` and source supplies a text literal or `string`
- **THEN** analysis rejects the implicit conversion and requires the effectful copying operation

#### Scenario: Borrow owned text explicitly

- **WHEN** source requests a view of a live `String` through a shared borrow
- **THEN** it receives an allocation-free `string` that cannot outlive the owner borrow

### Requirement: Unsafe UTF-8 construction has one narrow contract

The compiler SHALL expose one unsafe, allocation-free conversion from `&[u8]` to `string`. The
caller MUST establish that the complete byte view is valid UTF-8 and remains live for the returned
lexical view. The primitive SHALL perform no required validation; invalid input violates the unsafe
operation contract. Safe source SHALL instead use a stdlib validator that returns either a
`string` borrowing the original bytes or a typed invalid-UTF-8 value without publishing a partial
string.

#### Scenario: Validate bytes safely

- **WHEN** safe source validates a complete well-formed UTF-8 byte view
- **THEN** it receives a `string` over the same live bytes without allocation or copying

#### Scenario: Reject malformed UTF-8 safely

- **WHEN** safe source validates malformed UTF-8
- **THEN** it receives the typed invalid-UTF-8 result and no `string` value

#### Scenario: Require unsafe authority

- **WHEN** source invokes unchecked UTF-8 construction outside an unsafe boundary
- **THEN** semantic analysis rejects the call before HIR or executable artifacts are produced

### Requirement: Owned String remains ordinary standard-library source

The shipped standard library SHALL define nominal `String` as an owner of valid UTF-8 storage using
ordinary Silk allocation and collection facilities. The compiler, evaluator, and backends MUST NOT
recognize that owner by module or type spelling, choose its capacity or growth policy, or give it a
special ABI. The stdlib SHALL provide an effectful copy from `string` and an allocation-free
lexical `string` view of a shared owner.

#### Scenario: Copy static text into ownership

- **WHEN** source copies a text literal into `String` with an available allocator
- **THEN** the result independently owns the same valid UTF-8 content or reports the existing typed allocation failure

#### Scenario: Keep owner policy out of the compiler

- **WHEN** an equivalent user-defined owner uses the same safe and unsafe primitives
- **THEN** compilation applies the same ownership, layout, and lowering rules without consulting its declaration name

### Requirement: String equality is exact and normalization-free

Ordinary `string` equality SHALL compare the exact Unicode scalar sequence, equivalently the
canonical UTF-8 bytes of valid inputs. The language and compiler MUST NOT normalize, case-fold, or
apply locale rules implicitly. Unicode normalization and locale-sensitive comparison SHALL be
explicit stdlib operations with independently versioned policy.

#### Scenario: Distinguish canonically equivalent spellings

- **WHEN** one string contains precomposed `é` and another contains `e` followed by a combining acute accent
- **THEN** ordinary equality reports them as unequal until source explicitly normalizes them
