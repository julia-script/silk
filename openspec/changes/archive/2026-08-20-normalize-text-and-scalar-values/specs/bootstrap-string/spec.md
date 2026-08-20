## ADDED Requirements

### Requirement: Text uses ordinary value and borrow semantics

`string` SHALL be an immutable UTF-8 value whose references, byte views, moves, equality, length, calls, returns, and lexical lifetimes obey the ordinary type and ownership rules. A `string` and `&[u8]` SHALL remain distinct logical types even when they name identical valid UTF-8 bytes.

#### Scenario: Borrow string bytes

- **WHEN** a string's byte view is used within the string owner's lifetime
- **THEN** the view preserves that owner's provenance without allocation or a text-specific type exception

#### Scenario: Reject an escaping text view

- **WHEN** a function returns a byte view of a local string
- **THEN** ownership reports the same escaping-borrow error as for any local owner

### Requirement: UTF-8 traversal converts scalars explicitly and safely

Standard-library traversal SHALL decode validated UTF-8 into `char` through a checked scalar conversion. Invalid scalar values SHALL be rejected through the conversion's declared boundary, and no implicit truncation or locale behavior SHALL occur.

#### Scenario: Traverse non-ASCII text

- **WHEN** source traverses a string containing a multi-byte Unicode scalar
- **THEN** evaluation, LLVM, and Wasm produce the same `char` and next byte position

## MODIFIED Requirements

### Requirement: String access names its unit

The language and standard library SHALL expose explicitly named byte-length, immutable UTF-8 byte
view, Unicode-scalar traversal, and future grapheme traversal operations. `string` MUST NOT support
direct indexing or a generic `length` operation whose unit is unstated. Returning a UTF-8 byte view
MUST NOT allocate, copy, or permit mutation through that view.

The language SHALL provide lowercase `char` as the compiler-known scalar that names one Unicode
scalar value: a value from `0` to `0x10ffff` that is not a surrogate in `0xd800` to `0xdfff`.
`char` SHALL occupy exactly 32 bits with a 4-byte alignment, SHALL expose equality and ordering by
Unicode scalar value, and SHALL NOT expose arithmetic. Converting `u32` to `char` SHALL require one
explicit checked source operation returning `Option<char>`; converting an already valid `char` to
`u32` SHALL require one explicit total source operation. Scalar traversal SHALL expose `char`, not
an integer approximation of the scalar.

#### Scenario: Reject ambiguous indexing

- **WHEN** source applies the ordinary index operator to a `string`
- **THEN** semantic analysis rejects the expression and requires an explicitly selected byte, scalar, or grapheme operation

#### Scenario: Inspect encoded length

- **WHEN** source requests the byte length and UTF-8 bytes of a valid `string`
- **THEN** it observes the exact encoded byte count and one immutable allocation-free view

#### Scenario: Keep a scalar value distinct from a count

- **WHEN** source supplies a `u32` where a `char` is expected, or a `char` where a `u32` is expected
- **THEN** semantic analysis rejects the expression rather than converting between the two

#### Scenario: Convert a checked scalar explicitly

- **WHEN** source checks a valid non-surrogate `u32` and then explicitly converts the resulting `char` back to `u32`
- **THEN** it receives `Some<char>` followed by the original scalar value

#### Scenario: Reject invalid scalar integers

- **WHEN** checked conversion receives a surrogate or a value above `0x10ffff`
- **THEN** it returns `None` without truncation or a trap

#### Scenario: Refuse arithmetic on a scalar value

- **WHEN** source applies an arithmetic or bitwise operator to two `char` operands
- **THEN** semantic analysis reports the operand-type diagnostic rather than selecting a `char` operation
