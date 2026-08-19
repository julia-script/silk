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
