## ADDED Requirements

### Requirement: Text and byte literals retain distinct semantic types

A valid text literal SHALL elaborate directly to `string` while retaining its decoded immutable
UTF-8 data, source provenance, and program-lifetime storage. A byte-string literal SHALL continue
to elaborate to an immutable `u8` view. The compiler MUST NOT use one shared slice type for both
literal categories.

#### Scenario: Elaborate corresponding literal spellings

- **WHEN** source contains `"silk"` and `b"silk"`
- **THEN** the first expression has type `string`, the second has type `&[u8]`, and both retain the same four encoded bytes

#### Scenario: Preserve an allocation-free text literal

- **WHEN** a valid non-ASCII text literal is evaluated without conversion to owned `String`
- **THEN** it remains an allocation-free program-lifetime `string` with exact decoded UTF-8 content
