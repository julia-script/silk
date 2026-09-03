## ADDED Requirements

### Requirement: C-layout record pointers grant field interoperability

Foreign import and export signatures SHALL continue to admit `*const T` and `*mut T` without examining `T`, so any ordinary struct may remain an opaque handle. Only a valid C-layout record pointee SHALL grant the source-level guarantee that native code may interpret its fields according to the selected target C ABI. Every aggregate, including a C-layout record, SHALL remain rejected by value in a foreign signature.

#### Scenario: Pass a field-readable record pointer

- **WHEN** Silk passes `*mut Timespec` for a C-layout `Timespec` to a matching C function
- **THEN** C may write its declared fields and subsequent Silk field projections observe those writes

#### Scenario: Preserve an opaque ordinary pointee

- **WHEN** a foreign signature names `*mut Handle` where `Handle` is an ordinary struct
- **THEN** the pointer remains admitted while the language grants no right for C to interpret `Handle` fields

#### Scenario: Reject a C-layout record by value

- **WHEN** a foreign import or export uses a C-layout record directly as a parameter or result
- **THEN** analysis reports the existing foreign-type-not-admitted diagnostic
