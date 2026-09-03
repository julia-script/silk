## ADDED Requirements

### Requirement: C-layout structs are concrete source-usable records

`[pub] extern "C" struct` SHALL declare an ordinary nominal Silk value whose construction, projection, visibility, borrowing, ownership, and Copy behavior follow the existing struct rules. A C-layout struct SHALL declare no type parameters. Its fields SHALL be recursively limited to fixed-width integers, `isize`, `usize`, `f32`, `f64`, raw pointers, non-zero fixed arrays of permitted field types, and other C-layout structs. Unit, `bool`, `char`, strings, references, slices, ordinary structs by value, unions, enums, callable or Effect values, represented types, type parameters, and zero-length arrays SHALL be rejected as C-layout fields.

#### Scenario: Use a C-layout record as a Silk value

- **WHEN** source constructs a C-layout `Timespec`, borrows it mutably, forms a raw pointer, and later projects its fields
- **THEN** the same ordinary struct value and ownership rules apply at every step

#### Scenario: Reject a generic C-layout record

- **WHEN** source declares `extern "C" struct Box<T> { value: T }`
- **THEN** analysis reports the C-layout declaration restriction at the type parameters and grants no C-layout promise

#### Scenario: Accept recursively representable fields

- **WHEN** a C-layout record contains fixed-width scalars, a raw pointer, a non-zero fixed array, and another C-layout record
- **THEN** analysis records the complete declaration as C-layout

#### Scenario: Reject an ordinary nested struct

- **WHEN** a C-layout record contains an ordinary non-C-layout struct by value
- **THEN** analysis reports the unsupported C-layout field at that field type and grants no C-layout promise to the outer record

#### Scenario: Reject an inline C-layout cycle

- **WHEN** one or more C-layout records recursively contain themselves by value
- **THEN** analysis preserves the canonical inline-cycle diagnostic and grants no C-layout promise to any member of the cycle
