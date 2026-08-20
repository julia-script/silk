## ADDED Requirements

### Requirement: Text-derived views use ordinary borrow provenance

References to `string`, slices of `string`, and UTF-8 byte views derived from a runtime `string`
SHALL use the same stable place, access, reborrow, last-use, return-origin, and escape rules as other
ordinary values and slices. A text-derived byte view SHALL remain `&[u8]` and SHALL retain the
backing text storage provenance without allocation or a text-specific lifetime exception.

#### Scenario: Borrow a string value

- **WHEN** source passes `&string`, `&mut string`, or `&[string]` through a compatible ordinary boundary
- **THEN** analysis applies the ordinary reference or slice rules without a text-specific diagnostic

#### Scenario: Return text bytes from one borrowed source

- **WHEN** an ordinary function returns a UTF-8 byte view derived from exactly one borrowed string parameter
- **THEN** the caller receives a lexical `&[u8]` tied to that parameter's source owner

#### Scenario: Reject escaping runtime text bytes

- **WHEN** a byte view derived from a local runtime string would outlive its backing owner
- **THEN** ownership reports the ordinary escaping-borrow diagnostic
