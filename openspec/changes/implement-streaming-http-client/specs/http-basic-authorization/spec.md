## Purpose

Define explicit bounded Basic authorization encoding without inferred character sets or credentials.

## ADDED Requirements

### Requirement: Atomic explicit Basic encoding

The pure encoder SHALL accept caller-encoded username/password bytes, scratch and output storage and return Basic followed by a space and padded Base64 of username:password. It SHALL reject username colon and controls/DEL in either input. It SHALL validate arithmetic and both capacities before output mutation and leave output unchanged on failure. It SHALL allocate nothing and infer no charset. HTTP SHALL reject plaintext Basic unless AllowInsecureBasic is explicit, retain origin binding, and reject multiple authorization sources.

#### Scenario: RFC credential vector

- **WHEN** username Aladdin and password open sesame are encoded
- **THEN** output is Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==

#### Scenario: Atomic rejection

- **WHEN** credentials are invalid or storage is too small
- **THEN** the precise failure is returned and output is unchanged
