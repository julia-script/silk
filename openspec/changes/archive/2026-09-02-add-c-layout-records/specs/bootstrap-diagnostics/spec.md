## ADDED Requirements

### Requirement: Invalid C-layout contracts have stable diagnostics

Unsupported C-layout ABIs, generic C-layout declarations, and unsupported C-layout field types SHALL each produce stable semantic diagnostic codes with structured reasons and source-owned primary spans. A field diagnostic SHALL point at the offending declared type. Diagnostics SHALL preserve the nominal declaration for tooling while withholding the invalid C-layout promise.

#### Scenario: Diagnose an unsupported record ABI

- **WHEN** source declares `extern "system" struct Record { value: i32 }`
- **THEN** analysis reports the unsupported foreign ABI at the ABI literal and does not record a C-layout promise

#### Scenario: Diagnose an unsupported field

- **WHEN** a C-layout record declares a field of type `string`
- **THEN** analysis reports the C-layout field diagnostic at `string` with structured reason data naming the field type
