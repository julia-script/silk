## ADDED Requirements

### Requirement: C-layout record syntax names an explicit ABI

The declaration grammar SHALL parse `[pub] extern "C" struct Name { ... }` as one struct declaration retaining the `extern` marker, ABI literal, optional visibility, name, and ordered fields. The grammar SHALL recover malformed ABI-bearing struct headers without reinterpreting their fields or following declarations as foreign functions.

#### Scenario: Parse a public C-layout record

- **WHEN** source declares `pub extern "C" struct Timespec { seconds: i64 }`
- **THEN** the concrete syntax tree contains one public struct declaration with ABI `C` and one ordered field

#### Scenario: Recover a missing ABI literal

- **WHEN** source begins `extern struct Broken { value: i32 }` and then declares another item
- **THEN** recovery retains the damaged struct boundary and parses the following item independently
