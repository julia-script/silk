## ADDED Requirements

### Requirement: C-layout records format canonically

The formatter SHALL render a C-layout record header as `[pub ]extern "C" struct Name` with one space between modifiers and preserve the existing canonical struct body layout. Formatting SHALL be idempotent and SHALL retain malformed marker tokens losslessly during recovery.

#### Scenario: Format a C-layout record

- **WHEN** valid source contains irregular spacing around `pub extern "C" struct Timespec`
- **THEN** formatting emits the canonical header and existing canonical field indentation

#### Scenario: Reformat twice

- **WHEN** a C-layout record source is formatted twice
- **THEN** the second output is byte-identical to the first
