## ADDED Requirements

### Requirement: Top-level constants are lossless and recoverable

The parser SHALL recognize `[pub] const <name>: <type> = <literal>` as a top-level constant
declaration, retaining every modifier, separator, literal token, trivia slice, and source span.
Recovery SHALL stop at the next top-level declaration or end-of-file and SHALL keep missing names,
colons, types, equals signs, and literals explicit.

#### Scenario: Parse a public typed integer constant

- **WHEN** source spells `pub const opcode_add: u8 = 1`
- **THEN** the source tree contains one complete constant declaration with exact token provenance

#### Scenario: Recover before a following function

- **WHEN** a damaged constant declaration is followed by a valid function declaration
- **THEN** the damage remains inside the constant and the function remains a separate complete branch
