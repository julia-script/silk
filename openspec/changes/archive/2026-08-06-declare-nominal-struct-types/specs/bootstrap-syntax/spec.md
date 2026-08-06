## ADDED Requirements

### Requirement: Struct declarations parse losslessly

Every top-level declaration position SHALL accept `pub` optionally followed by `struct`, a name,
and a braced field list. Each field SHALL retain an optional `pub`, a name, a colon, and an explicit
local, selected, or namespace-qualified type path. Fields SHALL be recognized sequentially without
requiring punctuation between complete field declarations. The concrete tree SHALL own every token
and trivia slice exactly once and SHALL NOT claim that names, types, or visibility are semantically
valid.

#### Scenario: Parse a public struct

- **WHEN** source declares `pub struct Token { pub kind: I32 lexeme: Text }`
- **THEN** the tree retains the public struct, both ordered fields, the public first field, and every delimiter and trivia slice

#### Scenario: Parse an empty struct

- **WHEN** source declares `struct Marker {}`
- **THEN** the tree retains one default-private struct with an empty field list and exact braces

#### Scenario: Parse a qualified field type

- **WHEN** a field declares type `Tree.Node`
- **THEN** its type syntax retains the namespace, dot, and member independently of later name resolution

### Requirement: Struct recovery remains inside its declaration

A missing struct name, brace, field name, colon, or field type SHALL become explicit recovery data
at the nearest struct or field boundary. Recovery SHALL resume at the next `pub`, field-shaped name,
closing brace, top-level declaration keyword, or end-of-file. Unexpected tokens SHALL remain in
error regions, and a damaged struct MUST NOT consume a following top-level declaration.

#### Scenario: Recover a missing field type

- **WHEN** one field ends after its colon before a following field
- **THEN** the first field contains an explicit missing type and the following field parses independently

#### Scenario: Recover a missing closing brace

- **WHEN** a struct omits its closing brace before a following function declaration
- **THEN** the struct receives a missing brace and the function remains a separate top-level declaration

#### Scenario: Recover a missing field name

- **WHEN** a struct contains `pub : I32` before its closing brace
- **THEN** the field retains an explicit missing name and the struct retains its closing brace
