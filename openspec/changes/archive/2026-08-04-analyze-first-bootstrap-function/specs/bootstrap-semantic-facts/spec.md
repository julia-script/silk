## Purpose

Give the first parsed Silk function deterministic declaration, type, value, and compatibility
meaning while keeping incomplete syntax explicit and deferring semantic intermediate representations.

## ADDED Requirements

### Requirement: First function declaration fact

Semantic analysis SHALL retain the parse result and publish exactly one declaration fact for the
first function syntax. The fact SHALL expose a deterministic analysis-local identity, public
visibility, parameter count, declared-name state, return-type-reference state, and exact source
provenance. A present declaration name SHALL support lookup of that same declaration fact by its
source spelling.

#### Scenario: Collect the accepted declaration

- **WHEN** the accepted fixture `pub fn main() -> I32 { return 42 }` is analyzed
- **THEN** one public parameterless function declaration named `main` is available with provenance to its original function and name syntax

#### Scenario: Preserve a missing declaration name

- **WHEN** the parsed function contains a missing identifier before its parameter list
- **THEN** the declaration fact remains available with an unavailable name, no name lookup entry is invented, and no semantic diagnostic duplicates the parser's missing-token diagnostic

### Requirement: Built-in I32 type fact

Semantic analysis SHALL resolve the exact return-type spelling `I32` to the bootstrap signed 32-bit
integer type. Any other present identifier spelling SHALL remain an explicit unresolved type and
produce one `SEM0001` semantic diagnostic at the identifier's source span. Missing or
syntax-damaged return-type syntax SHALL remain unavailable without duplicating its parser
diagnostic.

#### Scenario: Resolve the bootstrap return type

- **WHEN** the accepted fixture is analyzed
- **THEN** its return-type reference resolves to the built-in signed 32-bit integer type

#### Scenario: Diagnose an unknown return type

- **WHEN** the source spells `pub fn main() -> Mystery { return 42 }`
- **THEN** the return-type fact is unresolved and one `SEM0001` diagnostic identifies `Mystery`

#### Scenario: Do not guess a damaged return type

- **WHEN** parser recovery leaves the return-type identifier missing or inside an error region
- **THEN** the return-type fact is unavailable and semantic analysis does not invent a type or repeat the syntax diagnostic

### Requirement: Exact decimal I32 value fact

Semantic analysis SHALL interpret a present decimal-integer expression as an exact non-negative
integer and publish its `I32` value when it is at most `2147483647`. A larger value SHALL remain
explicitly unavailable and produce one `SEM0002` semantic diagnostic covering the complete literal
span. Analysis MUST NOT lose precision because of the host numeric representation.

#### Scenario: Analyze the accepted integer value

- **WHEN** the accepted fixture is analyzed
- **THEN** its integer expression has the built-in `I32` type and exact value `42`

#### Scenario: Accept the positive I32 boundary

- **WHEN** the returned literal is `2147483647`
- **THEN** the integer expression has the exact available `I32` value `2147483647`

#### Scenario: Diagnose an out-of-range integer

- **WHEN** the returned literal is `2147483648`
- **THEN** the value fact is unavailable and one `SEM0002` diagnostic covers the entire literal

#### Scenario: Preserve a missing integer expression

- **WHEN** parser recovery inserts the decimal-integer token
- **THEN** the value and expression-type facts are unavailable without a duplicate semantic diagnostic

### Requirement: First return compatibility fact

Semantic analysis SHALL publish return compatibility as `Compatible` only when the declared return
type and returned expression type are both available and equal. It SHALL publish `Unavailable`
when either input fact is unresolved, missing, or invalid and MUST NOT treat unavailable facts as a
successful compatibility check.

#### Scenario: Check the accepted return

- **WHEN** the accepted fixture is analyzed
- **THEN** its return compatibility is `Compatible`

#### Scenario: Withhold compatibility for an unknown type

- **WHEN** the declared return type is unresolved
- **THEN** return compatibility is `Unavailable` even though the integer expression has an `I32` type

### Requirement: Semantic diagnostics are deterministic data

The semantic result SHALL expose semantic diagnostics as a separate readonly collection while
retaining lexical and parser diagnostics through its original parse result. Every semantic
diagnostic SHALL contain a stable code, severity, concise message, reason data, and source-owned
primary span. Semantic diagnostics SHALL be ordered by primary span and stable code, and semantic
source mistakes SHALL return facts and diagnostics rather than throw or fail an Effect.

#### Scenario: Repeat semantic analysis

- **WHEN** equivalent malformed parse results are analyzed repeatedly in fresh processes
- **THEN** their declaration identities, fact states, source provenance, and semantic diagnostics are identical

#### Scenario: Keep diagnostic phases separate

- **WHEN** one source contains both parser recovery and an unknown present return-type identifier
- **THEN** lexical, parser, and semantic diagnostics remain in their owning collections and only the unknown identifier produces a semantic diagnostic
