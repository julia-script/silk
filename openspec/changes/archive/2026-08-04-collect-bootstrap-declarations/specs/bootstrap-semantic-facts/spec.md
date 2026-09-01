## MODIFIED Requirements

### Requirement: First function declaration fact

Semantic analysis SHALL retain the parse result and publish one ordered function fact for every
direct function declaration in the source-file tree. Each function fact SHALL expose its declaration,
returned-expression fact, and return compatibility together. Every declaration SHALL have a
deterministic source-local identity whose ordinal matches concrete declaration order, public
visibility, parameter count, declared-name state, return-type-reference state, and exact syntax
provenance. Name lookup SHALL distinguish exactly one match, no match, and multiple matches without
discarding any collected declaration.

#### Scenario: Collect the accepted declaration

- **WHEN** the accepted fixture `pub fn main() -> I32 { return 42 }` is analyzed
- **THEN** one public parameterless function fact named `main` is available at ordinal zero with provenance to its original function and name syntax

#### Scenario: Collect two declarations in order

- **WHEN** parsed `answer` and `main` functions appear in that source order
- **THEN** two function facts are published with ordinals zero and one and lookup resolves each present unique name

#### Scenario: Preserve a missing declaration name

- **WHEN** a parsed function contains a missing identifier before its parameter list
- **THEN** its function fact remains available with an unavailable name, no name lookup entry is invented, and no semantic diagnostic duplicates the parser's missing-token diagnostic

#### Scenario: Keep duplicate declarations explicit

- **WHEN** two declarations have the same present name
- **THEN** both function facts remain in source order, lookup reports multiple matches, and one `SEM0003` diagnostic identifies the later duplicate name

### Requirement: Built-in I32 type fact

Semantic analysis SHALL resolve the exact return-type spelling `I32` for every function to the
bootstrap signed 32-bit integer type. Any other present identifier spelling SHALL remain an explicit
unresolved type and produce one `SEM0001` semantic diagnostic at that identifier's source span.
Missing or syntax-damaged return-type syntax SHALL remain unavailable without duplicating its parser
diagnostic.

#### Scenario: Resolve bootstrap return types independently

- **WHEN** two functions each declare `I32`
- **THEN** each function fact carries its own resolved built-in signed 32-bit return type and syntax provenance

#### Scenario: Diagnose one unknown return type

- **WHEN** one of two functions declares `Mystery`
- **THEN** only that function's return-type fact is unresolved and one `SEM0001` diagnostic identifies `Mystery`

#### Scenario: Do not guess a damaged return type

- **WHEN** parser recovery leaves one function's return-type identifier missing or inside an error region
- **THEN** that function's return-type fact is unavailable without changing the other function's facts or repeating the syntax diagnostic

### Requirement: Exact decimal I32 value fact

Semantic analysis SHALL interpret each present decimal-integer return expression as an exact
non-negative integer and publish its `I32` value when it is at most `2147483647`. A larger value
SHALL remain explicitly unavailable and produce one `SEM0002` diagnostic covering the complete
literal span. Analysis MUST NOT lose precision because of the host numeric representation.

#### Scenario: Analyze two integer values independently

- **WHEN** two functions return `42` and `0`
- **THEN** their ordered function facts contain the exact available `I32` values `42` and `0`

#### Scenario: Accept the positive I32 boundary

- **WHEN** one returned literal is `2147483647`
- **THEN** that function's expression has the exact available `I32` value `2147483647`

#### Scenario: Diagnose one out-of-range integer

- **WHEN** one returned literal is `2147483648`
- **THEN** only that function's value fact is unavailable and one `SEM0002` diagnostic covers the entire literal

#### Scenario: Preserve a missing integer expression

- **WHEN** parser recovery inserts one function's decimal-integer token
- **THEN** that function's value and expression-type facts are unavailable without affecting other function facts or adding a duplicate semantic diagnostic

### Requirement: First return compatibility fact

Every function fact SHALL publish return compatibility as `Compatible` only when that function's
declared return type and returned expression type are both available and equal. It SHALL publish
`Unavailable` when either input is unresolved, missing, or invalid and MUST NOT let one function's
facts determine another function's compatibility.

#### Scenario: Check two compatible returns

- **WHEN** two functions declare `I32` and return available `I32` integers
- **THEN** each function independently reports `Compatible`

#### Scenario: Withhold only damaged compatibility

- **WHEN** one function's declared return type is unresolved and another function is valid
- **THEN** the damaged function reports `Unavailable` and the valid function remains `Compatible`

### Requirement: Semantic diagnostics are deterministic data

The semantic result SHALL expose semantic diagnostics as a separate readonly collection while
retaining lexical and parser diagnostics through its original parse result. Every semantic
diagnostic SHALL contain a stable code, severity, concise message, reason data, and source-owned
primary span. Present duplicate names after the first occurrence SHALL produce `SEM0003` at each
later name span. Semantic diagnostics SHALL be ordered by primary span and stable code, and semantic
source mistakes SHALL return complete ordered facts and diagnostics rather than throw or fail an
Effect.

#### Scenario: Repeat multi-function semantic analysis

- **WHEN** equivalent malformed multi-function parse results are analyzed repeatedly in fresh processes
- **THEN** their declaration identities, fact order, lookup outcomes, source provenance, and semantic diagnostics are identical

#### Scenario: Keep diagnostic phases separate

- **WHEN** one source contains parser recovery, a duplicate present name, and an unknown present return-type identifier
- **THEN** lexical, parser, and semantic diagnostics remain in their owning collections and semantic diagnostics are ordered by their exact primary spans

#### Scenario: Diagnose every later duplicate

- **WHEN** three declarations share the same present name
- **THEN** the second and third names each produce one `SEM0003` diagnostic while the first remains the original declaration
