# Bootstrap Semantic Facts Specification

## Purpose

Give the first parsed Silk function deterministic declaration, type, value, and compatibility
meaning while keeping incomplete syntax explicit and deferring semantic intermediate representations.

## Requirements

### Requirement: First function declaration fact
Semantic analysis SHALL retain the parse result and publish one ordered function fact for every
direct function declaration in the source-file tree. Each function fact SHALL expose its declaration,
returned-expression fact, and return compatibility together. Every declaration SHALL have a
deterministic source-local identity whose ordinal matches concrete declaration order, public
visibility, exact concrete parameter count, declared-name state, return-type-reference state, and exact syntax
provenance. Name lookup SHALL distinguish exactly one match, no match, and multiple matches without
discarding any collected declaration.

#### Scenario: Collect the accepted declaration
- **WHEN** the accepted fixture `pub fn main() -> I32 { return 42 }` is analyzed
- **THEN** one public function fact named `main` is available at ordinal zero with zero parameters and provenance to its original function and name syntax

#### Scenario: Count typed parameters without resolving them
- **WHEN** a function has two complete typed parameters
- **THEN** its declaration fact reports parameter count two while parameter declaration meaning remains deferred

#### Scenario: Collect two declarations in order
- **WHEN** parsed `answer` and `main` functions appear in that source order
- **THEN** two function facts are published with ordinals zero and one and lookup resolves each present unique name

#### Scenario: Preserve a missing declaration name
- **WHEN** a parsed function contains a missing identifier before its parameter list
- **THEN** its function fact remains available with an unavailable name, no name lookup entry is invented, and no semantic diagnostic duplicates the parser's missing-token diagnostic

#### Scenario: Keep duplicate declarations explicit
- **WHEN** two declarations have the same present name
- **THEN** both function facts remain in source order, lookup reports multiple matches, and one `SEM0003` diagnostic identifies the later duplicate name

### Requirement: New value-carrying syntax remains explicitly deferred
Before parameter resolution and call checking are implemented, semantic analysis SHALL retain the
parse result and declaration facts for functions containing parameters, bare identifiers, or call
arguments without inventing parameter identities, bindings, argument compatibility, or values.
Bare-identifier expression meaning SHALL be unavailable. Existing top-level call-name resolution
and target-return-type facts SHALL remain available independently of the unchecked argument list.

#### Scenario: Analyze an unresolved parameter reference
- **WHEN** `identity(value: I32)` returns `value`
- **THEN** the declaration reports one parameter while the bare-identifier expression and return compatibility remain explicitly unavailable without a semantic error

#### Scenario: Retain an unchecked call argument
- **WHEN** `main` returns a uniquely resolved call `identity(42)`
- **THEN** the call relationship still resolves to `identity` while no argument binding or compatibility is claimed

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
declared return type and returned expression type are both available and equal. An integer expression
uses its existing `I32` type. A uniquely resolved call expression SHALL use its target declaration's
resolved return type even for forward or self references. Compatibility SHALL be `Unavailable` when
the caller type, expression type, or call target is unresolved, missing, ambiguous, or invalid, and
one function's compatibility MUST NOT overwrite another function's facts.

#### Scenario: Check an integer return
- **WHEN** a function declares `I32` and returns an available `I32` integer
- **THEN** that function reports `Compatible`

#### Scenario: Check a resolved call return
- **WHEN** `answer` declares `I32` and `main` declares `I32` and returns a uniquely resolved call to `answer`
- **THEN** the call expression has type `I32` and `main` reports `Compatible`

#### Scenario: Withhold compatibility for an unknown call
- **WHEN** a function returns a call whose reference is missing
- **THEN** the call expression type and caller return compatibility are `Unavailable`

#### Scenario: Withhold compatibility for an ambiguous call
- **WHEN** a function returns a call whose reference is ambiguous
- **THEN** the call expression type and caller return compatibility are `Unavailable` without selecting a declaration

#### Scenario: Withhold compatibility for an unresolved callee type
- **WHEN** a call resolves uniquely to a declaration whose return type is unresolved or unavailable
- **THEN** the call reference remains resolved but its expression type and caller return compatibility are `Unavailable`

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

### Requirement: First top-level call reference fact
Semantic analysis SHALL resolve every present zero-argument call callee against all collected
top-level declarations without depending on declaration order. A call-reference fact SHALL be
`Resolved` with the exact target declaration identity and callee syntax when exactly one declaration
matches, `Missing` when none matches, `Ambiguous` when multiple declarations match, or unavailable
when parser recovery did not supply a usable callee. Forward and self references SHALL resolve by
the same rules because this phase records relationships and does not execute functions.

#### Scenario: Resolve a call to an earlier declaration
- **WHEN** `answer` is declared before `main` and `main` returns `answer()`
- **THEN** the call reference resolves to `answer`'s exact declaration identity and preserves the call-site identifier span

#### Scenario: Resolve a forward call
- **WHEN** `main` returns `answer()` and `answer` is declared later in the same source
- **THEN** the call resolves to the later declaration independently of source ordering

#### Scenario: Resolve a self reference as data
- **WHEN** a function returns a call to its own unique name
- **THEN** the reference resolves to that declaration without evaluating the call or deciding recursion policy

#### Scenario: Preserve an ambiguous target
- **WHEN** a call name matches multiple duplicate declarations
- **THEN** the call reference is ambiguous, exposes all matching declaration identities in source order, and does not select one target

### Requirement: Unknown call target diagnostic
A present call name with no matching declaration SHALL produce one `SEM0004` semantic diagnostic at
the callee identifier span and retain a `Missing` reference fact. An ambiguous call SHALL rely on the
existing `SEM0003` duplicate-declaration diagnostics and MUST NOT add a second ambiguity diagnostic
at the call site. Missing or damaged callee syntax SHALL not duplicate parser diagnostics.

#### Scenario: Diagnose an unknown function
- **WHEN** `main` returns `missing()` and no declaration is named `missing`
- **THEN** the call reference is missing and one `SEM0004` diagnostic identifies the exact `missing` span

#### Scenario: Avoid duplicate ambiguity diagnostics
- **WHEN** a call targets a name already diagnosed as duplicated
- **THEN** the call remains ambiguous and the semantic collection contains the declaration-owned `SEM0003` diagnostics without an additional call-site ambiguity diagnostic

#### Scenario: Preserve parser ownership for a missing callee
- **WHEN** parser recovery inserts the call's identifier
- **THEN** the call reference is unavailable and no `SEM0004` diagnostic is emitted
