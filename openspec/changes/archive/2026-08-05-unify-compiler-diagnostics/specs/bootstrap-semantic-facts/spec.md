## MODIFIED Requirements

### Requirement: Semantic diagnostics are deterministic data
The semantic result SHALL expose semantic diagnostics as a separate readonly collection while
retaining lexical and parser diagnostics through its original parse result. Every semantic
diagnostic SHALL be a unified `Diagnostic` value whose originating phase is semantic analysis,
containing a stable code, severity, concise message, reason data, and source-owned primary span.
A semantic diagnostic produced because a fact is unavailable SHALL carry the originating
diagnostic's identity as its cause. Present duplicate names after the first occurrence SHALL
produce `SEM0003` at each later name span. Within the semantic result, diagnostics SHALL be
ordered by primary span and stable code, and semantic source mistakes SHALL return complete
ordered facts and diagnostics rather than throw or fail an Effect.

#### Scenario: Repeat multi-function semantic analysis
- **WHEN** equivalent malformed multi-function parse results are analyzed repeatedly in fresh processes
- **THEN** their declaration identities, fact order, lookup outcomes, source provenance, and semantic diagnostics are identical

#### Scenario: Keep diagnostic phases separate
- **WHEN** one source contains parser recovery, a duplicate present name, and an unknown present return-type identifier
- **THEN** lexical, parser, and semantic diagnostics remain in their owning collections, each identifying its originating phase, and semantic diagnostics are ordered by their exact primary spans

#### Scenario: Diagnose every later duplicate
- **WHEN** three declarations share the same present name
- **THEN** the second and third names each produce one `SEM0003` diagnostic while the first remains the original declaration

#### Scenario: Unavailability links to its cause
- **WHEN** a call target is unresolved and its argument-contract facts become unavailable as a result
- **THEN** any diagnostic reported on those dependent facts carries the unresolved-target diagnostic's identity as its cause, and no duplicate diagnostic restates the unresolved target
