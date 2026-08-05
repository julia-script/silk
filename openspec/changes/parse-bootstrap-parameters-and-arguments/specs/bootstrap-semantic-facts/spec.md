## MODIFIED Requirements

### Requirement: First function declaration fact
Semantic analysis SHALL retain the parse result and publish one ordered function fact for every
direct function declaration in the source-file tree. Each function fact SHALL expose its declaration,
returned-expression fact, and return compatibility together. Every declaration SHALL have a
deterministic source-local identity whose ordinal matches concrete declaration order, public
visibility, exact concrete parameter count, declared-name state, return-type-reference state, and
exact syntax provenance. Name lookup SHALL distinguish exactly one match, no match, and multiple
matches without discarding any collected declaration.

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

## ADDED Requirements

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
