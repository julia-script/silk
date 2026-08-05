## MODIFIED Requirements

### Requirement: First function declaration fact

Semantic analysis SHALL retain the parse result and publish one ordered function fact for every
direct function declaration in the source-file tree. Header collection SHALL be owned by the
declaration-index phase, and semantic analysis SHALL consume its collected headers rather than
re-deriving them. Each function fact SHALL expose its declaration, returned-expression fact, and
return compatibility together. Every declaration SHALL have a deterministic source-local identity
whose ordinal matches concrete declaration order, its canonical identity state from the
declaration index, public visibility, exact concrete parameter count, declared-name state,
return-type-reference state, and exact syntax provenance. Name lookup SHALL distinguish exactly
one match, no match, and multiple matches without discarding any collected declaration.

#### Scenario: Collect the accepted declaration

- **WHEN** the accepted fixture `pub fn main() -> I32 { return 42 }` is analyzed
- **THEN** one public function fact named `main` is available at ordinal zero with zero parameters, a canonical identity naming its module and `main`, and provenance to its original function and name syntax

#### Scenario: Count and collect typed parameters

- **WHEN** a function has two complete typed parameters
- **THEN** its declaration fact reports parameter count two and publishes two ordered parameter declaration facts

#### Scenario: Collect two declarations in order

- **WHEN** parsed `answer` and `main` functions appear in that source order
- **THEN** two function facts are published with ordinals zero and one and lookup resolves each present unique name

#### Scenario: Preserve a missing declaration name

- **WHEN** a parsed function contains a missing identifier before its parameter list
- **THEN** its function fact remains available with an unavailable name and an unidentified canonical state, no name lookup entry is invented, and no semantic diagnostic duplicates the parser's missing-token diagnostic

#### Scenario: Keep duplicate declarations explicit

- **WHEN** two declarations have the same present name
- **THEN** both function facts remain in source order, the later declaration's canonical state is a caused duplicate of the first, lookup reports multiple matches, and one `SEM0003` diagnostic identifies the later duplicate name
