## ADDED Requirements

### Requirement: Catalog namespaces are explicit-import completion candidates

In expression and actor-name contexts, completion SHALL include applicable preferred namespace
spellings from the deterministic distribution catalog even when no same-named source declaration
exists and even when the spelling at the cursor is partial. Each such candidate SHALL identify its
canonical module and SHALL remain an explicit-import option when the complete non-type spelling is
present but unavailable as a namespace binding. Completion MUST NOT turn catalog metadata into
semantic scope. In declared-type and type-argument contexts, the closed language `Effect` type
candidate SHALL remain import-free and the same spelling MUST NOT cause a namespace import edit.

#### Scenario: Complete a partial Effect namespace

- **WHEN** completion is requested after `Eff` in an expression or actor-name context without an `Effect` namespace binding
- **THEN** the results include an `Effect` candidate identified as coming from `silk/effect` with an explicit namespace-import edit

#### Scenario: Complete the full unavailable namespace spelling

- **WHEN** completion is requested on a complete non-type `Effect` spelling without the namespace import
- **THEN** the explicit-import `Effect` candidate remains available

#### Scenario: Keep Effect type completion import-free

- **WHEN** completion is requested for `Effect` in a declared-type or type-argument context
- **THEN** the closed language type candidate is offered without an import edit and no namespace-import candidate is added solely for that type use

#### Scenario: Complete imported Effect members

- **WHEN** `silk/effect` is imported under namespace `Effect` and completion is requested after `Effect.`
- **THEN** completion exposes the module's accessible public source operations through the ordinary namespace binding

#### Scenario: Repeat partial namespace completion

- **WHEN** identical source and catalog snapshots request completion for the same partial namespace spelling repeatedly
- **THEN** candidate labels, module identity, ordering, insertion text, and import edits are identical
