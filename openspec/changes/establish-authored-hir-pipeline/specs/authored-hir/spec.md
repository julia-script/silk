## Purpose

Define source-independent authored compiler artifacts with stable owner identities and exact content,
and the phase boundaries that let semantics and preparation consume them without source interpretation.

## ADDED Requirements

### Requirement: Authored HIR is complete and self-contained

Authored HIR SHALL represent every current semantic syntax category: imports, properties, conditional
groups, static/ordinary/foreign callables, aggregates, aliases, interfaces/services/roles/impls,
generic/lifetime/row contracts, expressions, patterns, statements and recovery. Containers and
punctuation MAY be absorbed into ordered semantic fields. Published artifacts MUST be immutable and
MUST NOT retain syntax, tokens, source objects, source slices or semantic declaration objects.

#### Scenario: Interpret after releasing syntax

- **WHEN** syntax/source storage is discarded after artifact construction
- **THEN** names, contracts, evaluation order and exact authored literal values remain interpretable

### Requirement: Authored owners have logical structured identity

Owners SHALL use logical namespace/module, enclosing owner, kind/name and necessary local
disambiguation, excluding physical paths, profiles, source offsets and content hashes. Unique named
owners SHALL remain stable across unrelated differently named insertions/reordering, movement,
header/body edits and pool renumbering. Rename/reparent SHALL change identity. Conditional branches
SHALL be distinct and selection-independent. Ambiguous same-key/anonymous siblings MAY change local
identities conservatively without renumbering unrelated unique owners. Namespace duplicate rejection
SHALL remain independent of owner identity. Full identity MUST remain available despite digest equality.

#### Scenario: Insert and edit unrelated owners

- **WHEN** `identity` is moved below a new differently named function and its body is edited
- **THEN** its authored owner identity is unchanged

#### Scenario: Conditional and duplicate owners

- **WHEN** both conditional arms declare the same name or same-key siblings repeat
- **THEN** each has a distinct authored owner, independent of profile selection
- **AND** ordinary selected same-scope cross-kind duplicate rules are preserved

### Requirement: Pools retain exact authored payloads

Module-owned immutable pools SHALL distinguish text and bytes, retain explicit lengths and embedded
zero bytes, and publish no mutable deduplication maps. Integers SHALL preserve arbitrary magnitude;
floats SHALL preserve exact pre-rounding decimal value and signed zero; text/bytes, character scalar,
duration value/unit and literal kinds SHALL remain distinct. Contextual range checks belong to demanded
semantics, including duration u64 overflow.

#### Scenario: Preserve exact literals

- **WHEN** an artifact contains embedded NUL, an integer above 2^53, and an unrounded decimal float
- **THEN** reading the artifact returns the exact payloads without host-number rounding

### Requirement: Presentation and recovery are separate from semantic content

Presentation SHALL own revision-specific spans, raw spelling/trivia/docs and diagnostic rendering.
Nodes/binders/causes SHALL have owner-local references; synthetic nodes SHALL identify authored origin
by local role/path/occurrence. Missing and invalid records MUST retain causes and healthy surrounding
structure. Damaged owners MUST NOT be eligible for successful semantic reuse or selected executable
publication. Recovery cause encoding MUST exclude absolute spans and global diagnostic ordinals.

#### Scenario: Repair damaged source

- **WHEN** a missing operand is repaired while an adjacent declaration is unchanged
- **THEN** the damaged owner's content changes and the healthy owner's content stays equal

### Requirement: Header and body encodings are canonical authored content

Encoding SHALL be versioned, domain-separated, length-framed and deterministic with fixed tags/fields,
absence markers, exact payloads and relevant sequence order. It SHALL encode reachable pool contents,
excluding pool indices, unrelated entries, object/map order, pointers, session IDs and presentation.
SHA-256 SHALL be the initial digest. Content equality MUST NOT alone prove semantic-result reuse.

#### Scenario: Pool and presentation edits

- **WHEN** an unrelated pool entry is inserted, references renumbered and source spans/docs changed
- **THEN** an unchanged declaration's header and body encodings remain byte-identical

#### Scenario: Body-only edit

- **WHEN** only a function's body changes
- **THEN** its owner and header bytes remain unchanged, its body bytes change, and header/body domains differ

### Requirement: Integration preserves one semantic pipeline and existing reuse

The coordinated milestone SHALL lower all loaded arms locally, analyze authored HIR into one typed TIR
plus indexed semantic results, and delete superseded syntax-backed semantic and duplicate body paths.
Formatting SHALL remain syntax-only. Existing dependency-sensitive editor reuse, including canonical
signature equality under alpha-renaming, SHALL remain. Prepared bundles SHALL distinguish analysis
and executable intent, seal selected source demand, and exclude later resolver/parser/semantic source
access. A1's foundation MUST NOT independently land as an unused competing pipeline.

#### Scenario: Seal an executable with demanded storage

- **WHEN** preparation discovers required runtime storage and reaches its selected-source fixed point
- **THEN** downstream compilation succeeds without reopening source discovery, while unused catalogs remain unloaded

#### Scenario: Analyze without executable work

- **WHEN** a request asks only for analysis
- **THEN** preparation performs no instance/MIR/backend work and later execution requires a new preparation request
