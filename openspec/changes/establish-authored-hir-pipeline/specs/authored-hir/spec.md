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
publication: a damaged owner's semantic result is a completed rejection, and neither building nor
reusing it may admit it as successfully checked. Such a rejection MAY be reused as a rejection only
while the owner's canonical authored content, including its recovery causes, and its semantic
dependencies remain valid; repairing the damage changes that content and SHALL invalidate it, and
healthy neighbouring owners SHALL remain independently reusable. Recovery cause encoding MUST
exclude absolute spans and global diagnostic ordinals.

#### Scenario: Repair damaged source

- **WHEN** a missing operand is repaired while an adjacent declaration is unchanged
- **THEN** the damaged owner's content changes and the healthy owner's content stays equal

#### Scenario: A presentation-only edit keeps a damaged owner's content

- **WHEN** whitespace or a comment changes around a damaged declaration without changing its recovery structure
- **THEN** its canonical authored content stays equal although its source bytes differ, so its rejection may be reused and is published at the new positions

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

### Requirement: Semantic reuse keys derive from authored content

Every loaded module SHALL be lowered to its authored module and presentation once per parse and
carried beside its syntax through the closure and elaboration inputs. Body-query reuse SHALL key
implementations by the canonical authored body, alpha-normalizing header lifetime binders to their
declaration ordinal, and SHALL key scope sensitivity by the authored names spelled in the body; it
MUST NOT tokenize or slice source to build a key. Hidden anonymous bodies SHALL take their
snapshot-local declaration identity from their enclosing declaration and callable site, never from a
byte offset. The typed executable representation is named TIR; authored HIR is the untyped input.

#### Scenario: Alpha-rename a lifetime

- **WHEN** a library renames `'a` to `'long` in a generic function without changing its meaning
- **THEN** its body query and every consumer body are reused without executing a checker, and consumers observe the new spelling through current declaration facts

#### Scenario: Insert an unrelated declaration

- **WHEN** a declaration is inserted above a function that owns an anonymous callable
- **THEN** the hidden body's identity moves with its enclosing declaration ordinal and the cached body is rebound without re-checking

### Requirement: Preparation seals intent-specific bundles

One preparation request SHALL name its intent. Analysis intent SHALL seal the required roots, normalized configuration and selected frontend closure. Executable intent SHALL additionally normalize the target profile, close runtime component demand to a fixed point and record the final realization product. Every bundle SHALL publish a manifest with its intent, root, identity, profile identity, target, loaded modules with origins, presentation revisions and resolved imports, component roots with reasons, source-closed or partial status, and diagnostic count. Executable consumers SHALL accept only executable bundles, and the operations downstream of a bundle SHALL carry no source-resolver requirement.

#### Scenario: Reject analysis-only input

- **WHEN** a Driver or realization consumer receives an analysis bundle
- **THEN** the operation rejects the analysis bundle before any executable work runs on analysis-only input

#### Scenario: Record component reasons

- **WHEN** an executable bundle admitted a composition root and a demanded storage provider
- **THEN** its manifest lists both with reasons `composition` and `execution-storage`

### Requirement: Semantic analysis consumes authored HIR with explicit context

Declaration collection and completion, name and import resolution, typing, conformance, static
evaluation and ownership SHALL consume authored HIR together with one explicit semantic context per
module: the authored module, pool-resolved text and bytes, and the current presentation for spans and
document order. No semantic operation SHALL take a source file, syntax file, syntax node or token,
slice source bytes, or compare syntax objects for identity. Facts SHALL carry authored anchors where
they carried syntax nodes or tokens, and identities that keyed on spans SHALL key on anchors. The
formatter, the syntax inspector and local lowering remain the only syntax consumers.

#### Scenario: Analyze after syntax release

- **WHEN** a module's syntax and source objects are released after lowering
- **THEN** declaration collection, resolution, typing, static evaluation and ownership complete from the authored module and its context with identical facts and diagnostic codes

#### Scenario: Literal facts read authored payloads

- **WHEN** a body contains `18446744073709551616`, `1.5e300` and `2h`
- **THEN** the integer, float and duration facts derive from the authored exact payloads and contextual range diagnostics keep their codes and spans

### Requirement: One typed TIR body publishes indexed results

Elaboration SHALL publish one typed executable body per checked function together with the indexed
results other phases need: types and conversions, resolved members and operations, conformance
witnesses, scopes, bindings and occurrences, lifetime, ownership and cleanup evidence, static
dependencies, diagnostics and unavailable causes. It MUST NOT retain a second executable fact tree
whose only purpose is conversion into TIR, and body reuse MUST NOT rebind cached facts to predecessor
syntax: a reused body resolves its anchors through the current presentation. Elaboration SHALL emit
typed nodes and indexed rows as each authored construct is checked; it MAY retain ephemeral scalar
decisions while checking that construct, but MUST NOT assemble a recursive working body for later
conversion. Reuse stores the checked body and its tables only, and the canonical encoding of a
checked body holds no source position and no header object. Positive and negative
reuse witnesses (private body edit, alpha rename, new caller, static-helper dependency, exported-bound
change, missing-member repair, SCC merge and split, origin isolation) SHALL keep their outcomes.

#### Scenario: Reuse across a trivia edit without rebinding

- **WHEN** whitespace above a function changes and its body is reused
- **THEN** the reused body's diagnostics and navigation report the new spans through the current presentation and no checker executes

#### Scenario: Hidden bodies keep structural identity

- **WHEN** a declaration owning an anonymous callable moves below an inserted declaration
- **THEN** the hidden body identity follows its enclosing declaration ordinal and callable site, and the reused body names the current declaration ids without consulting predecessor syntax or working records

#### Scenario: A checked body is portable

- **WHEN** a checked body is written through the canonical codec and read back for a revision
- **THEN** it equals the body that revision builds, and the same authored content at different source positions encodes to the same bytes

### Requirement: Diagnostics and navigation resolve through current presentation

Every semantic diagnostic and every navigation, hover, completion and inspector location SHALL be
resolved from an authored anchor through the current presentation of its module. Presentation spans
start at the first significant byte of the authored node. Moved declarations and hidden bodies SHALL
navigate to their current spans without consulting predecessor syntax.

#### Scenario: Navigate to a moved declaration

- **WHEN** a dependency declaration moves within its module and an importer's tooling is reused
- **THEN** definition queries from the importer resolve to the declaration's current span
