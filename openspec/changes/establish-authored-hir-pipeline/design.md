# Design

## Context

See proposal.md. At `6b66ea9b095f4319f2670188caa0c831539cd900`, `Hir.HirFunction` owns
semantic `DeclarationFact` and `ContractFact`, and `DeclarationId` uses a source ordinal.
`SyntaxTree.NodeKind` enumerates semantic categories and structural containers. Source roots
already use resolver-canonical module strings; physical paths belong to source provenance.

## Goals / Non-Goals

**Goals:** Give lowering a closed, typed, immutable vocabulary for all authored meanings and
recovery; make exact content available without retaining source; distinguish owner identity,
authored content equality, and contextual semantic-result reuse.

**Non-Goals:** A1 does not select imports, resolve types, replace semantic IDs in current consumers,
implement persistent reuse, or introduce a package manager. The shared milestone does not add a
disk cache or promise faster compilation.

## Decisions

### Actors and publication

Use `AuthoredHir` for the untyped model while A3 owns the existing typed-HIR terminology migration.
Use separate actors for logical identity, immutable text/byte pools, presentation and canonical
encoding. Closed tagged records encode semantic fields in a fixed order. This is preferable to
copying concrete syntax tokens or accepting arbitrary property dictionaries, which would preserve
the very interpretation dependency this boundary removes. Type-only syntax coverage maps in tests
force every grammar category to have an explicit disposition.

Publication copies/freeze-owns arrays and records; temporary construction/deduplication maps do
not survive in artifacts. Text and bytes have distinct references and explicit UTF-8/byte lengths.
Exact integers use bigint; floats retain exact decimal components and signed zero before contextual
rounding. Literal kind, text/byte distinction, character scalar and duration unit remain authored
data. Missing/invalid nodes retain local causes and healthy children, never a successful empty value.

### Identity and local references

Logical namespace plus canonical module and the enclosing authored owner path identify an owner.
Each segment names kind, optional name and a same-key local occurrence. Count only siblings with
the same kind/name/role key: inserting another unique name cannot renumber existing owners.
Conditional groups and their then/else branches are authored owners independent of selected profile.
Anonymous/same-key edits may change their local occurrences. The full structure, not its digest,
remains authoritative. A kind distinction does not grant a source namespace duplicate exemption.

Nodes, binders and causes use owner-local references. Source-lowering synthetic origins name the
authored local anchor, role and occurrence. Specialized instances remain typed semantic products.
Presentation maps these anchors to current source revision, spans, raw spelling/trivia and docs;
no source object or slice is part of authored HIR.

### Canonical content

Versioned header and body domains use fixed tags, ordered fields, length-framed payloads, explicit
absence, exact numbers and sequence order. Pool references encode referenced contents, never the
module pool index or unrelated entries. Fingerprints use SHA-256. Owner identity is available
separately from content; equal bytes are not authorization to share a semantic result between owners.
Recovery cause paths encode roles/local occurrences, never diagnostic ordinals or absolute spans.

Example: moving `fn identity[T](value: T) -> T { return value }` below another uniquely named
function changes presentation but not owner/header/body bytes. Reordering its pool also changes
none of those bytes. Editing only the return changes its body bytes. Renaming `value` can change
authored bytes while preserving a canonical semantic signature; A3 must retain that reuse.

### Shared phase rules

A2: loaded syntax → authored HIR + presentation, including static bodies and both conditional arms.
It records lexical relationships but defers imported/type/member resolution and all inactive semantic
errors. Formatting remains syntax-based. Damaged source retains parser diagnostics and healthy owners.

A3: authored HIR + explicit semantic context → one typed TIR body + indexed results. Shared bounded
static evaluation serves ordinary semantics and preparation. Delete duplicate executable fact trees,
syntax-backed semantic checking and cache rebinding. Preserve diagnostics through current presentation
and preserve positive and negative ProjectAnalysis reuse witnesses; hashes alone are insufficient.
The final TIR schema, its result tables, representative artifacts, the consumer map and the
implementation steps are fixed in `tir-contract.md`.

A4: canonical roots + intent/profile/composition → prepared bundle. Normalize package configuration,
select imports using A3, and for executable intent discover demanded runtime components to a monotone
fixed point. Share module success/absence/failure for the invocation. Analysis intent performs no
instance/MIR/backend work. Sealed downstream operations cannot resolve/parse/read semantic source.
Promotion or profile change creates a new request. Partial discovery permits only explicit editor
queries. Helper compilation is a separate request, never reopening its parent. Unused catalogs remain
unloaded. Selected damaged content cannot publish executable success.

## Risks / Trade-offs

- Schema omissions → audit all NodeKind categories, compile-time coverage table and structural fixtures.
- Accidental source retention or pool-number hashes → source-free fixtures and byte equality goldens.
- Ambiguous sibling matching → conservative documented local identities instead of speculative matching.
- Two representations at integration → A1 draft is not independently mergeable; A2–A4 replace consumers
  and delete the old path before the coordinated landing.
- Deep malformed graphs → publication validates references/structure; recovery stays explicit.

## Migration Plan

Deliver A1 schema/encoding in this draft. Stack A2 lowering, A3 semantic/TIR replacement, and A4 sealing
on the same contract, extending relevant existing specification deltas before each implementation.
Only the complete integration lands. There are no compatibility adapters or dual production paths.
