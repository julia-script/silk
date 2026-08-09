## Context

See `proposal.md` for motivation. `SyntaxFile` currently gives every element a deterministic
`sourceId + preorder ordinal` identity, while `ProjectAnalysis` constructs one shared immutable
frontend per accepted set of roots. `ProjectSession` atomically commits that frontend's document
views, but its analysis callback has no prior-commit input and `ModuleClosure` reparses every
resolved module.

The phase reports from the preceding changes establish the intended boundary: lexing/parsing lives
inside closure construction, while all semantic phases consume immutable closure facts. Reusing a
whole unchanged `SyntaxFile` at that seam is safe and measurable. Reusing semantic facts or adding a
persistent parser is not yet justified.

## Goals / Non-Goals

**Goals:**

- Skip lexing and parsing for byte- and origin-identical modules between accepted project revisions.
- Give later caches an exact, deterministic way to relate unchanged concrete subtrees in reparsed
  modules.
- Preserve each snapshot's existing canonical identities and the LSP's atomic latest-wins contract.
- Make reuse visible as immutable project data with focused operation-count and identity tests.

**Non-Goals:**

- Incremental tokenization or parsing inside an edited module.
- Green/red tree storage, globally persistent node IDs, or mutation of prior artifacts.
- Declaration, resolution, HIR, ownership, diagnostic, or tooling-index reuse.
- Public-header versus implementation invalidation or dependency-aware semantic recomputation.

## Decisions

### D1: Add correspondence beside canonical `SyntaxId`

`SyntaxId` remains the reproducible identity of one element inside one completed artifact.
`SyntaxCorrespondence` is a separate immutable relation containing previous/current artifact
references, bidirectional element maps, ordered canonical-ID pairs, and counts. This represents the
two concepts honestly: deterministic identity within a snapshot and stable correspondence across
adjacent snapshots.

Replacing ordinals with process-local IDs was rejected because it would sacrifice fresh-process
determinism. Embedding revision history into every syntax node was rejected because batch compiler
artifacts should remain history-independent.

### D2: Match exact unique siblings conservatively

The correspondence builder computes a compact structural fingerprint for every concrete element.
Fingerprints include element family, kind or expected token, exact token bytes, and recursively
ordered children. Fingerprints are only candidate indexes; an exact recursive comparison confirms
every accepted pair, so hash collisions cannot create false correspondence.

If two roots are exactly equal, their complete trees correspond. Otherwise, the builder compares
their direct children and accepts a structural fingerprint only when it occurs exactly once among
the previous siblings and once among the current siblings. Each accepted exact subtree is then
paired recursively. This handles inserted, removed, shifted, or reordered distinct declarations
while refusing ambiguous identical siblings.

Sequence LCS matching was rejected because repeated equal declarations make tie-breaking
deterministic but not truthful. Global subtree matching was rejected because an equal expression
moved between different declarations lacks enough context to establish logical continuity.

### D3: Reuse only whole syntax artifacts during closure loading

Revision-aware closure loading receives the prior `ProjectClosure`. After resolving current source,
it constructs the current immutable `SourceFile` and compares identity, origin, and bytes with the
same canonical module in the prior closure. Equality reuses the prior `SyntaxFile`; inequality runs
the ordinary lexer/parser. Import facts are derived again from the selected syntax artifact so the
current closure remains independently complete.

A process-wide content cache was rejected because ownership, eviction, and cross-project isolation
would become implicit. Partial token/tree reuse was rejected because it is the persistent parser
work this evidence-gated stage intentionally postpones.

### D4: `ProjectAnalysis.revise` owns revision observations

`ProjectAnalysis.make` remains the history-free entry point. `ProjectAnalysis.revise(previous,
roots)` invokes the same pipeline with the prior closure as a syntax-reuse source. The resulting
project records one `Fresh`, `Reused`, or `Changed` observation per current module. `Changed`
contains a `SyntaxCorrespondence`; `Reused` is guaranteed by exact `SyntaxFile` reference identity.

The semantic pipeline still runs once over the entire current closure and creates new semantic fact
tables. This prevents correspondence from silently becoming an unsound semantic cache.

### D5: Pass the committed map into the analysis callback

`ProjectSession` calls `analyze(currentDocuments, committed)` and replaces `committed` only after
the current revision is still latest. Workspace results retain their shared `ProjectAnalysis`, so
the workspace can select that actor from the prior committed map and call `revise`. A stale job may
compute a project but cannot commit it and therefore cannot seed later analysis.

Keeping a hidden workspace-global previous project was rejected because it would escape session
lifecycle and make independent-project behavior harder to test.

## Risks / Trade-offs

- **Fingerprint construction adds work to changed modules** → Build it only when a same-identity
  predecessor exists, cache fingerprints within one correspondence, and keep semantic phases
  unchanged so focused tests can measure the boundary independently.
- **Conservative ambiguity lowers match rate** → Prefer false negatives over false positives;
  future persistent parsing can provide stronger identity evidence without changing this contract.
- **Whole-project semantic recomputation limits the immediate speedup** → This stage targets parse
  reuse and establishes the safe mapping needed for later semantic invalidation.
- **Retaining prior/current artifacts increases one-revision memory pressure** → Correspondence is
  owned only by the newly committed project; the scheduler does not retain superseded projects.

## Migration Plan

Add the new compiler actor and revision entry point, then migrate the LSP callback and workspace
result shape together. Existing history-free `ProjectAnalysis.make`, single-source analysis, and
batch compilation remain available. The project is pre-release, so no compatibility shim is added
for the internal LSP callback signature.
