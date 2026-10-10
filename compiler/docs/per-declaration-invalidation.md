# Per-declaration invalidation

**Status:** proposal. Nothing here is implemented, and implementation waits for Julia's approval.
Measured on `selfhost` `e1022f2`; line numbers refer to `31fc6e3`.

## Summary

Every semantic answer in the native compiler stays valid only while the exact bytes of every
module it transitively touched stay the same. The query keys are already per declaration, but
validity is per module. An edit anywhere in a module therefore restarts every answer that touched
that module. Two answer kinds survive the restart:

- A checked body can keep its payload, but only if its own source positions did not move.
- LLVM text is content-addressed and survives.

MIR has no such escape and is lowered again every time. In the probe below, editing one leaf body
re-lowers 4 of 5 functions, and adding a comment line re-checks every body in that file.

This note proposes four changes:

1. Key declaration facts by stable declaration identity instead of syntax position.
2. Observe per-declaration projections (header digest, body digest, name buckets) instead of whole
   module bytes.
3. Validate each answer through its direct dependencies and stop at equal results, so the early
   cutoff that only Body has today applies to every family.
4. Make positions declaration-relative.

With all four, a body edit invalidates only that body and the MIR and text of that instance. A
signature edit invalidates only the declarations that read the signature.

## 1. What keys on the module today, and why

**Keys.** `Key` (`semantic/Semantic.silk:948`) is
`{family, module, name, declaration, member, external}`. They fall into four groups:

- **Declaration families** (`declarationFamily`, :845), from Import through Body and the
  origin families. These key on `(module ordinal, declaration ordinal)`. The declaration
  ordinal is the declaration's position in the module's `ModuleIndex`, so it is a syntax position:
  inserting a declaration renumbers every later one.
- **Applied families** (MemberShape, ConformanceHeads, ProofBounds, AppliedOperation,
  AppliedInherent). These add interned applied types in `name` and `member`.
- **Per-module families:** Name (per symbol), OperatorInterfaces, Implementations and
  ProfileInput.
- **Instance families:** Mir, EmissionInputs, Layout and LlvmText key only on an interned
  instance, layout type or content index (`sameKey`, :3491). Their module and declaration fields
  are informational.

**Validity.** This is where the module granularity comes from.

- `source` (:4027) records an `Observation` (`semantic/SourceRevision.silk:41`) holding the
  module's complete bytes, or the fact that it is absent.
- `declarationAt` (:4060) and `header` read through `source`, so reading one header observes the
  whole file.
- `demandChild` (:5272) also observes the declaring module of every declaration-family child. The
  reason given in the code: "a declaration ordinal names a declaration only in one source
  revision".
- `mergeEvidence` (:3985) copies the child's observations into the parent, so observations are
  transitive.
- On every later demand, `validAnswer` (:1573) compares each observation byte for byte. A single
  mismatch forgets the answer.

The design is sound and simple: positional ordinals and absolute spans are only meaningful
against the exact bytes they came from, so the whole file is what gets observed.

**Existing cutoffs.**

- **Body.**
  - A forgotten Body answer becomes a candidate (`priorCandidate`, :3290).
  - `reuseBody` (:44276) keeps the typed payload when two checks pass.
  - First, the declaration's `BodyInput` (`semantic/BodyInput.silk:24`) is equal. That compares
    the header and body fingerprints, but also the exact syntax index and span (`equals`, :85).
  - Second, every recorded direct dependency returns the same `answerId` or an equal identity
    value (`identityOfFact`, :3088).
- **LlvmText** (`resolveLlvmText`, :52030) is named by a SHA-256 of its content, so an unchanged
  instance keeps its text.
- **Everything else has no cutoff.**
  - `identityOfFact` returns nothing for Mir, Layout and EmissionInputs, so `resolveMir`
    (:50341) re-lowers after any observed byte change, even when its Body was reused.
  - Every restarted answer also re-reads and re-observes the module, which costs time even when
    the payload is kept.

## 2. What an edit invalidates today

The probe uses two modules. `lib` has `leaf` and `other`. `main` has `a`, which calls
`Lib.leaf()`, `b`, which is independent, and `c`, which calls `Lib.other()`. Every edit
demands the Body and the MIR of all five functions, then counts query events. "Restarted" counts
Body starts, and the figure in parentheses counts how many of those reused their payload.

| Edit | Bodies restarted (payload reused) | MIR lowered | Ideal |
|---|---|---|---|
| same-width edit to `leaf`'s body | 4 (3) | 4 | 1 body, 1 MIR |
| same-width edit to `b`'s body | 3 (2) | 3 | 1, 1 |
| edit to `leaf`'s body that shifts `other` | 4 (2) | 4 | 1, 1 |
| `leaf` result type `i32` to `i64` | 4 (2) | 4 | 2 (`leaf`, `a`), 2 |
| comment line at the top of `main` | 3 (0) | 3 | 0, 0 |

- **Signatures.** The edited module's Signature queries also restart: 2 or 3 per edit. Ideally
  that happens only for the edited declaration's own Signature.
- **Shifted positions.** `other` loses reuse once `leaf` grows by one byte, and every body in
  `main` loses it after the comment. That is the span comparison in `BodyInput.equals`.
- **Lowering.** MIR is re-lowered for every restarted body, including the reused ones, because
  MIR has no identity.

## 3. Proposed design

### 3.1 Stable declaration keys

Declaration families key on an interned `DeclarationId` instead of a `ModuleIndex` position.
That is the canonical owner path and name, plus an occurrence disambiguator for collisions, which
already exists for the Collision rejection. The position becomes a per-revision lookup from
identity to syntax (`declarationAt`), and that lookup is itself an observed fact. Inserting or
reordering declarations then no longer changes any key.

### 3.2 Projection observations instead of module bytes

Replace `Observation {id, content}` with typed leaf reads, each holding a digest:

| Read | Digest | Who records it |
|---|---|---|
| module shape | imports, declaration identities and kinds | Name, Implementations, OperatorInterfaces, ProfileInput |
| declaration header | `Fingerprint.headerDigest` | Signature, Contract, Members, Identity, ... |
| declaration body | `Fingerprint.bodyDigest` | Body, Constant, Initializer, static evaluation |
| name bucket | the candidate `DeclarationId` set for one name | Name |

- `hir/Fingerprint.silk` already produces header and body digests that do not depend on positions
  or intern indices, and `BodyInput` already uses them.
- `source` stops observing anything. Each provider records exactly the projection it read.
- `demandChild` no longer observes the child's module, because the key is now a stable identity.

### 3.3 Direct-dependency validation with early cutoff

Each answer stores its own leaf reads and its direct dependencies, each dependency as a key plus
the dependency's result fingerprint. It no longer stores transitive observations, so
`mergeEvidence` goes away. Validating an answer works like this:

1. Check its leaf reads against the current revision.
2. Validate each direct dependency recursively, in recorded order.
3. Compare each dependency's current fingerprint with the recorded one.
4. Re-run the provider only if something differs. If the re-run yields an equal fingerprint, its
   own dependents stay valid. That is the cutoff.

This is `reuseBody`'s rule (`answerId` or equal identity) applied to every family, and it
replaces the Body-only candidate path. Every `Value` needs a fingerprint:

- `identityOfFact` already covers the front-end facts.
- Mir, Layout and EmissionInputs need a canonical encoding digest. LlvmText already has one.

The bootstrap's `jul-217-semantic-revision-validation` change used the same model: typed query
and leaf reads with expected fingerprints, admitted by one ordered validator that stops at equal
results. That change targets the TypeScript stack, but its rules carry over.

### 3.4 Declaration-relative positions

Checked bodies, MIR origins and gap spans store offsets relative to their declaration's start,
plus that declaration's identity. A per-revision declaration base converts them to absolute spans
only when a diagnostic, gap or debug location is reported. `BodyInput.equals` then drops the span
and syntax-index comparison, so an edit to an earlier declaration or a comment no longer
invalidates.

### 3.5 What each edit invalidates under this design

- **Body edit:** the body digest of that declaration changes.
  - Its Body re-runs, then its Mir, EmissionInputs and LlvmText.
  - Callers read only the callee's Signature, so they are untouched. This matches the
    `mir-core-shape.md` rule that body edits do not change callers' content subjects.
  - Static evaluation that executed the edited body recorded a body read, so it re-runs.
- **Signature edit:** the header digest changes.
  - Its Signature re-runs, and if the result fingerprint differs, every answer that recorded that
    Signature as a dependency re-runs: the callers' Bodies, then their MIR.
  - Bodies that do not call it keep their answers without re-running.
- **Comment, whitespace or reorder:** no digest changes, so nothing re-runs. Only the module shape
  is re-validated.

### 3.6 Compiling only what is used

Builds are already demand-driven: `buildApplication` lowers the instances reachable from the
entry, and only those. Two changes keep it that way:

- The per-module families (Implementations, OperatorInterfaces) stay per module, but they read
  only the module shape and the headers of `impl` declarations. A body edit therefore never
  disturbs them, and unused declarations are never checked.
- Instance families keep keying on the instance. With #1242 that key becomes the region-erased
  code identity (see §4).

## 4. Cost, risks and the region-sensitivity work

**Suggested order.** Each step stands alone and is measurable with the probe above.

1. **Fingerprints and cutoff for Mir, Layout and EmissionInputs.** Mir keeps its answer when its
   Body and Signature dependencies are unchanged.
   - Before: every MIR answer that touched an edited module is lowered again.
   - After: only the edited instance's MIR, and the MIR of instances whose Body or Signature
     actually changed. Small and local.
2. **Declaration-relative spans.** This removes the position-shift losses: the shifting edit and
   the comment in the probe. It is medium-sized: TypedBody spans, MIR origins, gap spans and
   diagnostic rebasing.
3. **Stable `DeclarationId` keys and projection observations.** This is the largest step. Every
   `declarationAt` and `source` caller changes, and so do `SourceRevision` and `SourceIndex`.
4. **Direct-dependency validation.** Drop transitive observations and the Body-only candidate
   path, which removes the dual reuse mechanism.

**Risks.**

- **Soundness depends on complete reads.** Any provider that reads source or another fact without
  recording it can now keep a stale answer. Today the whole-module observation hides such gaps.
  Mitigation: a verification mode that, whenever it reuses an answer, recomputes it and compares
  fingerprints. It would run in the Cases roots and in one corpus CI leg. The reads most at risk:
  - static evaluation that executes other declarations' bodies;
  - reflection (`Reflect.fields`), which reads members;
  - conformance coherence, which reads every `impl` header in scope;
  - the module shape, which decides name resolution.
- **Fingerprint cost.** Canonical MIR encoding and hashing cost time on cold builds. Answers grow
  by one digest each. Leaf reads replace whole-file observations, which is roughly neutral.
- **Diagnostics.** Rebasing must happen in exactly one place, or spans will drift. The
  diagnostic catalog tests that assert spans would catch that.
- **Two reuse paths during the transition.** Steps 1 and 2 extend today's mechanism. Step 4 must
  delete the Body-only candidate path in the same change, to respect the green-field rule.

**Interaction with 01DLG's region-sensitivity work (#1242).**

- #1242 splits instance code identity (regions erased) from region-pattern validity (a new
  `Family.Validity`).
- Instance families (Mir, EmissionInputs, LlvmText) should key on that code identity. One body
  edit then invalidates one code instance, instead of one MIR per caller-region pattern.
- `Family.Validity` fits the cutoff model. Its dependencies are the instance's Signature and Body
  fingerprints, so a body edit that keeps the lowered code but changes a region obligation re-runs
  only validity checks.
- The keys don't conflict, and the two pieces of work are independent. Landing #1242's code
  identity first shrinks the number of answers that step 1 has to fingerprint and revalidate.
