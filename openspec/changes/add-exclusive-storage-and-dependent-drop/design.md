## Context

See proposal.md. The foundation already carries semantic lifetimes, nominal variance, sparse move paths, paired replacement MIR and a cleanup-validity pass. LifetimeAdmission explicitly rejects exclusive stored values and dependent Drop. RawBuffer and Slot are opaque builtin nominal types; their ordinary-source wrappers do not establish a lifetime contract merely by existing.

## Goals / Non-Goals

**Goals:** justify each newly admitted storage boundary with semantic and MIR evidence, preserve external payload provenance independently of owner-backed storage views, and include destructor uses in access conflicts as well as final validity checks.

**Non-Goals:** public mutable loan-history summaries, method-body-derived variance, lifetime-driven conformance search, new runtime lifetime tokens, broader Drop effects, partial suspended frames, borrowed Effect outcomes, arenas or pinning.

## Decisions

### Admission and declaration facts

Remove only the exclusive-storage and dependent-Drop gates once the associated rules pass adversarial witnesses. Keep the Effect outcome gate through generic instantiation and aliases. NominalVariance continues deriving ordinary field variance; compiler-owned mutable raw storage uses invariant argument rules. Shared/exclusive view access and payload variance stay distinct. Module surfaces retain the full declaration storage and Drop contract so consumers invalidate without inspecting implementation bodies.

### Cleanup and loans

Use the existing shared CleanupPlan recipes and sparse initializedness from Ownership. Conservative hook uses retain all stored lifetimes of the complete hook receiver; recursive cleanup retains only initialized child obligations, including generic payloads. Feed these requirements into loan liveness before deciding conflicting mutation, then validate ordered exit destruction. Index syntax endpoints once per body for cleanup lookup. Attribute liveness and ordered-validity solver work separately; retain sparse state and shared cleanup recipes. Do not waive obligations based on an empty hook body. Preserve copied-child ancestry until all dependents end.

### Replacement and raw storage

Keep the destination type stable through &mut, generic replacement and swap. Evaluate incoming values first; preserve incoming and displaced dependencies during cleanup and install without suspension. Reuse initialized/missing/maybe-initialized state and paired MIR transfer; no rollback is added. The builtin Slot nominal has two semantic parameters, `'storage` and `T`, with ordinary header/local elision; RawBuffer remains invariant in T. RawBuffer/Slot contracts distinguish the storage access lifetime from lifetimes inside T. Slot writes constrain T invariantly, while take/copy results retain T's external data lifetimes. Owner-backed views retain the allocation borrow. The existing operations supply the required transfer and cleanup; no new primitive is proposed.

### Ordinary-source witness and evidence

Exercise Vector rather than adding a collection. Its unit-result insertion Effects are in scope; ordinary extraction avoids dependent Effect outcomes. Share semantic snapshots in existing test files, use MIR for cleanup/installation structure, and add native corpus behavior only for unique exactly-once oracles. Extend the opt-in lifetime benchmark with exclusive chains, dependent recursive cleanup and failing cases; report work separately from elapsed time.

## Risks / Trade-offs

- Admission holes across raw storage → verify wrappers and intrinsic operations with both positive and invalid lifetime/provenance cases before opening the gates.
- Destructor-only dependencies missed by last use → make cleanup an input to conflicting-access checks and retain ordered-exit verification.
- Opaque mutation may retain displaced loans conservatively → permitted by the accepted design; never publish body-derived mutation histories.
- Recursive summaries or partial-state products → reuse finite type summaries and sparse paths, measure actual work and retain failed witnesses.

## Migration Plan

Implement on the JUL-117 branch, update every affected caller, fixture, diagnostic and prescriptive rule, and remove superseded gates. Strictly validate OpenSpec, then run typecheck, format:check, lint, test, check and release:candidate in the repository-required order. No compatibility path is retained.
