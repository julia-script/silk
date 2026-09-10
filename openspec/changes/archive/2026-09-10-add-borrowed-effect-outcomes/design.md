## Context

See proposal.md. JUL-116 and JUL-117 provide semantic lifetime arguments, explicit Effect environment syntax, finite type comparisons, source ownership, sparse move paths and conditional cleanup. LifetimeAdmission currently gates Effect outcomes, StatementAnalysis rejects lexical failure payloads, and SuspensionOwnership discards partial states at relay points. These gates must remain until their replacement proofs work together. The indexed graph predates this checkout; discovered actors are verified against current files.

## Goals / Non-Goals

**Goals:** preserve three independent dimensions: environment validity, complete outcome validity and execution access; prove stable referents and exact remainder cleanup throughout suspended execution; retain existing query and runtime identity boundaries.

**Non-Goals:** new Stream or Box APIs, lending item families, arbitrary nested higher ranks, pinning, structured concurrency borrowing its caller, execution classification or representation redesign, trap unwinding.

## Decisions

### Public contracts and stable sources

Keep the already implemented `Effect<'env; A ! E ? R>` and `effect<'env> fn` spellings. Environment validity is an upper bound on retaining borrowed captures, not a promise that the environment lives until that region ends. Outcomes retain their own full semantic types. For example `effect fn borrow<'data>(value: &'data i32) -> &'data i32 { return value }` is admitted; returning `&local` from run-local storage or a consumed owned capture is rejected. A consumed aggregate may forward external references stored within it, but cannot return references into its own inline storage. Static references preserve their established exception without a parallel borrowed-outcome implementation.

Map outcome lifetimes through already selected parameter and environment types into ordinary local loan dependencies. A receiver `&'call mut Holder<'data>` keeps its temporary access for execution while `&'data A` retains only the external source it represents. Success, failure, propagation, catch/re-fail and cleanup use the same transfer discipline. Reusable environments retain captures between runs; consuming environments transfer or clean each capture once. Provider provision removes a service row member but retains its capture loan and access requirement. A channels-only coercion cannot erase any of these facts. Do not introduce public result-source expressions or inspect specialized bodies to reconstruct erased contracts.

### Quantified compatibility

Compare an offered callable against one known expected outer binder using fresh rigid placeholders and existing finite outlives/variance facts. Operation invocation lifetimes are fresh at every call. Captured enclosing lifetimes remain distinct. Reject stronger required validity, stronger access and escaping placeholders. Do not use caller lifetimes for dispatch, select another implementation on a failed obligation, infer arbitrary ranks or admit nested universal callable types.

### Suspension ownership and stable placement

Extend SuspensionOwnership slots and releases with the existing sparse initialization snapshots and flag locals. Preserve state at the exact suspension boundary, including Missing and Maybe paths, instead of copying a whole-value cleanup recipe. Frame layout, save, restore and cancellation must carry flags and live storage together; moved children remain solely owned by their destinations. A missing field cannot be read after resume until ordinary reinitialization commits. Cancellation uses the same shared CleanupPlan recipes restricted by the stored state, preserves referents through their last cleanup use and follows established release order. Cleanup/installation and ownership-state update form a non-suspending commit.

Existing stable internal loan placement and root identities remain authoritative; lexical validity alone is insufficient to relocate a borrowed owner. Verify referents across frame growth, resume and cancellation before removing the partial-state gate. Do not replace ProvisionalMir classification, optional-provider policy or the general representation strategy.

### Independent execution and container witnesses

Keep ExecutableProperty's exact environment proof separate from outcomes, affinity and NonParking. Independent packages reject external caller/provider loans even when provision empties service rows. Completion rejects references into package-owned storage; boundaries requesting detached data inspect nested lifetime arguments even in empty constrained variants. Allocation and Copy do not manufacture detachment.

Use a local ordinary-source fixed-item interface with `take<'call>(&'call mut Self) -> Option<Item> ! E ? R`, instantiated with an external `&'data A`. Verify two results remain usable after temporary Effects and wrapper destruction, but source invalidation fails. Include Copy items containing references, affine moved dependent items, rejected self-owned scratch and existing Box.make success/failure ownership. The compiler never identifies Stream by spelling; JUL-21 owns broader library design.

### Canonical summaries and evidence

Retain canonical declaration-relative lifetime/environment/cleanup facts in semantic consumer fingerprints. Alpha-renames and private edits preserving the consumed surface reuse downstream checks; changed bounds invalidate actual consumers. Runtime layout and instance keys erase lifetime arguments. Reuse generic bodies; residual work remains separately attributed. Solvers consume selected semantic facts without resolution callbacks, hidden union/state products or speculative static work.

Extend opt-in workloads independently across Effect composition depth, binder width, provider forwarding, module fan-out and partial suspended fields, with accepted and failing sources. Record actual query/generic/residual, constraints, initialization/cleanup and resolution-initiator work. Compare development and optimized verdicts structurally; backend timing is not lifetime evidence.

## Risks / Trade-offs

- Outcome dependency loss at run/catch boundaries → pair valid forwarding with owner-destruction counterexamples, including nested/generic failures.
- Premature receiver release or retained unrelated sources → prove successive borrowing calls plus stored reusable exclusive capture rejection.
- Partial-frame cleanup reads moved or uninitialized memory → inspect MIR state and flags and use only necessary shared-native cancellation traces.
- Lifetime-valid pointers target moved frame storage → verify actual stable root placement, not just semantic outlives facts.
- Binder context leaks or overbroad cache reuse → test alpha-equivalence alongside strengthened-contract and changed-bound invalidation failures.

## Migration Plan

Land one coherent change after all admission, escape, stability and cleanup proofs pass. Remove superseded admission code and reconcile affected main specifications, reference, diagnostics and fixtures under the green-field policy. Iterate with focused semantic and MIR tests; reserve full workspace tests for major integration/final milestones. At handoff run typecheck, format:check, lint, test and check, plus release:candidate when package contents or exports change. Do not claim handoff completion if any required check is missing.
