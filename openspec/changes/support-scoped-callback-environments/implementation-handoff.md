# Scoped callback implementation handoff

## Outcome and scope

The approved finite-intersection contract is implemented on `julia/scoped-resource-callback`, based
on the preserved local checkpoint `d1d707e629a25316f0189cd65e5481114908d8aa`. The tiny resource,
borrowed config, captured once-callback, and symbolic requirement-row program analyzes and lowers
to valid MIR. Direct forwarding still works, resource-borrow escape is rejected, and ambient
`ByteDuplex` remains excluded. No runtime bracket mechanism or TLS implementation was changed.

The actual preserved JUL-188 caller is **still blocked**. Its bracket `SEM0089` is gone, but the
namespace witness retains `SEM0074` and `SEM0122` at the outer `withClient` application. It does
not pass analysis, so successful lowering of that actual caller is not claimed.

## Why the change is sound

The old callback promised its returned Effect was valid for the whole fresh resource loan. A
captured environment can be shorter. Both source helper and sealed intrinsic now return
`Effect<'scope & 'env; ...>` from each callback. Anonymous effect callables derive their returned
environment from established capture validity (including borrowed owners and unknown generic
contents) and retained input lifetimes. Generic content predicates remain separately checked.

Intersections are finite, flattened, sorted and deduplicated, with static identity. Their proof
rules never reverse an arbitrary lower-bound edge or promote a meet to a constituent. Substitution,
free-lifetime traversal, rigid-placeholder escape detection, semantic encoding, and finite loan
requirements recurse into constituents. Contextual comparison commits only free constituent
obligations; rigid invocation regions stay within their comparison universe. Runtime lifetime
erasure and the existing resource-finalizer lowering are unchanged.

Failed environment inference now reports the missing outlives relation before the fallback row
diagnostic. Actual ambiguous/non-finite row failures retain the existing diagnostic family.

## Separate remaining failure

`evidence/remaining-callback-row.silk` reproduces the remaining two diagnostics without TLS,
certificates, an implemented transport, or any resource bracket. It accepts a generic named callback
through a higher-ranked callback parameter and requires `R in Without<R, ByteDuplex>`.

For module `row-probe`, analysis reports:

- `SEM0074` at offsets 628–677, the `accept<i32, never>(move provider, authenticated)` application.
- `SEM0122` at offsets 662–676, the supplied `authenticated` callable.

Temporary instrumentation (removed) at `CallResolution.solveCallableConstraints` found the exact
failed proof: `R` remained the callee-owned row parameter, so structural checking received
`R ⊆ Without<R, ByteDuplex>`. The substitution contained the selected local environment, `A = i32`,
`E = never`, and `P = connect.P`; **it contained no substitution for R**. The offered named callback
has an `&mut Audit` row, but specialization had not propagated it before the exclusion check.

This establishes a separate callback-specialization/evidence blocker. It does not establish the
correct repair or authorize weakening the exclusion. Follow-up work should diagnose named generic
callback specialization and row propagation using this small source. The approved intersection
change does not alter those rules.

## Verification

Passed locally:

- Compiler TypeScript build and test typecheck.
- 25 focused checks across Type, TypeGenerics, ModuleSurface, AnonymousCaptureStabilization,
  SyntaxFormatter, Suspendability, and IntrinsicCatalog (seven files).
- The existing OwnedAllocationAcceptance finalized-export check: analysis plus MIR verification of
  success, typed failure, nested cleanup, cancellation, and a new captured-owner cancellation path.
  The new path checks captured-owner destruction before resource release and resource destruction.
- Positive captured generic content with `T: 'env`; removing the bound and promoting the returned
  environment are rejected. Invocation-placeholder escape and exclusive-payload invariance remain
  rejected. Semantic surface round trips, alpha renaming within the same binder identity, and
  runtime lifetime erasure are covered.

The shared native acceptance and existing Wasm finalized-destroy witnesses use the extended
cancellation source. Their execution belongs in CI; local MIR verification is not runtime evidence.
Broad `pnpm check` and `pnpm release:candidate` have not been run locally, per the task constraint.
Changed-file Oxfmt and Oxlint passed. The affected Effect page was generated through the canonical
DocumentationProject/DocumentationReference model for all six profiles; its policy check passed.
Full documentation generation was attempted and stopped before writing because unchanged
`byte_duplex.silk` has three SummaryShape violations (ByteDuplex, readSome, writeSome, at lines 82,
115, 138). This predates the scoped change. Exact-revision CI remains outstanding.

## Preserved integration state

The exact five uncommitted JUL-188 files were copied to
`/private/tmp/silk-scoped-callback-integration`, then the focused compiler patch was applied there.
The original worktree was not changed. The namespace witness ran once after resolving checkout
package links; two earlier attempts failed at module loading before any test executed.

The original worktree still has exactly the five expected modified files. The two experimental
stashes remain `a8c0572105521ac61a7767d15a86c332c92bfc20` and
`9a2144084596c6c02967100bb79d6bbcb39a62ae`.

The paused implementer should integrate the focused commits into another checkout with the
preserved five-file correction set. The lexical constraint-forwarding hunk from commit `81eb4667`
already exists in that correction set; retain it once. Regenerate the stdlib embedding after
combining the TLS source corrections with the new Effect helper signature. Do not overwrite the
preserved generated file with this branch's embedding, which intentionally omits those TLS edits.
Do not mark JUL-188 ready or merge it while the separate row/evidence failure remains.
