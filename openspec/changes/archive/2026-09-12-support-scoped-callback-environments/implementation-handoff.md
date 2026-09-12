# Scoped callback implementation handoff

## Integrated outcome

PR [#427](https://github.com/julia-script/silk/pull/427) was merged into JUL-188's
PR [#424](https://github.com/julia-script/silk/pull/424) at
`234cba3870ca39a88cfbc0bc6fa6422d3cce73af`. The user then authorized completing JUL-188 in this
checkout. Verified JUL-187 merge `6ab7959ad15841f390f326b8bdc1338a5f732624` is an ancestor.
There is no remaining integration step for the paused implementer.

The original finite-intersection change removed the resource-bracket lifetime failure. Subsequent
JUL-188 fixes resolved the independent named-callback row/provider failure and the lowering/runtime
failures exposed after it. The actual TLS namespace witness accepts the intended callback and
rejects ambient ByteDuplex access. The authenticated LLVM-to-Wasm connection witness passes at
`a843c5d3f7e8f09d2fa9e4245d52f303f451cb8e` (232.61 s), including plaintext I/O, exact outbound bytes,
directional shutdown, and terminal close. Full native/CI conclusions are tracked in the
[JUL-188 handoff](../2026-09-12-implement-scoped-byte-duplex-tls-connection/implementation-handoff.md) and PR.

## Lifetime and callback contract

A resource borrow and a captured environment can have different validities. Source helpers and the
sealed intrinsic return `Effect<'scope & 'env; ...>` from resource callbacks. Intersections remain
finite, flattened, sorted, and deduplicated, with static identity. Their proof rules do not reverse
an arbitrary lower-bound edge or promote a meet to a constituent. Substitution, free-lifetime
traversal, rigid-placeholder escape detection, semantic encoding, and finite loan requirements
visit constituents. Runtime lifetime erasure is unchanged.

Named callback specialization retains invocation binders and their established validity relations
while inferring value/service-row arguments. Concrete provider obligations are proven before a
fully selected callable loses schema metadata. Open or assumed evidence remains subject to escape
checks. The current TLS source and examples explicitly use one callback invocation lifetime;
this handoff does not claim every historical unannotated callback spelling is accepted unchanged.
Ambient transport access remains excluded at every service access level.

The historical `evidence/remaining-callback-row.silk` records the isolated failure found at the end
of the original prerequisite investigation. The active handoff is the integrated implementation,
not the earlier statement that JUL-188 remains blocked on SEM0074/SEM0122.

## Subsequent general compiler corrections

- Provider operation resolution and witness construction share the associated-owner selection.
- Captured callback identities and outer lifetime arguments survive acquisition-runner discovery.
- Returned-view loans join only continuing paths; disjoint field/index access is distinguished from
  whole-owner access. The existing control-flow proof still checks exact loan endpoints.
- Whole-value union cleanup retains the complete tagged carrier; actual field bindings project
  their fields. Selected stored-reference reborrows load the referent address.
- Exact context-atom interning bounds instance-discovery keys without changing published identities.

Small analysis, MIR, and layout regressions cover these defects. Shared native byte-duplex and
finalized-destroy cases cover borrowed-provider addressing and release ordering. The integrated
TLS source also observes authentication already published during output acknowledgment.

## Preserved user state

The original worktree `/Users/juliaortiz/.codex/silk-manager/worktrees/jul-188` remains at
`d1d707e629a25316f0189cd65e5481114908d8aa`, with the same five modified files:
`ExpressionAnalysis.ts`, `Stdlib.generated.ts`, `tls_connection.silk`,
`StdlibNamespaceAcceptance.test.ts`, and `support/tlsConnectionAcceptance.ts`.
The two experimental stashes remain `a8c0572105521ac61a7767d15a86c332c92bfc20` and
`9a2144084596c6c02967100bb79d6bbcb39a62ae`. Completion work uses the separate JUL-188 checkout.

## Verification and delivery

The original 25 focused compiler checks, finalized-export MIR check, capture/escape controls,
semantic round trips, and generated Effect documentation passed. Later integration verification is
recorded in the JUL-188 handoff and its `TEST_REVIEW.md`; overlapping groups are not additive.
The earlier ByteDuplex documentation violations have been corrected: standard-library documentation
generation checked 123 modules with no policy violations, and the TLS module example passes target
analysis. Analysis-only examples are not runtime evidence.

The user instructed this session not to start another review-agent cycle. Delivery records a direct
assessment, not independent approval. Full `pnpm check` and `pnpm release:candidate` run in exact-head
CI; broad suites are not run locally. PR #424 remains the draft implementation delivery target.
