## 1. Canonical lifetime foundation

- [ ] 1.1 Implement lifetime identity, scoped binders/placeholders, canonical assumptions and finite outlives propagation; verify alpha identity, assumption separation, transitive obligations and invalid-region rejection with focused pure checks.
- [ ] 1.2 Add lifetime tokens, binders/arguments/bounds, reference/slice/string syntax, one outer quantified callable and Effect environment syntax; verify existing lexer/parser tests plus focused valid and malformed lifetime forms.
- [ ] 1.3 Preserve lifetimes through semantic Type values, generic arguments, canonical keys, substitutions, represented callables/Effects and every type traversal; verify round-trip semantic identity and nested substitutions in Type tests.
- [ ] 1.4 Elaborate explicit and omitted function/field/nominal lifetime parameters from declarations only, preserve well-formedness and outlives bounds, and diagnose ambiguity; verify independent Pair fields, receiver/data distinction and body-independent output signatures.
- [ ] 1.5 Derive finite nominal variance and memoized contextual compatibility with rigid quantified checks; verify shared covariance, mutable payload invariance, actual recursion, placeholder escape and nested-quantifier rejection.

## 2. Lifetime-bearing values and local obligations

- [ ] 2.1 Replace the one-source returned-view path with declared lifetime relationships through ordinary functions and callable contracts; verify common-source selection, independent pair projection and rejection of local/temporary/inline-storage escapes.
- [ ] 2.2 Propagate lifetime-bearing generic payloads through source and synthesized structs/tuples, unions, arrays and identity; verify Option<&A>, nested references and builtin string views without erasing constrained empty-variant lifetimes.
- [ ] 2.3 Generate use-driven body regions and place loans with retained dependent copies, moves and reborrow ancestry; verify a stored view survives its wrapper while source invalidation and premature exclusive-parent reuse fail.
- [ ] 2.4 Check mutations against unchanged invariant destination types including failure after mutation and displaced-storage overlap; verify valid reset to another owner and invalid short-source replacement through shortened outer access.
- [ ] 2.5 Preserve complete capture/environment bounds and exact detachment through storage/generics from first admission; verify nested string/aggregate captures and empty constrained unions cannot become detached, and retain explicit gates for exclusive storage, dependent Drop, dependent Effect outcomes and partial suspension.
- [ ] 2.6 Admit opt-in Copy for eligible shared borrowed aggregates while retaining affine defaults; verify copies preserve dependent obligations and exclusive values cannot duplicate.

## 3. Sparse partial ownership

- [ ] 3.1 Implement canonical sparse move paths, inherited initializedness, per-path finite joins, reachability, variant discriminants and lazy shape queries; verify mixed versus conditional state, sparse large arrays and convergence without state combinations.
- [ ] 3.2 Integrate path state throughout ownership reads/moves/writes, scopes, branches, loops, deferred bodies and structured exits; verify initialized siblings, whole-owner rejection, restoration, repeated loop moves and incoming-evaluation failure.
- [ ] 3.3 Admit visible nested fields and statically known in-bounds array elements with overlap, dereference and strict-ancestor user Drop checks; verify complete Drop-bearing field transfer and rejection of borrowed/dynamic/private/opaque extraction.
- [ ] 3.4 Implement match place discriminant-only refinement and consuming-match cleanup under the same ownership authority; verify guards cannot commit moves, partial payload siblings remain usable and joins require new refinement.
- [ ] 3.5 Elaborate shared cleanup recipes restricted by per-exit initializedness, including conditional fields, sparse array remainders, active variants and explicit drop place; verify every structured exit cleans only remaining ownership in established order.

## 4. Executable ownership and erasure

- [ ] 4.1 Preserve explicit consuming projected moves into MIR place extraction and ownership-state updates; verify MIR rejects borrowed holes while allowing owned field transfer and complete borrowed replacement.
- [ ] 4.2 Carry conditional initialization and exact cleanup through lowering, propagation, native cleanup and compile-time execution; verify initialization versus replacement, whole replacement, early failures and exactly-once native-corpus cleanup where structural evidence is insufficient.
- [ ] 4.3 Reject suspension with partial live owners until the frame extension exists, preserving existing complete-owner suspension checks; verify the diagnostic at the suspension boundary with focused ownership/MIR fixtures.
- [ ] 4.4 Erase lifetimes recursively at layout, instance and backend-symbol boundaries while retaining semantic/verification facts; verify differing owners/regions share runtime identities and no borrowing runtime tokens/refcounts/allocations are introduced.

## 5. Query reuse and attribution

- [ ] 5.1 Publish declaration-relative semantic lifetime/variance/environment/cleanup summaries separately from implementation fingerprints; verify stable alpha-renames and changed exported-bound identity.
- [ ] 5.2 Reuse checked declaration and generic body facts by actual consumed semantic/static inputs, with separate residual ownership accounting; verify private-body edits spare siblings/downstream bodies, new generic calls instantiate obligations, and changed consumed bounds invalidate real consumers.
- [ ] 5.3 Expose actual comparison, constraint, region, loan, path, join, cleanup, query and resolution-initiator work; verify lifetime failures never initiate candidate discovery or backend emission and repeated compatible comparisons reuse facts.
- [ ] 5.4 Add opt-in independently varying lifetime/union/wrapper/loan/binder/recursive/module/partial-field/sparse-array/join families including failing diagnostics; run representative growth and edit workloads and record attributable work with any justified algorithm/domain trade-offs.

## 6. Presentation, reconciliation and handoff

- [ ] 6.1 Format all lifetime and refinement forms canonically without elaborating ordinary format requests; verify parser/formatter round trips, comments and idempotence.
- [ ] 6.2 Expose stable readable inferred lifetimes in compiler presentation/hover and compiler-owned Make lifetimes explicit edits; verify reparsing preserves semantics and LSP rejects stale or ambiguous edits.
- [ ] 6.2a Implement `effect<'env> fn` as the explicit retained-environment spelling; verify make-explicit round trips for generic captures with incomparable outlives bounds without strengthening their contracts.
- [ ] 6.3 Reconcile reference, diagnostics, callers, fixtures and all superseded spec restrictions; verify strict OpenSpec validation, generated artifacts and focused documentation examples.
- [ ] 6.4 Run required milestone checks in order (pnpm typecheck, pnpm format:check, pnpm lint, pnpm test, pnpm check, and pnpm release:candidate for package contents/exports); record exact outcomes and distinguish pre-existing failures.
- [ ] 6.5 Complete independent code review and dedicated test-economics approval of the exact committed issue diff, with focused base/head timing and every test's unique signal; resolve findings and recheck affected paths.
- [ ] 6.6 Commit only JUL-116 changes, submit a confirmed draft PR using gh stack, publish acceptance/verification/test-economics evidence and exact PR-head Review baseline to Linear, and read back In Review before starting the dependent issue layer.
