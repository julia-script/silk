## 1. Canonical lifetime foundation

- [x] 1.1 Implement lifetime identity, scoped binders/placeholders, canonical assumptions and finite outlives propagation; verify alpha identity, assumption separation, transitive obligations and invalid-region rejection with focused pure checks.

  Evidence: `Lifetime.ts` and the first three `Type.test.ts` checks cover alpha/scoped identity, separate assumptions, placeholder universes, transitive cyclic propagation, expired points, and missing-region rejection. Verified by the 58-test Type run recorded below.

- [x] 1.2 Add lifetime tokens, binders/arguments/bounds, reference/slice/string syntax, one outer quantified callable and Effect environment syntax; verify existing lexer/parser tests plus focused valid and malformed lifetime forms.

  Evidence: `Lexer.test.ts` distinguishes lifetime tokens from valid/invalid character literals and preserves diagnostic spans. `Parser.test.ts` verifies named/static lifetime arguments, mixed binders and bounds, references/slices/string, outer callable quantification, callable/Effect environments, requirement arguments, `match place`, and malformed recovery. The earlier full lexer/parser/formatter run passed all 277 tests in those three files (`/tmp/silk-lifetime-syntax-final.log`; its separate LexerPressure runtime case failed and is not claimed as passing here). The current scoped evidence run below refreshes the lifetime syntax cases.

- [x] 1.3 Preserve lifetimes through semantic Type values, generic arguments, canonical keys, substitutions, represented callables/Effects and every type traversal; verify round-trip semantic identity and nested substitutions in Type tests.

  Evidence: `Type.test.ts` covers nested generic/represented substitutions, sequential substitutions, binder masking, traversal, and semantic versus runtime keys. `RuntimeSliceOwnership.test.ts` “round-trips semantic borrowed types and executable predicates without erasure” verifies ModuleSurface round-trips and subsequent substitutions of lifetime-bearing callable/represented Effect types. Existing declaration make-explicit round-trips also pass.

- [x] 1.4 Elaborate explicit and omitted function/field/nominal lifetime parameters from declarations only, preserve well-formedness and outlives bounds, and diagnose ambiguity; verify independent Pair fields, receiver/data distinction and body-independent output signatures.

  Evidence: declaration-only tests cover stable headers under body/offset edits, authored outlives and type bounds, unknown names/ambiguous outputs, and omitted nominal arity resolved after referenced declarations. The Pair make-explicit round trip preserves two independent field lifetimes and canonical contracts; RuntimeSliceOwnership verifies independent projection. A direct `Analysis.ofSource` probe additionally accepted `View<'data>` methods with `owner(self: &Self, other: &i32) -> &Self` and `data(self: &Self, other: &i32) -> &'data i32`, checking the former result equals the receiver type and the latter retains the distinct data binder. No body-derived signature or static fallback was used.

- [x] 1.5 Derive finite nominal variance and memoized contextual compatibility with rigid quantified checks; verify shared covariance, mutable payload invariance, actual recursion, placeholder escape and nested-quantifier rejection.

  Evidence: `Type.test.ts` covers covariance, mutable invariance, assumption-sensitive cached compatibility, rigid escape/nested-quantifier rejection, and proof rollback/replay. `RuntimeSliceOwnership.test.ts` “derives recursive borrowed nominal variance from actual declaration cycles” verifies mutually recursive wrapper references and recursive slices; the existing `InlineStructReach.test.ts` recursion acceptance also passes.

## 2. Lifetime-bearing values and local obligations

- [x] 2.1 Replace the one-source returned-view path with declared lifetime relationships through ordinary functions and callable contracts; verify common-source selection, independent pair projection and rejection of local/temporary/inline-storage escapes.

  Evidence: obsolete single-source helpers are removed. Type tests cover common-source selection, callable contracts, and local/temporary rejection. `RuntimeSliceOwnership.test.ts` “projects independent borrowed fields while rejecting unrelated and owned-inline escapes” accepts each independent Pair projection, rejects the unrelated return (SEM0129), and rejects an owned inline-array escape (OWN0019).

- [x] 2.2 Propagate lifetime-bearing generic payloads through source and synthesized structs/tuples, unions, arrays and identity; verify Option<&A>, nested references and builtin string views without erasing constrained empty-variant lifetimes.

  Evidence: Type tests cover semantic shared unions and generic transport. `RuntimeSliceOwnership.test.ts` “retains borrowed generic payloads across containers and builtin string views” exercises Option<&A>, named/anonymous tuples, anonymous records, arrays, identity, nested references, and both UTF-8 view intrinsics. It checks retained result lifetimes and rejects Detached for the constrained empty Option type.

- [x] 2.3 Generate use-driven body regions and place loans with retained dependent copies, moves and reborrow ancestry; verify a stored view survives its wrapper while source invalidation and premature exclusive-parent reuse fail.

  Evidence: `LifetimeFlow` and `Ownership` retain use-driven regions separately from concrete loan ancestry. The wrapper probe in `ownership-acceptance-current.mjs` accepts a projected stored view after dropping its wrapper and rejects source invalidation with OWN0011/OWN0019. Existing RuntimeSliceOwnership Copy/move cases retain dependent obligations; Ownership’s returned-exclusive-reborrow snapshot rejects overlapping siblings and premature parent access while accepting access after the child ends.

- [x] 2.4 Check mutations against unchanged invariant destination types including failure after mutation and displaced-storage overlap; verify valid reset to another owner and invalid short-source replacement through shortened outer access.

  Evidence: The installed-reference and complete-carrier-reset snapshots in `Ownership.test.ts` cover installation, outgoing-source retirement, conditional writes and same-lifetime siblings across structs, arrays and variants. The refreshed boundary probe rejects a short source assigned through shortened outer access with SEM0037, including failure after mutation, accepts a sufficiently long source, and rejects displaced self-storage overlap with OWN0019. `StatementAnalysis` activates assignment-local compatibility and `WriteStatement` carries only that checked proof slice into HIR lowering.

- [x] 2.5 Preserve complete capture/environment bounds and exact detachment through storage/generics from first admission; verify nested string/aggregate captures and empty constrained unions cannot become detached, and retain explicit gates for exclusive storage, dependent Drop, dependent Effect outcomes and partial suspension.

  Evidence: The existing Suspendability snapshot accepts moved static string views, nested static wrappers and an explicit static-bound generic owner, while rejecting a local-byte string wrapper and nested borrowed Effect at exact SEM0139 call spans. Type/RuntimeSliceOwnership cover retained environments and constrained-empty Option detachment. DeclarationIndex’s staged admission cases retain exclusive-storage, dependent Drop and borrowed Effect-outcome gates; SuspensionOwnership rejects partial suspension with OWN0020 and unavailable MIR. Detached uses already-checked type-outlives assumptions; runtime identity reuse remains restricted to NonParking facts.

- [x] 2.6 Admit opt-in Copy for eligible shared borrowed aggregates while retaining affine defaults; verify copies preserve dependent obligations and exclusive values cannot duplicate.

  Evidence: `RuntimeSliceOwnership.test.ts` “requires opt-in Copy for borrowed aggregates and preserves copied dependencies” accepts an explicit Copy view, rejects its copied local escape (OWN0019), and rejects use after affine/default or exclusive transfer (OWN0001). Direct conformance queries distinguish Copy View, affine default, and exclusive references.

Scoped evidence command: `pnpm exec vitest run packages/compiler/test/Type.test.ts` — 58/58 passed (latest verified run: 2026-09-04, `/tmp/lifetime-proof-58.log`). Additional verified commands: `pnpm exec vitest run packages/compiler/test/RuntimeSliceOwnership.test.ts -t "round-trips semantic borrowed|recursive borrowed nominal|projects independent|borrowed generic payloads|opt-in Copy"` — 5/5 passed (`/tmp/lifetime-evidence-probes-final3.log`); `pnpm exec vitest run packages/compiler/test/DeclarationIndex.test.ts packages/compiler/test/InlineStructReach.test.ts -t "makes lifetime headers explicit|makes retained Effect environments explicit|accepts recursive lifetime-bearing"` — 3/3 passed (`/tmp/lifetime-existing-evidence.log`). These are scoped semantic checks, not broader handoff verification.

## 3. Sparse partial ownership

- [x] 3.1 Implement canonical sparse move paths, inherited initializedness, per-path finite joins, reachability, variant discriminants and lazy shape queries; verify mixed versus conditional state, sparse large arrays and convergence without state combinations.

  Evidence: `MovePath` uses sparse inherited states and finite joins; existing Ownership algebra tests distinguish incomplete versus Missing ancestors, Mixed versus Maybe, feasible active variants, restoration and idempotent joins. The opt-in sparse-array workload retains three state nodes and two selector computations while array length grows from 16 to 65,536; no missing-field type or variant-state product is constructed.

- [x] 3.2 Integrate path state throughout ownership reads/moves/writes, scopes, branches, loops, deferred bodies and structured exits; verify initialized siblings, whole-owner rejection, restoration, repeated loop moves and incoming-evaluation failure.

  Evidence: Existing Ownership tests exercise initialized sibling use, whole-owner rejection, field restoration and conditional state. The ownership acceptance probe rejects an unrestored repeated loop move with OWN0005, accepts restoration before fallthrough/continue, and retains f0=Maybe after a partial break. Its incoming-failure case verifies that propagation cleanup still sees f0=Missing before an assignment succeeds. The guarded-loop MIR regression now passes after making omitted fields explicit with `..` (2026-09-05, `final-fixture-fixes.log`).

- [x] 3.3 Admit visible nested fields and statically known in-bounds array elements with overlap, dereference and strict-ancestor user Drop checks; verify complete Drop-bearing field transfer and rejection of borrowed/dynamic/private/opaque extraction.

  Evidence: Ownership/StructValues fixtures cover visible nested and constant-index extraction, borrowed/dynamic rejection, disjoint loans, complete Drop-bearing child transfer and strict-ancestor Drop exclusion. The two-module acceptance probe accepts a visible field, rejects a private field with SEM0028, and rejects extraction through an opaque callable abstraction with SEM0026. Explicit Copy moves remain consuming while ordinary Copy reads preserve their source.

- [x] 3.4 Implement match place discriminant-only refinement and consuming-match cleanup under the same ownership authority; verify guards cannot commit moves, partial payload siblings remain usable and joins require new refinement.

  Evidence: The place-match acceptance probe preserves the discriminant, moves one selected payload field, reads its sibling and successfully refines the remaining partial union again; a deliberately consuming guard reports OWN0008. Existing Ownership tests cover provisional consumed-match guards, omitted-field cleanup and feasible variant joins. The expanded guarded-loop MIR case verifies fresh arm bindings and result initialization on each iteration.

- [x] 3.5 Elaborate shared cleanup recipes restricted by per-exit initializedness, including conditional fields, sparse array remainders, active variants and explicit drop place; verify every structured exit cleans only remaining ownership in established order.

  Evidence: The structured-exit acceptance probe retains f0=Missing on ArmEnd, Break, Continue and typed-failure Return releases, and a sparse array exit retains only i1=Missing with ArrayCleanup. Existing MIR corruption assertions reject incorrect state/flag recipes. `NativeAggregate` skips missing paths, guards conditional paths and preserves array cleanup order. The shared native corpus case `partial-owner-cleanup-and-refinement` passed at PR checkpoint 3e17d365 and pins destructor trace `1212354678`; this is case evidence, not a claim that that CI run was globally green.

## 4. Executable ownership and erasure

- [x] 4.1 Preserve explicit consuming projected moves into MIR place extraction and ownership-state updates; verify MIR rejects borrowed holes while allowing owned field transfer and complete borrowed replacement.

  Evidence: consuming owned projections and exact initialization paths are retained in MIR. The existing exclusive replacement fixture now reaches both replacement functions, verifies valid MIR, and removes the paired write to prove `InvalidSliceOperation` rejects a borrowed hole (85 ms focused test work, `/tmp/silk-lifetime-work/borrowed-hole-focused.log`). Its unrelated standard-library import was removed. An independent direct-reference corruption probe also produced `InvalidAggregateOperation`; neither proof requires backend emission.

- [x] 4.2 Carry conditional initialization and exact cleanup through lowering, propagation, native cleanup and compile-time execution; verify initialization versus replacement, whole replacement, early failures and exactly-once native-corpus cleanup where structural evidence is insufficient.

  Evidence: Whole replacement retains the displaced partial state and restores complete initialization only after incoming evaluation; the acceptance failure probe carries f0=Missing in RunStaticEffect releases with verifier-clean MIR. `StaticText.test.ts` covers nested/constant-array projected restoration and complete-child replacement through StaticEvaluation (expected result 23). The shared native partial-owner corpus case passed at 3e17d365 (643 ms in `ci-3e17-clean.log`). Current guarded-loop MIR verification also passes. Full current-head milestone checks remain tracked separately in 6.4.

- [x] 4.3 Reject suspension with partial live owners until the frame extension exists, preserving existing complete-owner suspension checks; verify the diagnostic at the suspension boundary with focused ownership/MIR fixtures.

  Evidence: `Realization.finalizeMir` now publishes suspension ownership planner violations as OWN0020 and makes MIR unavailable, including when normalization is disabled. `SuspensionOwnership.test.ts` checks the exact partial-owner run span while retaining a complete-owner control. Full focused `SuspensionOwnership.test.ts` and `SuspensionMir.test.ts` run: 6/6 passed (`/tmp/silk-lifetime-work/suspension-gate-group.log`). Independent review confirmed the publication gate with normalization enabled and disabled; a separate residualization review confirmed that compile-time failures withhold MIR while preserving layout diagnostics.

- [x] 4.4 Erase lifetimes recursively at layout, instance and backend-symbol boundaries while retaining semantic/verification facts; verify differing owners/regions share runtime identities and no borrowing runtime tokens/refcounts/allocations are introduced.

  Evidence: `Instances.test.ts` "shares runtime instances and layouts across distinct proven lifetime arguments" retains distinct semantic keys while checking one read/wrap instance, shared reference/nominal layouts and calling shapes, equal backend symbols, and verifier-clean MIR. Type tests additionally cover nested generic/represented runtime erasure. Source audit confirms runtime-key boundaries in Layout, Instances and Backend; `NativePlaceOperation` lowers borrows to existing address/view lanes and makes EndLoan a no-op, introducing no lifetime runtime token, reference-count operation, or lifetime allocation.

## 5. Query reuse and attribution

- [x] 5.1 Publish declaration-relative semantic lifetime/variance/environment/cleanup summaries separately from implementation fingerprints; verify stable alpha-renames and changed exported-bound identity.

  Evidence: `ModuleSurface.memberSignature` includes canonical lifetime binders, bounds, executable environments and nominal fields/Drop contracts; `memberImplementation` records consumed static syntax separately and canonicalizes lifetime spelling. `NominalVariance`, `TypeOutlives` and `CleanupPlan` retain declaration-context summaries and reusable recipes. ProjectAnalysis verifies zero body/ownership checks after alpha-renaming and targeted invalidation after an exported bound changes; DeclarationIndex independently verifies body-independent lifetime headers.

- [x] 5.2 Reuse checked declaration and generic body facts by actual consumed semantic/static inputs, with separate residual ownership accounting; verify private-body edits spare siblings/downstream bodies, new generic calls instantiate obligations, and changed consumed bounds invalidate real consumers.

  Evidence: `BodyQuery` records actual consumed semantic and static implementation dependencies, rebinding retained facts after source edits. ProjectAnalysis checks one edited private body while reusing four others, all five bodies after alpha-renaming, one newly added caller while reusing the generic body, and exactly two bodies after a consumed bound changes; its static-dependency case checks only the three transitive consumers and retains the actual recursive component. Ownership verifies exact source-proof reuse, failed residual caching and misses for changed HIR, semantic facts, index or local-shared boundaries. Instances asserts zero executed residual body/ownership work for ordinary generic specialization; StaticText separately verifies repeated static-query reuse and cached growth failures.

- [x] 5.3 Expose actual comparison, constraint, region, loan, path, join, cleanup, query and resolution-initiator work; verify lifetime failures never initiate candidate discovery or backend emission and repeated compatible comparisons reuse facts.

  Evidence: `Lifetime.solve`, contextual TypeCompatibility, Ownership, CleanupPlan, BodyQuery and ResolutionWork expose actual finite work with initiator attribution. Existing Type tests prove assumption-separated comparison reuse and rollback/replay of accepted obligations; Ownership verifies source/cache hits contribute zero executed residual work. The committed frontend/residual reports distinguish retained proof counts from executed query work and retain failing diagnostic samples. Lifetime solvers consume resolved finite types/assumptions without resolver, provider-selection or backend callbacks; the benchmark records observed resolution initiators separately and executes no backend. Its documented attribution limits do not claim whole-compiler resolver totals.

- [x] 5.4 Add opt-in independently varying lifetime/union/wrapper/loan/binder/recursive/module/partial-field/sparse-array/join families including failing diagnostics; run representative growth and edit workloads and record attributable work with any justified algorithm/domain trade-offs.

  Evidence: `benchmarks/lifetimes.md` and the two committed JSON reports distinguish 69 frontend growth/edit samples from nine target-selected residual samples. At sizes 4/8/16, ordinary generic owners execute zero residual body or ownership checks; repeated static applications execute each query once and reuse it 3/7/15 times. Distinct static branches perform separate checks. Reports retain actual work, failed-source diagnostics, selection reasons, and the documented reachability/history memory tradeoffs; timings are observations, not correctness assertions.

## 6. Presentation, reconciliation and handoff

- [x] 6.1 Format all lifetime and refinement forms canonically without elaborating ordinary format requests; verify parser/formatter round trips, comments and idempotence.

  Evidence: the formatter lifetime fixture preserves comments and omitted `&T`, canonically prints bounds, reference/slice/string, `for<'call> once fn<'env>`, `Effect<'env; ...>`, `effect<'env> fn`, and contextual `match place`, then checks normalized reparse equality and formatting idempotence. The full syntax run above passed the formatter suite; its lifetime round trip is refreshed by the scoped evidence run below.

- [x] 6.2 Expose stable readable inferred lifetimes in compiler presentation/hover and compiler-owned Make lifetimes explicit edits; verify reparsing preserves semantics and LSP rejects stale or ambiguous edits.

  Evidence: compiler make-explicit tests compare canonical declaration/field/callable keys after reparsing and verify second application produces no edits. The LSP tests preserve Unicode/comment text, reject changed document versions and changed bytes, and withhold edits for ambiguous/invalid headers and body-only selections. The inherent-member hover test checks the stable `self: &'life0 Counter` signature. Current scoped command: `pnpm exec vitest run packages/compiler/test/Lexer.test.ts packages/compiler/test/Parser.test.ts packages/compiler/test/SyntaxFormatter.test.ts packages/compiler/test/DeclarationIndex.test.ts packages/compiler/test/Instances.test.ts packages/compiler/test/RuntimeSliceOwnership.test.ts packages/lsp/test/Document.test.ts -t 'lifetime|retained Effect environments|independent borrowed fields|nests inherent members'` — 17/17 passed, 470 unrelated cases skipped (2026-09-05, `/tmp/silk-openspec-assigned-evidence.log`).

- [x] 6.2a Implement `effect<'env> fn` as the explicit retained-environment spelling; verify make-explicit round trips for generic captures with incomparable outlives bounds without strengthening their contracts.

  Evidence: parser and formatter fixtures accept and preserve the declaration environment slot. DeclarationIndex's retained-environment round trip covers generic T captures, two independent borrowed inputs, and T with incomparable `'a + 'b` bounds; emitted `effect<'env> fn` plus predicates reparse to exactly equal executable semantic keys and a second expansion is empty. The service operation fixture also retains an explicit static environment. Refreshed in the 17-test scoped run above.

- [ ] 6.3 Reconcile reference, diagnostics, callers, fixtures and all superseded spec restrictions; verify strict OpenSpec validation, generated artifacts and focused documentation examples.

  Partial evidence: reference lifetime/ownership/callable/Effect pages, diagnostic catalog, stdlib signatures, callers, fixtures and the affected main OpenSpec restrictions have been reconciled during implementation; an earlier generated-documentation build/check passed. Left unchecked: current-head full documentation/example validation and the remaining caller/fixture reconciliation belong to the pending milestone. `pnpm exec openspec validate add-lifetime-values-and-partial-moves --strict` passed (2026-09-05, `/tmp/silk-openspec-assigned-strict.log`); this does not substitute for those checks.

- [ ] 6.4 Run required milestone checks in order (pnpm typecheck, pnpm format:check, pnpm lint, pnpm test, pnpm check, and pnpm release:candidate for package contents/exports); record exact outcomes and distinguish pre-existing failures.

  Milestone evidence (2026-09-05): `pnpm typecheck` passed 18/18 tasks, `pnpm format:check` passed, and `pnpm lint` passed. The first `pnpm test` attempt stopped in `@silklang/docgen`'s standard-library doctest: 59/60 examples passed; `silk/hash_map:27` reported OWN0010 after consuming an insertion Effect and then mutably accessing the map again. The focused fix now passes the failing example: returned-reference provenance permits the proven RawBuffer parent reborrow used by HashMap.withMut while preserving sibling and live-child conflicts. The full documentation example gate has not yet been rerun. The separately run compiler parallel milestone finished with 2,004 passing and 217 failing tests across 211 files; failures include stale lifetime fixtures and shared ownership/backend defects. Its log is `/tmp/silk-lifetime-work/milestone-compiler-parallel.log`; the exact failing case inventory is `/tmp/silk-lifetime-work/milestone-failures.txt`. Focused fixes are in progress; this is not a passing handoff. `pnpm check` and `pnpm release:candidate` remain unrun. Logs: `/tmp/silk-lifetime-work/milestone-{typecheck,format,lint,test}.log`.

  Second milestone: the standard-library documentation gate now passes all 60 examples (docgen 66/66 tests). `pnpm test` progressed to a failing CLI starter check; the provider-context fix subsequently passed its targeted init/check/build/run test (7.54 s). The separate compiler parallel run finished with 2,132 passing and 102 failing tests across 211 files in 415.97 s; `/tmp/silk-lifetime-work/milestone2-failures.txt` records the remaining cases. Some focused fixes landed during or after that run, so it is milestone evidence rather than final-diff validation. Current checkpoint `pnpm typecheck` passes 18/18, formatting and lint pass. Remaining work includes stale lifetime fixtures and unresolved lowering/ownership regressions; `pnpm check`, release-candidate validation and final test-economics approval remain pending.

- [ ] 6.5 Complete independent code review and dedicated test-economics approval of the exact committed issue diff, with focused base/head timing and every test's unique signal; resolve findings and recheck affected paths.
- [ ] 6.6 Commit only JUL-116 changes, submit a confirmed draft PR using gh stack, publish acceptance/verification/test-economics evidence and exact PR-head Review baseline to Linear, and read back In Review before starting the dependent issue layer.
