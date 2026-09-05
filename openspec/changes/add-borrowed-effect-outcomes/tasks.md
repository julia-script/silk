## 1. Contracts and admission

- [x] 1.1 Preserve full Effect environment, outcomes, services, representation and run mode through construction, generic composition, callable results and nominal storage; verify canonical semantic contracts and rejected short environment escapes with shared focused snapshots.
- [x] 1.2 Implement dependent success and failure transfer through run, propagation, handlers, retry and cleanup; verify external nested/generic results survive temporaries while run-local and consumed owned captures cannot escape, then remove superseded admission gates.
- [x] 1.3 Preserve selected provider lifetime and reusable/consuming capture access; verify provided-row erasure does not erase loans, retained exclusive environments block access, and consuming captures transfer or clean once.
- [x] 1.4 Complete expected-type-directed bounded quantified callable/interface compatibility; verify fresh receivers, enclosing lifetime preservation, rejected stronger validity/access and binder escape without lifetime-driven discovery.

## 2. Suspension and independent execution

- [x] 2.1 Persist sparse definite/missing/conditional ownership and flag locals in suspension plans, save/restore and cleanup lowering; verify MIR for moved-before-suspend, restored-after-resume and cancellation-before-restoration cases, then remove the partial-state gate.
- [x] 2.2 Verify stable placement and cleanup dependencies across suspended frame growth, resumption and cancellation; prove moved children remain destination-owned, missing reads fail, cleanup order is exact and installation commits cannot suspend, using structural MIR and only necessary shared-native witnesses.
- [x] 2.3 Extend exact environment and independent completion checks to nested outcomes; verify external caller/provider and package-owned references are rejected independently from service rows, allocation, constrained empty variants, affinity and NonParking.

## 3. Ordinary-source integration and compiler work

- [x] 3.1 Compile local fixed-item Stream witnesses with fresh receiver lifetimes; verify two externally borrowed items survive wrapper destruction, source invalidation fails, Copy and affine item transfers preserve nested lifetimes, and self-owned scratch lending fails without compiler name recognition.
- [x] 3.2 Verify existing Box.make with borrowed elements using ordinary source; prove retained element validity, source-owner escape rejection and success/failure cleanup without adding a Box API.
- [x] 3.3 Preserve canonical environment/cleanup consumer fingerprints and runtime erasure; verify alpha-renames, private body edits, changed consumed bounds and new generic calls with query, layout and instance assertions.
- [x] 3.4 Extend and run opt-in composition-depth, callback/binder-width, provider-forwarding, module-fan-out and partial-suspension workloads including invalid programs; record actual query/generic/residual work, constraints, initialization/cleanup and resolution initiators with explained growth and consistent development/optimized verdicts.

## 4. Reconciliation and milestone verification

- [ ] 4.1 Reconcile prescriptive Effect/failure, callable, ownership, suspension and independent-execution reference, diagnostics, main specs and fixtures; verify strict OpenSpec validation, diagnostic generation and affected documentation examples.
- [ ] 4.2 At the final implementation milestone run pnpm typecheck, pnpm format:check, pnpm lint, pnpm test and pnpm check, plus pnpm release:candidate when package contents/exports change; record exact outcomes and distinguish pre-existing failures. During iteration use localized tests; do not repeatedly run the full suite.
