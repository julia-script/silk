## 1. Catalog Availability

- [ ] 1.1 Add a normalized evaluator/backend supported-target set to every sealed intrinsic catalog entry and mark all current operations as all-target.
- [ ] 1.2 Extend intrinsic inventory validation, deterministic encodings, and tooling facts to require and expose supported-target metadata.
- [ ] 1.3 Add tests proving ordinary source declarations cannot acquire compiler target privilege by name or annotation.

## 2. Executable Reachability

- [ ] 2.1 Extend executable closure planning to retain the exact canonical intrinsic identities reachable from the selected entry and instances.
- [ ] 2.2 Keep source module loading and semantic analysis target-neutral so unreachable restricted declarations remain navigable and inert.
- [ ] 2.3 Add deterministic target validation over the reachable intrinsic inventory after closure and before evaluator/backend entry.
- [ ] 2.4 Add the stable target-unavailable diagnostic with intrinsic identity, requested target, call provenance, and deterministic ordering.

## 3. Evaluator and Backend Enforcement

- [ ] 3.1 Require evaluation requests to consume a validated intrinsic inventory and reject reachable unsupported operations before execution.
- [ ] 3.2 Require LLVM link planning to include runtime support only for retained intrinsic identities.
- [ ] 3.3 Require direct-Wasm import planning to include only retained identities and reject unsupported ones before partial artifact construction.
- [ ] 3.4 Preserve explicit backend selection when validating targets supported by multiple backends.

## 4. Pay-for-Use Acceptance

- [ ] 4.1 Add fixtures where an unsupported intrinsic is loaded but unreachable and prove evaluation, LLVM, and direct Wasm remain accepted.
- [ ] 4.2 Add matching fixtures where that intrinsic becomes reachable and prove the stable target diagnostic is emitted.
- [ ] 4.3 Add artifact inventory tests proving unreachable restricted operations contribute no runtime symbols, host imports, or adapters.
- [ ] 4.4 Add fresh-process determinism coverage for closure inventories, diagnostics, and emitted artifact inventories.
- [ ] 4.5 Regenerate committed manifests and goldens, run `pnpm check`, and run `pnpm release:candidate` if package contents or exports changed.

