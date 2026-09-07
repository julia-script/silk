## 1. Contract and catalog

- [x] 1.1 Complete prescriptive stream contract, pin headers/tools/prior-art sources and validate the descriptive PlatformCatalog production/update record before implementation.
- [x] 1.2 Implement selected read/write/errno declarations and source descriptor transfer policy; verify target availability and exact C types/constants/symbols.

## 2. Providers and deletion

- [x] 2.1 Migrate Writer factories/providers and StandardInput with zero-capacity behavior, exact prefixes/tails, immediate error translation and per-provider EOF latching.
- [x] 2.2 Delete both stream intrinsics, HostWrite HIR/MIR/backend paths, reserved imports, C adapters and forced inclusion; migrate every direct caller, fixture, formatter/logger and suspended consumer.
- [x] 2.3 Update prescriptive/public/generated docs and inventory together; verify portable source providers on Wasm without native imports.

## 3. Conformance and delivery

- [x] 3.1 Execute deterministic foreign-boundary fixtures and independent header/symbol checks on all three debug/optimized native lanes; fail skipped/missing lanes and reject LTO.
- [x] 3.2 Extend the shared native corpus only for distinguishing real stream behavior; retain exact conformance results and audit source/artifact absence.
- [ ] 3.3 Run typecheck, format:check, lint, test, check and release:candidate, record exact outcomes and publish through gh stack.
