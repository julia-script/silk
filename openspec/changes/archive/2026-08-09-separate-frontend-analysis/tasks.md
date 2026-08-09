## 1. Canonical Compiler Pipeline

- [x] 1.1 Add the internal Pipeline actor with immutable phase observations and optional heap probing.
- [x] 1.2 Move closure, declaration, resolution, elaboration, ownership, and frontend diagnostic orchestration into `Pipeline.frontend` with equivalence coverage.
- [x] 1.3 Move instance discovery, target/layout planning, MIR lowering, and runtime diagnostics into `Pipeline.realize` with phase-order coverage.

## 2. Analysis Snapshot Separation

- [x] 2.1 Split the facade into immutable `FrontendSnapshot` construction and explicit realized `Snapshot` derivation, then narrow query inputs by required snapshot level.
- [x] 2.2 Add Analysis phase observations and tests for frontend phase exclusion, immutable realization, repeated determinism, and multiple target realizations.
- [x] 2.3 Migrate compiler, CLI, documentation, and test consumers so each explicitly requests frontend or realized analysis according to the facts it uses.

## 3. Driver Integration

- [x] 3.1 Replace Driver's duplicate compiler orchestration with Pipeline frontend and realization while preserving rejection, backend, toolchain, and outcome behavior.
- [x] 3.2 Preserve driver heap observations and add tests that compare canonical Analysis/Driver phase order and exclude runtime phases after frontend rejection.

## 4. Language Server Integration

- [x] 4.1 Change Workspace, ProjectSession, and document feature adapters to retain and consume frontend snapshots only.
- [x] 4.2 Extend LSP coverage to prove all existing editor features use the version-matched frontend snapshot and execute no realization phase.

## 5. Verification

- [x] 5.1 Update affected public documentation and explicit package exports for the breaking snapshot construction interface.
- [x] 5.2 Run typecheck, Biome, tests, `pnpm check`, OpenSpec strict validation, and `pnpm release:candidate`.
