# Proposal

## Why

Silk needs a basic way to declare and run tests without embedding test-runner policy in the compiler. A runner written in ordinary Silk, built on narrow discovery primitives, provides a useful first implementation and the same foundation for future user-written runners.

## What Changes

- Add `test fn` and `test effect fn` for named, monomorphic, parameterless module-level functions with unit success. Keep ordinary function, Effect, visibility, ownership, and trap semantics.
- Discover only current-project tests in the active transitive import graph of one explicit discovery root. Ordinary imports include separate test files; directory scanning does not.
- Expose a finite static test catalog, declaration identity/name/source metadata, exact typed callable access, and an opaque versioned fingerprint of each test's authored HIR header and body through sealed `Intrinsic` operations.
- Implement assertion helpers, runtime filtering, sequential execution, failure reporting, and exit policy in ordinary Silk. Tests supply their own services and any scheduler/execution owner in v1.
- Add `silk test`, defaulting discovery to `package.root`, with one optional `--root`, exact project-relative `--file`, and case-insensitive test-name substring `--filter`. Filters combine with AND and affect execution only; every discovered test still compiles.
- Keep the discovery root separate from the source runner entry. Testing must not call the discovery root's `main` or require it to exist.
- Expose fingerprints without adding persistent result caching, dependency-aware skip decisions, or automatic skipping.

## Capabilities

### New Capabilities

- `silk-test-declarations`: Test qualification, admissible function contracts, visibility, authored metadata, and ordinary-build behavior.
- `silk-test-discovery`: Explicit project/root discovery context, finite static descriptors, precise callable access, phase boundaries, and authored-content fingerprints.
- `silk-test-runner`: Ordinary Silk assertion and runner APIs, sequential execution, runtime selection, reporting, and local service/execution ownership.

### Modified Capabilities

- `silk-cli-workflows`: Register the test command and define its root selection, host executable composition, runtime filter transport, artifact separation, and exit behavior.

## Impact

- Compiler syntax, authored lowering/encoding, declaration facts, static evaluation/residualization, intrinsic catalog, executable preparation, and inspection/editor surfaces.
- Bundled Silk source for testing and a default runner, reusing existing Effect, diagnostics, host-input, and native startup facilities.
- CLI command/workflow planning, existing source resolution and project membership, dedicated test artifact destinations, documentation, and focused compiler/CLI acceptance coverage.
- No package manager, general declaration reflection, source-tree scan, named test-host configuration, parallel execution, process-per-test isolation, watch mode, test-result cache, or automatic shared providers in this change. Future hosts may supply services using the preserved callable requirement types.
