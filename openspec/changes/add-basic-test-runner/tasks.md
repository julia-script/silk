# Tasks

## 1. Test declarations and authored metadata

- [x] 1.1 Add contextual `[pub] test [effect] fn` parsing and authored-header lowering, preserving qualifier spans and existing identifier/module-path meanings; extend existing parser/lowering fixtures for both forms, static-branch declarations, local recovery, and source-free authored round trips.
- [x] 1.2 Carry test qualification through declaration completion and inspection, enforcing module-level safe runtime functions with bodies, no parameters/binders, and unit success while preserving exact Effect channels; prove valid and rejected contracts with shared structured analysis fixtures and diagnostic code/span assertions.
- [x] 1.3 Integrate the qualifier into formatter, highlighting, editor/documentation declaration views, and affected generated schema/catalog fixtures; demonstrate lossless formatting and unchanged ordinary private-name resolution, and show that the qualifier alone adds no ordinary-build executable root.

## 2. Root-scoped test discovery

- [x] 2.1 Add the explicit discovery root and requesting-project ownership/path mapping to compiler requests and preparation, independent of application/runtime roots; prove import-only and missing-root behavior, sealed source preparation, and logical identity changes for different discovery contexts.
- [x] 2.2 Construct the deterministic active test catalog from the discovery root's import closure intersected with project ownership; extend existing closure/preparation coverage for direct imports, diamonds/cycles, inactive imports, unimported files, dependency/toolchain tests, and runner-only imports without directory scanning.
- [x] 2.3 Add dependency observations for catalog membership, qualifier/signature changes, and current metadata; use adjacent-revision fixtures to prove private test additions/removals and location changes refresh discovery even when ordinary public semantic surfaces stay equal.

## 3. Static primitives and fingerprints

- [x] 3.1 Add sealed catalog/test descriptor types plus `Intrinsic.tests` and `Intrinsic.testInfo` contracts to the intrinsic inventory and static evaluator; demonstrate heterogeneous finite iteration, exact metadata, required discovery context, module-selection rejection, and budget/phase enforcement with focused static-analysis fixtures.
- [x] 3.2 Add `Intrinsic.testFunction` as a static-descriptor-to-ordinary-callable specialization bridge; prove private marked-test access, rejected unrelated private access/forged descriptors, exact heterogeneous failure/requirement types, and descriptor-free residual TIR/MIR through existing structural suites.
- [x] 3.3 Implement the versioned domain-separated test fingerprint by framing canonical authored header/body bytes and reusing the existing digest boundary; add encoding/golden cases for body/header changes, trivia/location/pool stability, separate owner identity, and unchanged test content after a helper implementation edit.
- [x] 3.4 Connect fingerprint observations to static catalog invalidation and expose the new primitive metadata in compiler tooling; demonstrate a body-only test edit updates the observed fingerprint without relying on exported surface changes or claiming result-cache validity.

## 4. Ordinary Silk testing library and runner

- [x] 4.1 Add a minimal provider-free boolean assertion helper carrying a diagnostic message through an ordinary typed failure; prove true/false behavior and normal Effect recovery in the shared native acceptance corpus, with no compiler-known assertion actor.
- [x] 4.2 Implement ordinary Silk adapters for parameterless unit functions and unit-success Effects with arbitrary valid failures, empty residual requirements, and the existing complete-invocation non-parking boundary; prove unsupported residual requirements structurally and add shared acceptance cases for local providers, sequential continuation, and exactly-once cleanup.
- [x] 4.3 Implement exact logical-file and locale-independent ASCII case-insensitive name-substring filtering in Silk, including combined predicates and empty patterns; add shared acceptance cases that distinguish exact path matching, mixed case, literal metacharacters, and AND behavior.
- [x] 4.4 Add the bundled Silk runner entry with pre-invocation test identity, per-test diagnostics/results, summary counts, status 0/1/2 policy, and explicit zero-selection output; reuse existing source diagnostics/startup and extend shared execution fixtures for typed failure continuation and fatal trap behavior without a new scheduler, isolation layer, or result cache.
- [x] 4.5 Add a separately named ordinary Silk runner fixture using the same explicit discovery request and primitives; prove through analysis and the shared native corpus that custom source policy can inspect and invoke tests without compiler name registration and that repeated runs execute matching tests despite equal fingerprints.

## 5. CLI test workflow

- [x] 5.1 Register `silk test` with shared project/profile controls and single `--root`, `--file`, and `--filter` options; extend existing command/workflow coverage for help, manifest-relative path normalization, default package root, unchanged source root, and required-root failures.
- [x] 5.2 Compose the ordinary bundled runner as the application entry, the discovery root as a distinct required root, and the hosted runtime as a final native host test executable; extend workflow planning fixtures for import-only roots, no test-root main invocation, library packages, incompatible explicit selections, and distinct test artifact destinations.
- [x] 5.3 Transport normalized filters through runtime arguments and preserve runner output/status/abnormal termination; extend existing CLI integration coverage to show filters do not change compiled content identity or load files, a filtered-out compilation error prevents execution, and test output does not overwrite ordinary artifacts.

## 6. Reference and user documentation

- [x] 6.1 Document test syntax/contracts, discovery ownership and imports, the three intrinsic operations, authored-fingerprint limitations, and ordinary-build behavior in the language reference; include the helper-edit counterexample and explicit absence of result caching, with all source examples checked by the existing documentation workflow.
- [x] 6.2 Document the testing library and CLI using colocated tests, an import-only test root, combined filters, and locally provided services; show zero-match/failure/trap outcomes and identify shared providers, configurable hosts, and dependency-aware result reuse as future work rather than current promises.
