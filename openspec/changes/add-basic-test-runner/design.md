# Design

## Context

See `proposal.md` for motivation and scope. The current compiler already follows selected imports, resolves canonical module identities, supports heterogeneous `static for`, and residualizes ordinary typed callable values. Source-written native startup owns process inputs, diagnostics, and exit conversion. Ordinary functions cannot propagate typed failures; Effects can, while traps remain fatal.

The authored-HIR pipeline supplies source-independent owner identity and canonical header/body encodings through `AuthoredEncoding`. Its SHA-256 digest is content metadata, not proof of semantic reuse or executable equivalence. Compiler query reuse can legitimately reuse a caller after a callee body changes; a test-result cache could not make that same decision.

This design crosses syntax, preparation, static elaboration, source runtime composition, and CLI planning, so a shared design artifact is necessary.

## Goals / Non-Goals

**Goals:** Make test declarations inspectable and invocable by ordinary Silk without erased callable types, hidden standard-library privilege, or a second runtime. Keep discovery independent of the runner implementation and preserve typed requirements for future hosts.

**Non-Goals:** General module/declaration reflection, arbitrary private-member access, new failure semantics, shared test fixtures/providers, test isolation, automatic result reuse, or new package acquisition and ownership rules. A runtime list of erased test functions is not needed for the first runner.

## Decisions

### 1. Test qualification is orthogonal to Effect semantics

Use `[pub] test [effect] fn name() [-> ()] ... { ... }` at module declaration scope, including selected module-level static branches. The test qualifier is retained in syntax, authored headers, declaration facts, and tooling. Treat `test` contextually in the function-modifier position so normal identifier/module-path use remains possible. This avoids expanding the language's reserved-name surface merely to add a declaration marker.

Tests are safe, named runtime functions with bodies, no parameters, no generic/lifetime binders, and unit success. Reject the qualifier on static, unsafe, foreign/exported, anonymous, or associated functions and on other declaration kinds. Unsafe operations may still appear inside an ordinary explicit unsafe boundary in a test body. Keep ordinary duplicate-name and callable rules.

An ordinary test function returns normally to pass. An Effect test constructs an ordinary `Effect<() ! E ? R>`. Do not infer Effect semantics from `test`, introduce a privileged test error type, or make the compiler know assertion helper names. The first runner accepts Effects with arbitrary valid `E` and an empty residual `R`; this is runner policy, not a permanent restriction on test declarations.

In ordinary builds, active test declarations undergo normal analysis but their qualifier neither runs nor retains them. They remain normally callable where visible. This intentionally does not add test-only imports or conditional compilation. Separate test roots can keep dedicated test modules out of production source graphs.

### 2. Explicit discovery context is independent of application entry

Add an optional compiler-request discovery context identifying one canonical test root and the requesting project's authoritative source ownership. It is absent for ordinary compilation. It is an explicit input to preparation, static-query dependencies, and logical artifact identity, not a mutable global flag or a special library name.

For the CLI, the application entry of the test executable is the bundled runner module, composed with the existing hosted source runtime. The test root is an additional required source root. The runner's `main` is executed; the discovery root's `main` is neither required nor called. A compiler client may supply another ordinary Silk runner as the application entry with the same discovery context; a custom-runner CLI/configuration surface is deferred.

Complete normal module static selection before catalog enumeration. Traverse active import edges from the test root, then include only declarations whose source owner is the requesting project. Traverse dependencies normally for compilation, but exclude their tests and toolchain tests from the catalog. A source loaded solely by runner/runtime composition does not become a discovery candidate. Ordinary imports and diamond/cyclic graphs need no registration call and yield each declaration once.

Project membership comes from the compilation source supply, not physical path prefix guessing. The current project file resolver supplies project-owned modules; reserved toolchain modules are excluded. Future dependency supplies must retain distinct ownership rather than masquerading as project source. Do not introduce a package manager to solve this distinction. The request must make this ownership available to in-memory compiler clients too.

Catalog order is canonical module identity, then declared test name, then canonical authored owner identity. Use identity for deduplication, not a name or content hash. Reject catalog queries without a discovery context, from module-selection/configuration evaluation, or before a valid selected declaration surface exists. In particular, a catalog query cannot decide the imports that define its own catalog. Preparation seals all needed source before downstream lowering.

### 3. Three narrow intrinsic operations, no test-execution intrinsic

Use the following source-facing shape; `F` denotes the exact callable type, not an erased runtime interface:

| Operation | Contract |
| --- | --- |
| `Intrinsic.tests()` | Static-only finite heterogeneous iterable of sealed `Intrinsic.Test<F>` descriptors for the request's catalog. |
| `Intrinsic.testInfo(descriptor)` | Static-only projection to ordinary immutable metadata values: opaque declaration identity, name, canonical module, project-relative source path, current declaration line/column, and authored fingerprint. |
| `Intrinsic.testFunction(descriptor)` | Consume a required static descriptor during specialization and produce the corresponding ordinary runtime callable of exact type `F`. |

The catalog iterable and descriptors are phase-only compiler values. They cannot be forged, serialized as runtime descriptors, addressed, or passed through a residual parameter. The callable projection is the same kind of staging bridge as static field projection: no descriptor or discovery intrinsic survives into runtime TIR/MIR/backend operations. Metadata strings and integer coordinates may become ordinary constants when used by the runner. Locations refer to current presentation; they are excluded from the fingerprint. For in-memory sources, the request supplies the logical project-relative source path; no ambient filesystem probe is needed.

`test` explicitly authorizes access to that test callable through an admitted descriptor, including private tests. This does not authorize ordinary qualified access to the private function or inspection/calling of unrelated private declarations. A returned callable retains its original function identity, signature, failure channel, and requirement row. A test can therefore call private helpers normally from its own module.

The bundled source wrapper uses `static for` to specialize ordinary generic runner helpers for each concrete `F`. Do not build a runtime reflection registry, compiler-owned result sum, `runTest` intrinsic, or compiler-known `Test` actor. A copied/renamed source runner must work through the same public operations. Static expansion follows existing budgets and atomic-publication rules.

Record catalog membership and every observed qualifier, signature, metadata, and fingerprint as static dependencies. Adding/removing a test, changing its body, or moving its current location must refresh the relevant catalog projections even when ordinary exported semantic surfaces are unchanged. This is catalog correctness, not a test-result cache.

### 4. Authored fingerprints are deliberately local

Compute a domain-separated, versioned SHA-256 digest of the canonical authored header and body encodings, framing both components. Expose an opaque string including its fingerprint format version. Keep declaration identity separate, so equal content does not merge tests. Reuse `AuthoredEncoding`; do not hash raw source, pool indices, presentation, TIR object addresses, or emitted binaries.

Whitespace/comments, source movement, and unrelated pool changes preserve the fingerprint. Header/body edits change its input bytes. Called function bodies, resolved type meanings outside the declaration, providers, selected target/profile, runner/compiler versions, external files, environment, time, and network state are not covered. A helper edit is an explicit counterexample: the test fingerprint may stay equal while its result changes.

No result cache is written or consulted. The builtin runner executes every matching test even when its fingerprint matches a previous run. Future result caching needs a separate dependency/configuration/input contract; even a whole-compilation key alone does not describe changing external state.

### 5. Runner and assertions are ordinary Silk

Add an actor-oriented testing library and bundled runner entry. The library provides a minimal boolean expectation operation with a caller-supplied diagnostic message and an ordinary typed assertion failure. It needs no ambient provider. Equality-specific helpers, snapshots, fixtures, skip annotations, parameterization, and pretty printers are deferred.

For each matching catalog element, print its module/name before invoking it, adapt ordinary unit or an Effect producing unit through ordinary interfaces/generic functions, observe the outcome with existing diagnostic facilities, and update pass/fail counts. Catch arbitrary lifetime-valid typed failures without requiring `Display`/`Report` conformance on their payloads. Preserve available origin/trace reporting, drop owned failures exactly once, and finish structured cleanup before starting the next test. A trap terminates the process under existing semantics; printing the current test first makes the active case identifiable.

Tests must close their own service requirements and provide any scheduler/Execution owner needed to complete parking work. The bundled runner enforces the existing non-parking host boundary on each complete invocation; it does not install a hidden scheduler or expose its internal host-input service to tests. A future source host may supply providers before the same typed invocation. Local provider setup is knowingly clunky; do not address it by silently making `test` effectful or implicitly supplying services.

Default policy choices for unspecified small behaviors: print per-test pass/fail and a final discovered/selected/passed/failed summary; return 0 when every selected test passes and 1 when any test fails; zero matches return 0 with an explicit zero-selected summary. Operational runner failures use status 2. Fatal termination remains fatal and is not converted to an ordinary test failure.

### 6. CLI composition and runtime selection

`silk test` discovers the nearest manifest using the existing workflow and accepts `--manifest-path`. Select `package.root` unless `--root` supplies one project-relative `.silk` path. Interpret project-relative paths from the manifest directory, retain the configured source root, and require the overridden root to belong to it. Root failures use existing required-root error behavior. An import-only root is valid.

Build a final native host executable for the test purpose, even when the package normally builds a library. Reuse compatible profile bindings, optimization, and native link inputs. Reject an explicitly selected foreign target, absent/custom incompatible runtime, or non-executable test profile before running; do not launch cross-target artifacts or add host matrices. The ordinary project build/run configuration and artifacts retain their existing meaning. Place test outputs in a dedicated test-purpose subtree under the existing output directory so they cannot overwrite application or library artifacts.

Normalize `--file` to the same exact project-relative logical path stored in catalog metadata, preserving case. Do not glob, scan, or load its path; an existing but unreachable file simply matches nothing. `--filter` matches the declared identifier name, excluding module/path decoration. Identifiers currently use ASCII, so implement locale-independent ASCII letter folding and literal substring comparison in Silk; other bytes are literal, an empty search matches all names, and regex/glob/Unicode-normalization behavior is not added. Both predicates must hold when supplied together.

The CLI transports normalized options through ordinary process arguments/HostInput. It must not prune the compiler catalog or generate a prefiltered harness. Changing runtime filters leaves compiled test content and test artifact identity unchanged. All discovered test callable paths are compiled, including a nonmatching test whose body has an error or unsupported reachable operation. Project execution invokes the runner, never the test root's conventional `main`.

### 7. Acceptance evidence stays at the cheapest adequate tier

Use structured compiler analysis for modifier rules, contracts, scope, identity, private access, phase violations, and residual requirements. Inspect residual TIR/MIR for ordinary callables and absence of descriptors; inspect authored encodings for fingerprint claims. Share analysis snapshots per fixture and use committed golden comparisons rather than fresh-process per-feature checks.

Extend the shared native acceptance corpus for sequential success/failure, continuation, local providers, and cleanup. Add focused CLI coverage to the existing workflow suite for root/filter transport, import-only roots, host/library composition, status, and distinct destinations. Test a separately named source runner through a compiler request to prove the public seam without adding custom-host CLI configuration. No broad test rerun or CI gate belongs in the implementation task list.

## Risks / Trade-offs

- Separate discovery and entry roots could accidentally merge candidate sets → track the root-specific active import closure and source ownership explicitly; exercise runner-only and dependency modules as negative cases.
- Private callable access could become general visibility bypass → only unforgeable descriptors for marked eligible declarations grant access; keep ordinary resolution unchanged.
- Whole-program discovery could create selection cycles → make it unavailable while configuration/module selection is being decided.
- Static expansion grows runner code linearly with test count → use existing residual budgets and source helpers; erased runtime registries are deferred until justified.
- Unrelated tests can block a filtered run at compilation → this is the agreed execution-only filter boundary and must be documented.
- Local provider/scheduler setup is repetitive → preserve exact requirement rows for later host composition rather than adding implicit provision now.
- A local HIR fingerprint can be mistaken for a safe result key → name/document it as authored content, include the helper-change counterexample, and perform no skipping.

## Migration Plan

Implement the grammar/metadata, discovery context and primitives, ordinary source runner, CLI composition, and documentation as one coherent feature. Existing tests, formatter/highlighting fixtures, generated catalogs, and compiler clients affected by the new authored-header field move together. Do not add a second analysis pipeline or compatibility adapters. Existing projects opt in by marking functions and invoking `silk test`; no manifest migration or persistent cache migration is required.
