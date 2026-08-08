## Context

See `proposal.md` for motivation and the delta specs for observable behavior. The current CLI has one `CompileCommand.run` operation that loads a source entry, constructs filesystem resolution, drives compilation, renders reports, and owns every flag. The completed resolver change makes a project seam possible without changing compiler resolution semantics. The repository uses Effect platform services, one actor per module, typed errors, explicit package subpaths, and application-edge layer composition.

## Goals / Non-Goals

**Goals:**

- Make project discovery and planning one deep module used identically by build, check, and run.
- Keep command modules thin and keep compiler/reporting orchestration reusable outside argument parsing.
- Preserve a focused direct-file operation without leaking those controls into project commands.
- Use Effect filesystem, path, and child-process capabilities behind typed boundaries.
- Make the first manifest deliberately small while leaving room for later targets and dependencies.

**Non-Goals:**

- Package download, dependency resolution, lockfiles, workspaces, or registries.
- Incremental compilation, persistent artifact caches, parallel build graphs, or custom build steps.
- Implementing test, clean, new/init, fmt, doc, targets, or language-server commands.
- Toolchain installation or discovery; project commands use the current bootstrap `clang` default internally.
- Stabilizing the manifest format during alpha.

## Decisions

### One project actor owns discovery, decoding, and source entry materialization

`Project.ts` will define immutable project data, a typed `ProjectError` with semantic reasons, `discover`, and `load`. `load` accepts an optional manifest path and working directory, searches upward only when no path is supplied, decodes TOML, validates package/path invariants, and delegates exact entry reading to `SourceEntry`. Callers receive canonical manifest directory, entry, source root, and root module identity rather than repeating path rules.

Alternative: leave discovery in each command and share flag helpers. Rejected because deletion would merely scatter manifest and path complexity across every workflow.

### A pure build plan hides artifact-layout and target-selection policy

`BuildPlan.ts` will take a loaded project plus target/profile overrides and return either an immutable plan or typed planning failure. The plan contains the resolved target, profile, destination, and toolchain inputs required by compiler orchestration. It is the only place that knows `.silk/build/<target>/<profile>/<package>`.

Alternative: have build and run each construct destination paths. Rejected because output policy would immediately diverge and tests would target command internals instead of the shared seam.

### Workflow execution is separate from CLI argument adapters

`Workflow.ts` will expose named Effect operations for `check`, `build`, and `run` configuration. It will own resolver provisioning, driver/facade invocation, reporting, exit classification, artifact-directory creation, and successful executable execution. `BuildCommand`, `CheckCommand`, and `RunCommand` only translate CLI values into workflow configuration and convert a returned status into the runtime exit marker.

Alternative: preserve one large command module and parameterize its mode. Rejected because mutually irrelevant flags and branches would remain one shallow interface.

### Direct-file compilation is renamed, not wrapped for compatibility

The existing compile workflow becomes the `BuildExeCommand` actor and remains explicit about entry, source root, output, target, profile, Clang, temporary artifacts, and timings. `compile` is removed rather than retained as an alias because the repository explicitly forbids alpha compatibility debt.

### TOML decoding uses one external boundary and runtime validation

A small TOML dependency will parse text inside `Effect.try`; its unknown output will be decoded through an explicit validator before becoming project data. Parse failures, invalid shapes, invalid names, and path violations become `ProjectError` reasons, while Effect filesystem failures retain causal ancestry through the same project abstraction.

Alternative: implement a partial TOML parser locally. Rejected because parsing edge cases are unrelated complexity. JSON was considered but rejected because the accepted language-tool shape specifically establishes `silk.toml` and future configuration benefits from TOML's human-authored form.

### Run uses Effect's child-process module with inherited streams

`Program.ts` will be the executable process actor. It will construct a child process with argument arrays and inherited stdin/stdout/stderr, then return the branded numeric exit code. Platform process failures translate to a typed `ProgramError`; shell strings are never constructed.

Alternative: raw `node:child_process`. Rejected because Effect already provides a scoped, replaceable child-process capability through the Node platform layer.

### Unsupported language-tool features are documentation, not dead commands

The README will include a future-work table describing intended semantics and dependencies for test, clean, creation, formatting, documentation, targets, caching, workspaces, packages, machine output, and toolchain management. No command is registered until it can perform its contract.

## Risks / Trade-offs

- [The initial manifest may evolve quickly] → Keep it minimal, validate strictly, and rely on the repository's explicit alpha breaking-change policy.
- [A TOML dependency expands the CLI package] → Use one small parser at one boundary and verify packed contents and dependency metadata.
- [Project build still relies on the bootstrap compiler's synchronous native toolchain] → Hide that detail behind workflow execution now; migrating the compiler toolchain boundary is separate work.
- [Forwarding a program exit code can overlap compiler-reserved codes] → Reserved `0/1/2` meanings apply only before execution; after a successful build, `run` transparently returns the program status.
- [No cache means repeated check/build work] → Keep deterministic layout now and document persistent caching as future work rather than inventing an unstable cache key.

## Migration Plan

1. Add project, plan, program, and workflow actors with focused tests.
2. Add project commands and migrate the existing direct workflow to `build-exe`.
3. Remove `compile` registration and update exports, README, changesets, and packed-consumer expectations.
4. Run focused CLI verification, repository checks, and release-candidate validation.

Rollback is a normal source revert; no persistent state beyond generated `.silk/build` artifacts or external data migration is involved.
