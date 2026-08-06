## 1. Project Manifest Foundation

- [x] 1.1 Add the TOML parser dependency, lockfile metadata, and package export entries required by the new public CLI actors.
- [x] 1.2 Add the `Project` actor with immutable project data, typed semantic and wrapped failure reasons, strict package/manifest validation, and exact relative path interpretation.
- [x] 1.3 Implement nearest-ancestor `silk.toml` discovery plus exact `--manifest-path` loading over Effect filesystem and path capabilities.
- [x] 1.4 Materialize the project entry through `SourceEntry` and add focused tests for minimal manifests, explicit source roots, nested discovery, nearest selection, explicit absence, malformed TOML, invalid names, and path violations.

## 2. Shared Project Planning

- [x] 2.1 Add the pure `BuildPlan` actor for target/profile selection, deterministic `.silk/build` destinations, bootstrap toolchain configuration, and foreign-run rejection.
- [x] 2.2 Add deterministic planning tests for host and explicit targets, profile changes, invalid targets, portable package names, and repeated plan equality.
- [x] 2.3 Add shared project CLI option decoding for manifest, target, `--profile`, and `--release`, including early conflicting-profile rejection.

## 3. Workflow Execution

- [x] 3.1 Extract reusable catalog construction and compiler-result rendering from the current command into a coherent workflow/reporting seam.
- [x] 3.2 Implement project `check` through recoverable `Analysis` with the filesystem resolver, including multi-file diagnostics and operational resolver failure classification without creating artifacts.
- [x] 3.3 Implement project `build` through the strict compiler driver, creating destination parents and preserving no-partial-artifact behavior and `0/1/2` exit classes.
- [x] 3.4 Add the `Program` child-process actor using Effect process capabilities, inherited streams, structured arguments, typed platform failure translation, and exact program exit propagation.
- [x] 3.5 Implement project `run` as host-only build followed by `Program.run`, with program arguments remaining distinct from compiler arguments.

## 4. Command Surface Reshape

- [x] 4.1 Rename and reshape the direct compiler command as `BuildExeCommand`, preserving direct source-root, output, target, profile, Clang, temporary-artifact, and timing controls.
- [x] 4.2 Add thin `BuildCommand`, `CheckCommand`, and `RunCommand` adapters over shared project options and workflow operations.
- [x] 4.3 Replace root command registration with `build`, `check`, `run`, and `build-exe`, remove `compile`, and update public namespace/subpath exports.
- [x] 4.4 Add command and integration coverage for help surface, project discovery, check without artifact, deterministic build output, run exit propagation, direct compilation, removed compile command, source rejection, and infrastructure failure.

## 5. Documentation and Future Shape

- [x] 5.1 Rewrite the CLI README around project-first usage, `silk.toml`, deterministic output, direct `build-exe`, profiles, targets, run arguments, and exit behavior.
- [x] 5.2 Add an explicit future-work section for test, clean, new/init, fmt, doc, targets, caching, workspaces, dependencies, machine output, language-server integration, and toolchain management without exposing placeholder commands.
- [x] 5.3 Add changesets for the breaking command interface, new manifest/project workflows, and dependency/export changes.
- [x] 5.4 Update packed-release command/export/dependency expectations and prove the installed CLI exposes the new root help and project workflow actors.

## 6. Verification

- [x] 6.1 Run focused compiler-CLI typechecks and tests throughout migration, including repeated deterministic project fixtures.
- [x] 6.2 Run `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test` in repository order and fix every introduced failure.
- [x] 6.3 Run `pnpm check`, `pnpm release:candidate`, strict OpenSpec validation, and `git diff --check` before handoff.
