## Why

The CLI currently exposes one low-level `compile <source>` operation, forcing users to supply source roots, output paths, and Clang details that belong to a project model. Silk needs a stable language-tool interface where ordinary workflows discover a project, share configuration, and hide compiler orchestration while direct-file compilation remains available explicitly.

## What Changes

- Add a minimal `silk.toml` project manifest with package name and root source configuration.
- Add upward manifest discovery from the current directory plus an explicit `--manifest-path` override.
- Add project-oriented `silk build`, `silk check`, and `silk run -- <args>` commands sharing one project-loading and build-planning seam.
- Add deterministic project artifact placement under `.silk/build/<target>/<profile>/<package>`.
- **BREAKING** Replace `silk compile <source>` with the explicit low-level `silk build-exe <source>` command.
- Keep source-root, output, Clang, temporary-artifact, and timing controls on `build-exe`; keep project commands focused on project, target, and profile selection.
- Forward `silk run` arguments and standard streams to the compiled executable while preserving its exit status.
- Document unsupported future workflows without registering misleading placeholder commands: testing, cleaning, project creation, formatting, documentation, package management, workspaces, caching, toolchain discovery, and machine-readable output.

## Capabilities

### New Capabilities

- `silk-project-manifest`: Project discovery, manifest decoding, validation, canonical paths, and deterministic artifact layout.
- `silk-cli-workflows`: Project build/check/run behavior, explicit direct-file compilation, shared command options, reporting, and process exit semantics.

### Modified Capabilities

None.

## Impact

- Reshapes `packages/cli` command modules and public subpath exports.
- Adds project and build-plan actors plus a thin executable-running boundary actor.
- Adds a small TOML decoding dependency and updates the workspace lockfile.
- Changes the executable command interface and CLI documentation; no backward compatibility is retained during alpha.
- Extends CLI integration and packed-release validation to cover project discovery and the new command surface.
