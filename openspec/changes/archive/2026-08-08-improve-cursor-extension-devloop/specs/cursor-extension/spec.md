## ADDED Requirements

### Requirement: Retargetable local install for the current checkout

The extension package SHALL provide a documented install command that creates or replaces the local Cursor (and optionally VS Code) extensions-directory symlink so it points at this repository checkout's `apps/vscode` path, after ensuring the extension and language-server packages are built.

#### Scenario: Install from a worktree

- **WHEN** a contributor runs the install command from a git worktree whose absolute path differs from any previous install
- **THEN** the extensions-directory entry for Silk resolves to that worktree's `apps/vscode` folder and the extension entrypoint exists on disk

#### Scenario: Replacing a dangling symlink

- **WHEN** an existing Silk extensions-directory symlink points at a missing path
- **THEN** running the install command replaces it with a symlink to the current checkout and leaves a loadable extension

#### Scenario: Reload after install

- **WHEN** the install command completes successfully
- **THEN** documentation instructs the contributor to reload the editor window so contributions are picked up from the retargeted path

### Requirement: Reload versus language-server restart is documented

The extension README SHALL distinguish window reload (needed for contribution and grammar changes, and after retargeting the install) from `Silk: Restart Language Server` (sufficient after rebuilding the language-server binary when the extension path is already correct).

#### Scenario: Contributor updates only the language server

- **WHEN** a contributor rebuilds `@silklang/lsp` while the extension is already loaded from the correct checkout
- **THEN** documentation tells them to run `Silk: Restart Language Server` rather than reinstalling the symlink

## MODIFIED Requirements

### Requirement: Local-only install, excluded from release machinery

The extension SHALL be a private workspace package installable by running the package's local install command (which symlinks the extension folder into the editor's extensions directory for the current checkout), and SHALL be excluded from Changesets and release-candidate validation. Marketplace packaging remains out of scope.

#### Scenario: Symlink install

- **WHEN** the user runs the documented local install command and reloads Cursor
- **THEN** the extension loads from the current checkout without any packaging or marketplace step

#### Scenario: Release tooling ignores the extension

- **WHEN** release-candidate validation runs
- **THEN** the private extension package is not packed or validated for publication
