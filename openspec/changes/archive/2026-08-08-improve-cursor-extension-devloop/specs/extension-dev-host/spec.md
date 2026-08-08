## Purpose

Workspace launch and task definitions that open a VS Code-compatible Extension Development Host against the Silk extension package so contributors iterate without installing into the global extensions directory.

## ADDED Requirements

### Requirement: Extension Development Host launches the Silk extension from the workspace

The repository SHALL provide a debug launch configuration that starts an Extension Development Host with the Silk extension loaded from the workspace `packages/vscode` path (not from `~/.cursor/extensions` or `~/.vscode/extensions`).

#### Scenario: Launch from the open checkout

- **WHEN** a contributor runs the Silk Extension Development Host launch configuration from the repository workspace
- **THEN** the host window loads the Silk language contribution from that workspace's `packages/vscode` folder

#### Scenario: Guest host is independent of global install

- **WHEN** the global Cursor/VS Code extensions directory has no Silk extension, or points at a different checkout
- **THEN** the Extension Development Host still highlights `.silk` files using the workspace extension

### Requirement: Pre-launch build keeps host binaries current

Launching the Extension Development Host SHALL ensure the extension and its language-server dependency are built so the host activates against current `dist` output rather than a missing or stale binary.

#### Scenario: Fresh checkout launch

- **WHEN** a contributor launches the Extension Development Host after a clean checkout that has been installed but not yet built for the extension packages
- **THEN** the pre-launch build produces the extension entrypoint and the language-server binary the extension resolves before the host window opens

### Requirement: Watch tasks support iterative extension and server rebuilds

The repository SHALL provide watch tasks that rebuild the extension and language-server packages on change so a contributor can rebuild without leaving the editor, then restart the language server or reload the host window as appropriate.

#### Scenario: Server code change without window reload

- **WHEN** a contributor changes language-server sources while watch rebuild is running, the rebuild finishes, and they run `Silk: Restart Language Server` in the Extension Development Host
- **THEN** the restarted server loads the rebuilt binary without requiring a full host window reload

#### Scenario: Contribution or grammar change needs reload

- **WHEN** a contributor changes extension activation code or the contributed TextMate grammar files
- **THEN** documentation and tasks make clear that a host window reload is required for those changes to take effect
