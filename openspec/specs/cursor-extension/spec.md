# cursor-extension Specification

## Purpose

A private, declarative VS Code-compatible extension registers the Silk language so `.silk` files
highlight in Cursor via a local symlink install — no marketplace, no packaging.

## Requirements

### Requirement: Silk files are recognized and highlighted

The extension SHALL declare the `silk` language for the `.silk` file extension and contribute the
Silk TextMate grammar and language configuration, so an editor loading the extension highlights
`.silk` files and applies Silk comment/bracket behavior.

#### Scenario: Opening a Silk file

- **WHEN** the extension is installed and the user opens a `.silk` file in Cursor
- **THEN** the file is identified as Silk and keywords, comments, and numbers are highlighted

#### Scenario: Grammar matches the shared grammar

- **WHEN** the extension is built or installed
- **THEN** the grammar and language configuration it contributes are the ones exported by the
  language package, not an independent copy that can drift

### Requirement: Local-only install, excluded from release machinery

The extension SHALL be a private workspace package installable by symlinking its folder into the
editor's extensions directory, and SHALL be excluded from Changesets and release-candidate
validation.

#### Scenario: Symlink install

- **WHEN** the user symlinks the extension folder into `~/.cursor/extensions/` and reloads Cursor
- **THEN** the extension loads without any packaging or marketplace step

#### Scenario: Release tooling ignores the extension

- **WHEN** release-candidate validation runs
- **THEN** the private extension package is not packed or validated for publication
