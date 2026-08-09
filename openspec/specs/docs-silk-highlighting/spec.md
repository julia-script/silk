# docs-silk-highlighting Specification

## Purpose

Silk source shown anywhere in the docs app is syntax-highlighted: the labs workbench edits Silk in
a real code editor, and Markdown ` ```silk ` fences render highlighted.

## Requirements

### Requirement: Workbench source pane is a highlighting editor

The labs workbench source pane SHALL edit Silk in a syntax-highlighting code editor while
preserving the pane's existing observable behavior: edits update the active module's source, the
source round-trips through the URL, and selecting a nonempty text range moves the shared span
cursor to the selected byte range.

#### Scenario: Editing highlighted source

- **WHEN** the user types Silk source containing keywords into the workbench source pane
- **THEN** keywords are visibly highlighted and downstream phase panes update from the edited
  source exactly as they did with the previous editor

#### Scenario: Selection drives the span cursor

- **WHEN** the user selects a nonempty range of source text in the editor
- **THEN** the shared span cursor moves to that range and downstream panes light up the
  corresponding spans

#### Scenario: Source survives the URL round trip

- **WHEN** the user edits source and reloads the resulting workbench URL
- **THEN** the editor shows the same source that was encoded in the URL

### Requirement: Silk code fences highlight in docs pages

Markdown code fences labeled `silk` SHALL render with Silk syntax highlighting in both the docs
site's light and dark themes.

#### Scenario: A silk fence in a docs page

- **WHEN** a docs page contains a ` ```silk ` fence with `pub fn main() -> i32 { return 0 }`
- **THEN** the rendered page highlights `pub`, `fn`, and `return` as keywords in both themes

#### Scenario: Unknown fence languages still fall back

- **WHEN** a docs page contains a fence in a language Shiki has no grammar for (e.g. `ebnf`)
- **THEN** the page still renders that fence as plain text without failing the build
