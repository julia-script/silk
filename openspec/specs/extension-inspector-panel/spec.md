# extension-inspector-panel Specification

## Purpose

The Silk Inspector: a single webview panel in the VS Code extension that shows one compiler-phase
view at a time for the open project, backed by the language server's inspection requests.

## Requirements

### Requirement: One panel, one view at a time

The extension SHALL contribute a command that opens the Silk Inspector as a single webview panel
beside the active editor. The panel SHALL show exactly one view at a time and SHALL offer an
in-panel phase picker listing every registered view except the source view (the editor itself is
the source). Running the open command when the panel exists SHALL reveal the existing panel
rather than creating another.

#### Scenario: Open the inspector

- **WHEN** the user runs the Silk Inspector command with a `.silk` editor active
- **THEN** one panel opens showing a view of that document, and running the command again reveals the same panel

#### Scenario: Switch phases

- **WHEN** the user picks a different view in the phase picker
- **THEN** the panel replaces its content with the picked view for the same document

### Requirement: The panel follows the active editor

The panel SHALL re-root its view to the active `.silk` editor when editor focus changes, and
SHALL refresh its content when the language server reports that a newer analysis committed for
the document's project. When no `.silk` editor is active the panel SHALL keep showing its last
document rather than going blank.

#### Scenario: Focus another Silk file

- **WHEN** the user focuses a different `.silk` editor
- **THEN** the panel re-projects its current view for the newly focused document

#### Scenario: Edit refreshes the view

- **WHEN** the user edits the document and the server commits a new analysis
- **THEN** the panel refreshes to rows projected from the new analysis

### Requirement: Span cursor syncs both ways

The panel and the editor SHALL share a span cursor. Moving the editor selection SHALL tint the
rows whose module-qualified span equals or contains the selection; activating a row SHALL reveal
and select the row's span in the editor, opening the row's module document when it differs from
the active one.

#### Scenario: Editor selection tints rows

- **WHEN** the user places the editor cursor inside a construct
- **THEN** rows covering that construct are tinted in the panel

#### Scenario: Row activation navigates the editor

- **WHEN** the user activates a row whose span lives in another module
- **THEN** the extension opens that module's document and selects the span

### Requirement: The panel renders with editor theming

The panel SHALL derive its colors and typography from the editor's theme variables so it remains
legible in light, dark, and high-contrast themes without theme-specific styling.

#### Scenario: Theme change

- **WHEN** the user switches the editor color theme
- **THEN** the panel re-renders legibly without reload
