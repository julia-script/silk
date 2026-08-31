# snippet-element Specification

## Purpose

An embeddable, framework-free custom element that displays Silk source with compiler-driven
syntax highlighting and, per snippet, opt-in language-server features: diagnostics, hover,
inlay hints, and editing.

## Requirements

### Requirement: Snippet element works in plain HTML

The snippet element SHALL be usable from a plain HTML page by loading one script and writing the
custom element tag with Silk source as its content. It SHALL NOT require React or any other UI
framework at runtime, and a page without JavaScript SHALL still show the snippet's source text.

#### Scenario: Embedding in a generated page

- **WHEN** a static HTML page loads the element script and contains the snippet tag wrapping Silk
  source
- **THEN** the element renders that source with Silk syntax highlighting without any framework on
  the page

#### Scenario: JavaScript disabled

- **WHEN** the same page is opened with JavaScript disabled
- **THEN** the snippet's source text is still visible as readable text

### Requirement: Semantic features are opt-in per snippet

Syntax highlighting SHALL always be on. Each semantic feature — diagnostics, hover, and inlay
hints — SHALL be enabled per snippet by a boolean attribute on the element and SHALL be off when
its attribute is absent. A snippet with no semantic attribute SHALL NOT compile its content at
all.

#### Scenario: Highlight-only snippet

- **WHEN** the element has no semantic feature attributes
- **THEN** the source is highlighted, the content is never compiled, and no diagnostics, hover, or
  inlay hints appear

#### Scenario: Selected features only

- **WHEN** the element enables diagnostics but not hover or inlay hints
- **THEN** diagnostics render and hovering the source produces no hover content and no inlay
  hints appear

### Requirement: Diagnostics match the doctest compile convention

When diagnostics are enabled the element SHALL compile the snippet content as one complete,
standalone Silk module — nothing prepended, nothing wrapped — and SHALL mark each reported
diagnostic at its source range with its code and message available to the reader. A snippet that
doctest accepts SHALL show no diagnostics; a snippet that doctest rejects SHALL show the same
failures.

#### Scenario: A valid example

- **WHEN** diagnostics are enabled on a snippet whose content compiles without diagnostics
- **THEN** no diagnostic marks render

#### Scenario: A failing example

- **WHEN** diagnostics are enabled on a snippet whose content produces a compiler diagnostic
- **THEN** the diagnostic's range is visibly marked and the reader can see its code and message

### Requirement: Hover presents language-server content safely

When hover is enabled, pointing at source SHALL show the language server's hover content for that
position, rendered from its CommonMark payload into a detached DOM node. Links in hover content
SHALL only be emitted for explicit `http:`, `https:`, and `mailto:` destinations after CommonMark
entity decoding and ASCII control-character removal. Relative, fragment, protocol-relative, and
all other destinations SHALL render as plain text. Raw HTML SHALL render literally rather than
creating authored elements. The renderer SHALL highlight fenced Silk code with the editor's
classes and SHALL NOT inject standalone CSS.

#### Scenario: Hovering a declaration

- **WHEN** hover is enabled and the pointer rests on an identifier the language server has hover
  content for
- **THEN** a tooltip shows that content with Silk code inside it highlighted

#### Scenario: Unsafe link scheme

- **WHEN** hover content contains a relative, fragment, protocol-relative, unknown-scheme, or
  encoded-control destination that does not normalize to an explicit allowed scheme
- **THEN** the tooltip shows the link text without a hyperlink

#### Scenario: Raw HTML

- **WHEN** hover content contains authored raw HTML
- **THEN** the tooltip shows that HTML literally and creates no authored element

### Requirement: Inlay hints render inline

When inlay hints are enabled the element SHALL display the language server's inlay hints inline at
their anchor positions, visually distinct from the authored source, and hint text SHALL NOT become
part of the snippet's selectable source content.

#### Scenario: Hints appear

- **WHEN** inlay hints are enabled on a snippet for which the language server reports hints
- **THEN** each hint renders at its position, styled distinctly from authored source

### Requirement: Snippets are read-only unless made editable

The element SHALL reject edits by default. When the editable attribute is present the reader SHALL
be able to edit the source, and every enabled semantic feature SHALL update from the edited
content.

#### Scenario: Read-only default

- **WHEN** a reader types into a snippet without the editable attribute
- **THEN** the content does not change

#### Scenario: Editing updates semantics

- **WHEN** a reader edits an editable snippet with diagnostics enabled, introducing an error
- **THEN** the new diagnostic appears for the edited content

### Requirement: Semantic work is deferred

The element SHALL NOT compile snippet content during page load. Compilation for enabled semantic
features SHALL begin only when the snippet becomes visible or the reader interacts with it, so a
page holding many snippets stays responsive.

#### Scenario: Many snippets below the fold

- **WHEN** a page contains many snippets with diagnostics enabled and the reader has not scrolled
- **THEN** snippets that have never been visible have not compiled their content

### Requirement: Styling is isolated and themable

The element's rendering SHALL NOT be affected by the host page's element styles, and the host page
SHALL be able to theme it through documented CSS custom properties. Without host theming the
element SHALL be legible in both light and dark color schemes.

#### Scenario: Host styles do not leak

- **WHEN** the host page styles `pre`, `code`, or `span` elements globally
- **THEN** the snippet's rendering is unchanged

#### Scenario: Default themes

- **WHEN** the element renders on a page with no snippet theming, in light and in dark color
  scheme
- **THEN** source, diagnostics, and tooltips are legible in both
