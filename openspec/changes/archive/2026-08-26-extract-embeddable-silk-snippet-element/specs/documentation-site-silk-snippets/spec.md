# documentation-site-silk-snippets Delta

## Purpose

The static documentation site embeds Silk code fences as live snippet elements, with per-fence
feature flags derived from fence attributes, while remaining a self-contained plain-HTML site.

## ADDED Requirements

### Requirement: Silk fences render as snippet elements

The generated site SHALL render each `silk` code fence as a snippet element carrying the fence's
source text, with diagnostics and hover enabled by default, and SHALL keep rendering fences in
other languages exactly as before.

#### Scenario: A silk fence

- **WHEN** a documented declaration carries a ` ```silk ` example fence
- **THEN** its generated page renders that example as a snippet element with diagnostics and hover
  enabled

#### Scenario: A non-silk fence

- **WHEN** documentation contains a fence in another language
- **THEN** the generated page renders it as escaped preformatted text, unchanged from today

### Requirement: Ignored fences degrade to highlight-only

A fence marked with the ignore attribute (`silk,ignore`) SHALL render as a highlight-only snippet
element: no semantic feature is enabled and its content is never compiled in the reader's browser.

#### Scenario: An ignore fence

- **WHEN** a documented declaration carries a ` ```silk,ignore ` fence whose content does not
  compile
- **THEN** the generated page shows it highlighted with no diagnostic marks

### Requirement: Generated sites stay self-contained

The site generator SHALL ship the snippet element script with the generated output and reference
it relatively, so a generated site works from local files or any static host without fetching
resources from other origins.

#### Scenario: Serving from a static host

- **WHEN** a generated site directory is served as static files with no other origin reachable
- **THEN** snippet elements load, highlight, and run their enabled semantic features
