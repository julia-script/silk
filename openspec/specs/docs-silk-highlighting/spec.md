# docs-silk-highlighting Specification

## Purpose

Silk source shown in Markdown ` ```silk ` fences is syntax-highlighted in the documentation app.

## Requirements

### Requirement: Silk code fences highlight in docs pages

Markdown code fences labeled `silk` SHALL render with Silk syntax highlighting in both the docs
site's light and dark themes.

#### Scenario: A silk fence in a docs page

- **WHEN** a docs page contains a ` ```silk ` fence with `pub fn main() -> i32 { return 0 }`
- **THEN** the rendered page highlights `pub`, `fn`, and `return` as keywords in both themes

#### Scenario: Unknown fence languages still fall back

- **WHEN** a docs page contains a fence in a language Shiki has no grammar for (e.g. `ebnf`)
- **THEN** the page still renders that fence as plain text without failing the build
