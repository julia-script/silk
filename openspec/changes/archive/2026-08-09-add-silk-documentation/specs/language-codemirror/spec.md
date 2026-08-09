## ADDED Requirements

### Requirement: CodeMirror highlights documentation structure lazily

The CodeMirror integration SHALL distinguish `///` declaration documentation and `//!` module
documentation and SHALL expose stable highlight ranges for their markers, Markdown headings,
emphasis, inline code, links, and fenced code. Silk code inside a fenced `silk` block SHALL receive
nested Silk highlighting. Documentation highlighting SHALL execute only in the editor integration
and SHALL NOT change compiler token classification or ordinary analysis cost.

#### Scenario: Highlight a documented example

- **WHEN** the editor contains a `///` block with an `Examples` heading, intra-document link, and fenced `silk` example
- **THEN** the marker, heading, link, fence, and nested Silk tokens receive stable distinct highlight ranges

#### Scenario: Highlight module documentation

- **WHEN** the editor begins with `//!` module prose
- **THEN** its marker and Markdown content are distinguished from ordinary and declaration documentation comments
