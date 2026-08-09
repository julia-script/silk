## ADDED Requirements

### Requirement: Symbol hover includes complete authored documentation

When a declaration or resolved reference has attached documentation, hover SHALL render the
compiler-derived source-like signature followed by the complete parsed Markdown document, including
its examples. Definition and reference hover for the same declaration SHALL show equivalent
documentation. Hover SHALL resolve intra-document links best-effort and render unresolved links as
inline code without a diagnostic. Symbols without documentation SHALL retain signature-only hover.

#### Scenario: Hover a documented function definition

- **WHEN** the cursor hovers the declared name of a function preceded by `///` prose and an `Examples` section
- **THEN** hover contains the function signature, prose, and fenced example as Markdown

#### Scenario: Hover a documented function reference

- **WHEN** the cursor hovers a resolved reference to that function
- **THEN** hover contains documentation equivalent to the declaration hover

#### Scenario: Hover an undocumented symbol

- **WHEN** the cursor hovers a semantic symbol with no attached documentation
- **THEN** hover continues to contain its existing source-like presentation without an empty documentation section
