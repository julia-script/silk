## ADDED Requirements

### Requirement: Raw documentation attachment is a facade query

The analysis facade SHALL expose a module's raw module documentation and the raw documentation
attached to canonical declarations and declaration-owned children. Tooling MUST NOT reconstruct
attachment by walking syntax trivia independently. The query SHALL return raw source-owned blocks
without parsing Markdown and SHALL preserve availability of unrelated facts around damaged syntax.

#### Scenario: Query documentation through a resolved reference

- **WHEN** a semantic occurrence resolves to a function declared in another loaded module
- **THEN** the facade can return that canonical declaration's raw documentation block from the owning syntax file

#### Scenario: Query an undocumented declaration

- **WHEN** a declaration has no attached documentation block
- **THEN** the facade reports documentation as absent without affecting the declaration's other semantic facts
