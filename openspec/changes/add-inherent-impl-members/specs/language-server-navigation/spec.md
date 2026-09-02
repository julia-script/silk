## ADDED Requirements

### Requirement: Associated members have one semantic identity

Definition, references, and rename SHALL treat an inherent member as one semantic identity across
its declaration, its `Owner.member` call form, its function-item and section forms, and its applied
`Owner<Args>.member` form. Definition from any of those occurrences SHALL land on the member's
declared name inside its impl block, and rename SHALL rewrite every occurrence in one workspace
edit while leaving same-named members of other owners and same-named root functions untouched.

#### Scenario: Navigate from a section to the member

- **WHEN** the cursor is on `map` in `value |> Option.map(addOne)`
- **THEN** definition navigates to `map` inside `impl<T> Option<T>`

#### Scenario: Rename a member across call forms

- **WHEN** `Option.map` is renamed to `transform`
- **THEN** the declaration, every `Option.map(...)` call, every section, and every applied-qualifier use are rewritten, and a root function named `map` in another module is untouched
