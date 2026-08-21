## ADDED Requirements

### Requirement: Namespace completion plans one explicit namespace import

Selecting a catalog namespace completion SHALL produce one coherent source change that inserts the
chosen local namespace spelling and makes it available through an ordinary explicit namespace
import. When no equivalent import exists, the edit SHALL materialize `import <module> as <name>` in
the import region. An existing equivalent namespace binding SHALL be reused without duplication.
When the preferred namespace spelling collides with another source binding, the planner SHALL use
one deterministic legal alias and insert that alias at the completion site. It MUST NOT generate a
selected-member import for a namespace candidate or rewrite unrelated imports.

#### Scenario: Insert the Effect namespace import

- **WHEN** the author selects catalog namespace `Effect` from `silk/effect` and no import from that module exists
- **THEN** one edit plan inserts `import silk.effect as Effect` and replaces the partial spelling with `Effect`

#### Scenario: Reuse an equivalent namespace import

- **WHEN** the source already imports `silk.effect as Effect` and the matching namespace completion is selected
- **THEN** the completion inserts the available spelling without adding or changing an import

#### Scenario: Avoid a selected-member import

- **WHEN** the `Effect` namespace candidate is selected
- **THEN** the planner does not generate `import silk.effect { Effect }`

#### Scenario: Alias a colliding preferred namespace

- **WHEN** local binding `Effect` prevents that spelling from becoming a namespace binding
- **THEN** completion inserts one deterministic legal alias at both the import and completion site without shadowing the existing binding

#### Scenario: Preserve unrelated imports

- **WHEN** namespace completion adds a new catalog module import near recovered or differently shaped imports
- **THEN** the returned plan is non-overlapping and leaves every unrelated import and declaration unchanged
