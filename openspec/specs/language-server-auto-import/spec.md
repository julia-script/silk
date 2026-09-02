# language-server-auto-import Specification

## Purpose

Defines how the Silk language server discovers importable declarations and offers coherent,
deterministic quick fixes that add the selected declaration to the requesting module's imports.

## Requirements

### Requirement: Unresolved references offer applicable auto-import candidates

When a code-action request covers an unresolved source name, the language server SHALL search the
current project's source-root modules and shipped toolchain modules for public declarations with
the exact case-sensitive spelling and a semantic kind applicable at that reference. The server
SHALL exclude candidates already available in the requesting scope, candidates that cannot be
named from the requesting module, private declarations, and candidates whose import would create a
binding collision. Every remaining exporting module SHALL produce a distinct `quickfix` action
whose title identifies both the declaration and its module.

#### Scenario: Import from a closed project module

- **WHEN** a function reference is unresolved and one closed source-root module publicly exports a function with that exact name
- **THEN** the server offers one quick fix identifying that module and importing the function

#### Scenario: Import a shipped declaration

- **WHEN** a type reference is unresolved and one shipped toolchain module publicly exports that type
- **THEN** the server offers one quick fix identifying the toolchain module and importing the type

#### Scenario: Keep ambiguous modules explicit

- **WHEN** two importable modules publicly export applicable declarations with the unresolved spelling
- **THEN** the server returns one separately titled action per module rather than selecting a module without the author's choice

#### Scenario: Exclude the wrong semantic kind

- **WHEN** an unresolved type position shares its spelling with a public function and a public nominal type
- **THEN** the server offers the nominal type candidate and does not offer the function candidate

#### Scenario: Exclude unavailable candidates

- **WHEN** the only declaration with the unresolved spelling is private, already available under that spelling, or would collide with another binding
- **THEN** the server offers no auto-import action for that declaration

### Requirement: Auto-import plans one coherent import edit

Resolving an auto-import action SHALL produce one atomic source change that makes the selected
declaration available under the unresolved spelling. The change SHALL extend a compatible existing
import from the same module when one exists and otherwise insert a selected-member import in the
module's import region. It MUST NOT duplicate an existing binding, remove an existing import,
rewrite an explicit alias, or alter source outside the import change required by the action.

#### Scenario: Extend an existing selected-member import

- **WHEN** the requesting module already imports selected members from the chosen module
- **THEN** the action adds the selected declaration to that import while preserving its existing members and aliases

#### Scenario: Preserve a namespace alias

- **WHEN** the requesting module imports the chosen module under a namespace alias but does not import the unresolved member directly
- **THEN** the action adds the selected-member binding without rewriting or removing the namespace alias

#### Scenario: Insert a new import

- **WHEN** no import from the chosen module exists
- **THEN** the action inserts one selected-member import in the existing import region and leaves declarations and unrelated trivia unchanged

#### Scenario: Apply one atomic edit plan

- **WHEN** making the selected declaration available requires both inserting an import declaration and preserving an existing recovered import nearby
- **THEN** the action either returns one coherent non-overlapping change plan or is withheld, and never returns a partial correction

### Requirement: Auto-import observes one accepted workspace revision

Candidate discovery, semantic applicability, module source, and import edit planning SHALL observe
one accepted workspace revision. Synchronized buffers SHALL take precedence over the corresponding
disk files. Creating, changing, deleting, or renaming a closed source-root file SHALL refresh later
auto-import results for affected open documents without requiring the candidate file to be opened.
An action resolved after its source revision is superseded MUST NOT apply offsets or candidate facts
from the older revision to the newer document.

#### Scenario: Unsaved export becomes available

- **WHEN** an open module adds a public declaration in an unsaved editor buffer and that workspace revision is accepted
- **THEN** another open document can receive an auto-import action for the buffered declaration without the file being saved

#### Scenario: Closed candidate changes on disk

- **WHEN** a closed source-root module removes or renames a public declaration on disk
- **THEN** later code-action requests stop offering the removed spelling after the affected project refreshes

#### Scenario: Reject a stale resolved action

- **WHEN** an auto-import action is requested for one document revision and resolved after a newer revision supersedes it
- **THEN** the server recomputes the action against the current accepted revision or returns no edit rather than applying stale ranges

### Requirement: Candidate discovery does not widen semantic project roots

Maintaining and querying auto-import candidates SHALL NOT make every source-root file an accepted
semantic project root. Ordinary synchronized project analysis SHALL remain rooted at open documents
and their transitive import closure; candidate modules outside that closure SHALL contribute only
the declaration and module-summary facts required for discovery until an import makes them
reachable.

#### Scenario: Query a large source root

- **WHEN** one open document requests an auto-import from a source root containing unrelated closed modules
- **THEN** the server can discover their public exports without semantically elaborating every closed module as an open project root

#### Scenario: Import the chosen module

- **WHEN** the author applies an auto-import and the chosen module enters the open document's import closure
- **THEN** the next accepted semantic project revision analyzes that module through the ordinary module-closure rules

### Requirement: Auto-import ordering is deterministic

For one accepted revision and request context, candidate filtering, titles, and ordering SHALL be
deterministic. Candidates that extend an already present module import SHALL precede candidates
requiring a new module import; project modules SHALL precede shipped toolchain modules; remaining
ties SHALL use canonical module identity and declaration order.

#### Scenario: Repeat an ambiguous request

- **WHEN** the same unresolved reference receives repeated code-action requests against one accepted revision
- **THEN** the server returns byte-identical titles and the same candidate order

#### Scenario: Prefer an existing module import

- **WHEN** two modules export the unresolved spelling and one of them is already imported by the requesting module
- **THEN** the action extending that module import appears before the action introducing a new module import

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
