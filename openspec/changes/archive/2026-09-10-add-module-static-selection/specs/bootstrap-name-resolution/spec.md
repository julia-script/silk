## MODIFIED Requirements

### Requirement: Visibility governs cross-module member access

A declaration SHALL be visible within its defining module regardless of whether it is public. Only
an explicitly public declaration SHALL resolve through a namespace or selective import. Selecting
or qualifying an unknown member SHALL produce an unknown-member outcome; naming a private member
from another module SHALL produce a distinct inaccessible-member outcome retaining the private
declaration candidate and diagnostic cause. Ordinary imports MUST NOT re-export members. Explicit
`pub import module { member as alias }` SHALL publish only the selected public declarations,
preserving original identity and collision diagnostics. Imports MUST NOT activate unlisted methods,
operators, overloads, conformances, or runtime initialization.

#### Scenario: Call a private local function

- **WHEN** a module-local body calls a unique private function declared in the same module
- **THEN** the call resolves to that function's canonical declaration identity

#### Scenario: Reject a selected private function

- **WHEN** an import selects a private function from another module
- **THEN** the selected binding is inaccessible with its stable diagnostic and no callable member binding is created

#### Scenario: Reject a qualified private function

- **WHEN** `Tree.hidden()` names a private function through a valid namespace import
- **THEN** lookup reports the inaccessible canonical declaration without producing a resolved call

#### Scenario: Keep imports behavior-neutral

- **WHEN** a module imports another actor module
- **THEN** only its explicit namespace and selected-member bindings enter scope and no unlisted behavior or runtime action appears
