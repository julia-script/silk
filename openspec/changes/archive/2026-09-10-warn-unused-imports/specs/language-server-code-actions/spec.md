# Delta: language server code actions

## ADDED Requirements

### Requirement: Unused import warning and action

The server SHALL publish Warning-severity `LSP0004` on the exact authored name of each accepted unused import binding and SHALL expose an unresolved “Remove unused import” quick fix when the compiler supplies a current plan. Invalid, recovered, conflicting, and non-effective bindings and declarations owned by LSP0001-LSP0003 SHALL not receive an unused cascade. Redundancy ownership SHALL be declaration-local and MUST NOT hide an unrelated LSP0004 in the same document. Resolving the action SHALL reacquire the same binding and plan from the exact document version before returning an edit.

#### Scenario: Current action

- **WHEN** an unused-import action is resolved against the exact accepted document version and the same binding remains safely removable
- **THEN** the server returns the compiler-owned workspace edit titled “Remove unused import”

#### Scenario: Stale snapshot

- **WHEN** source changes after action discovery
- **THEN** the server disables the action and stale byte offsets are never returned as a workspace edit

#### Scenario: Unrelated redundancy

- **WHEN** one import is owned by LSP0001-LSP0003 and a separate declaration contains an unused valid binding
- **THEN** the server publishes the redundancy diagnostic and the unrelated LSP0004 without a cascade on the owned declaration
