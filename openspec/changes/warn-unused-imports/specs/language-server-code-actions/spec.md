# Delta: language server code actions

## ADDED Requirements

### Requirement: Unused import warning and action
The server SHALL publish Warning-severity `LSP0004` on each accepted unused import binding and SHALL expose “Remove unused import” when the compiler supplies a current plan. Invalid/conflicting bindings and declarations owned by LSP0001-LSP0003 SHALL not receive an unused cascade.

#### Scenario: Stale snapshot
- **WHEN** source changes after action discovery
- **THEN** stale byte offsets are never applied
