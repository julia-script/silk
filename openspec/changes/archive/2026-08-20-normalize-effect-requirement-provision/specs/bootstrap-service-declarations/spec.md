## ADDED Requirements

### Requirement: Roles are nominal compile-time declarations

`role Name` SHALL declare an ordinary nominal compile-time identity that MAY qualify a service
requirement as `Service at Name`. Roles SHALL participate in normal visibility, import, semantic
fact, navigation, completion, formatting, and deterministic module-surface behavior. A role SHALL
not create a provider, service conformance, runtime value, runtime slot, or access mode.

#### Scenario: Inspect a role declaration

- **WHEN** a source module declares `pub role Primary` and another module imports it
- **THEN** tooling resolves and navigates `Clock at Primary` to that declaration using ordinary visibility

#### Scenario: Reject a non-role qualifier

- **WHEN** `Clock at Value` names a visible declaration that is not a role
- **THEN** semantic analysis rejects the requirement entry at the qualifier
