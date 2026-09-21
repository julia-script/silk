# Spec Delta

## ADDED Requirements

### Requirement: Name resolution is addressed through semantic queries

Every migrated name read SHALL use one authoritative typed semantic request that returns the
existing resolved, missing, conflicting, or unavailable outcome together with anchored diagnostics.
The request SHALL observe selected bindings and every relevant examined candidate or absence,
including import selection and namespace membership.

#### Scenario: Resolve a selected imported binding
- **WHEN** a scope selects one public imported declaration for a requested name
- **THEN** the request returns the resolved canonical declaration and observes both the selection and the examined candidate set

#### Scenario: Preserve a missing result
- **WHEN** no visible candidate matches a requested name
- **THEN** the request returns the existing missing outcome and observes every namespace whose absence determines that answer

#### Scenario: Preserve a conflict
- **WHEN** multiple visible candidates conflict for a requested name
- **THEN** the request returns the existing conflicting outcome and observes the complete examined candidate set

#### Scenario: Distinguish query context
- **WHEN** the same spelling is requested from different scopes, profiles, or canonical substitution contexts
- **THEN** requests whose answers can differ do not share identity or completed results
