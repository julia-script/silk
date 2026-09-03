# Delta: semantic facts

## ADDED Requirements

### Requirement: Authored import binding use
The frontend SHALL classify each valid effective namespace and selected-member import by authored local binding identity. Import syntax, comments, and spelling alone SHALL NOT count as semantic use; aliases of one canonical declaration and shadowed locals SHALL remain independent.

#### Scenario: Alias is unused
- **WHEN** one canonical declaration is imported under two aliases and only one alias is referenced
- **THEN** only the unreferenced authored binding is unused
