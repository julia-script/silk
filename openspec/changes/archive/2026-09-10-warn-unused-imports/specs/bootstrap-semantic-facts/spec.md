# Delta: semantic facts

## ADDED Requirements

### Requirement: Authored import binding use

The frontend SHALL classify each valid effective namespace and selected-member import by authored local binding identity. Each unqualified semantic occurrence SHALL retain the exact authored import binding that supplied lookup separately from its canonical navigation identity. Import syntax, comments, qualified access through another binding, and spelling alone SHALL NOT count as use; aliases of one canonical declaration, repeated bindings, receiver members, and shadowed locals SHALL remain independent. Recovered, unavailable, inaccessible, inherent-member, and conflicting imports SHALL NOT produce unused facts.

#### Scenario: Alias is unused

- **WHEN** one canonical declaration is imported under two aliases and only one alias is referenced
- **THEN** only the unreferenced authored binding is unused

#### Scenario: Qualified access does not use a direct selector

- **WHEN** one declaration is imported directly and through a namespace and only the qualified member is referenced
- **THEN** the namespace is used and the independent direct selector is unused

#### Scenario: Conformance-only use

- **WHEN** an import appears only in a conformance capability, provider, conditional requirement, mapped target, or hook contract
- **THEN** its semantic occurrence retains the authored binding and the import is used

#### Scenario: Recovered import syntax

- **WHEN** parser recovery leaves an import declaration unavailable
- **THEN** none of that declaration's partially resolved bindings is classified as unused
