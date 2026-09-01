## ADDED Requirements

### Requirement: HIR represents canonical union conversion explicitly

HIR SHALL carry normalized union types as canonical nominal member sets and represent each accepted
injection or widening as one typed conversion around its source expression. The conversion SHALL
carry the exact source type, target union, canonical total member mapping, access mode, and
provenance. It MUST NOT encode numeric runtime tags, backend storage, pattern narrowing, or cyclic
control edges.

#### Scenario: Elaborate a nominal injection

- **WHEN** a `Token` expression enters a declared `Token | End` return context
- **THEN** HIR contains one conversion from precise `Token` to the canonical two-member union

#### Scenario: Elaborate union widening inside a loop

- **WHEN** a mutable `Token | End` binding is assigned into a `Token | End | Fault` destination inside a loop
- **THEN** the write source contains one canonical widening operation and the surrounding HIR region graph remains acyclic
