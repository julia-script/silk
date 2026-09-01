## ADDED Requirements

### Requirement: Layout planning specializes reachable generic types

The target layout catalog and runtime plan SHALL compute physical facts for each reachable concrete
application of a generic nominal type from its normalized substituted fields. Open generic types
MUST NOT receive speculative physical layouts, and equivalent concrete applications SHALL reuse one
canonical layout entry before MIR and backend selection.

#### Scenario: Plan two concrete boxes

- **WHEN** runtime discovery reaches `Box<I32>` and `Box<Token>`
- **THEN** the selected target plan contains two canonical entries with independently derived concrete layouts

#### Scenario: Omit an open generic layout

- **WHEN** the compiler analyzes `Box<T>` without a concrete runtime instance
- **THEN** no physical layout is invented for the open type
