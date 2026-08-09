## ADDED Requirements

### Requirement: Constants resolve through explicit value scopes

Constant references SHALL use the existing local declaration, selective import, namespace alias,
visibility, collision, and cycle rules. Only public constants SHALL resolve across a module
boundary, and a constant in type or callable position SHALL produce a kind mismatch rather than an
alternate lookup.

#### Scenario: Resolve local, selected, and qualified constants

- **WHEN** source uses one local constant, selectively imports a public constant, and qualifies another through a namespace alias
- **THEN** all uses resolve to their exact canonical constant declarations

#### Scenario: Refuse a private imported constant

- **WHEN** another module selects or qualifies a private constant
- **THEN** lookup retains the inaccessible candidate and reports the existing visibility outcome without exposing a usable value
