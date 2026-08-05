## MODIFIED Requirements

### Requirement: Explicit logical source identity

Every source file SHALL carry an explicit, copyable logical identity supplied by the caller. Source
identity MUST NOT be inferred from a current directory, filesystem traversal, object identity, or
the contents of the file. The `SyntaxFile` artifact produced from a source SHALL carry exactly this
identity, and every stable syntax element identity SHALL be qualified by it.

#### Scenario: Equal bytes with different identities

- **WHEN** two source files contain equal bytes but have different caller-supplied identities
- **THEN** they remain distinct source files for span ownership and diagnostics

#### Scenario: Qualify syntax identities by source identity

- **WHEN** two source files with equal bytes but different identities are parsed
- **THEN** their artifacts' element identities are distinct because each is qualified by its own source identity
