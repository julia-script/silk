## ADDED Requirements

### Requirement: TextMate tokenization covers generic contexts

TextMate and generated VS Code grammars SHALL tokenize generic declaration parameters and
applications without reclassifying comparison operators or reserved template starts.

#### Scenario: Tokenize mixed angle contexts

- **WHEN** a document contains a generic declaration, an explicit specialization, a comparison, and a reserved template start
- **THEN** generated grammar fixtures assign stable context-appropriate scopes to all four forms
