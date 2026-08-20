## ADDED Requirements

### Requirement: Pattern-local navigation follows resolved semantic identity

Definition, references, and rename SHALL treat each shared pattern binding as one compiler-owned
semantic identity. A use in a match arm, after an irrefutable let, or inside a taken if-let body
SHALL navigate to the exact binding token; unavailable and out-of-scope uses SHALL invent no target.

#### Scenario: Navigate a local destructuring binding

- **WHEN** definition is requested on a later use of a field-shorthand binding
- **THEN** the target selection range is the shorthand token that declared the local
