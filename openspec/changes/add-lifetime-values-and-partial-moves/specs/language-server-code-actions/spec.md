## ADDED Requirements

### Requirement: Make inferred lifetimes explicit uses a current compiler-owned edit

The server SHALL offer Make lifetimes explicit for supported elided declarations and signature type occurrences when compiler facts establish a complete semantics-preserving expansion. The action SHALL use the accepted document version, negotiated position encoding, stable nonconflicting binder names, the exact retained environment annotation and type/lifetime outlives predicates, and the requested target range. Resolving a stale action SHALL disable it rather than return stale byte offsets. The server MUST NOT infer a public lifetime relationship from function bodies or fabricate an edit for ambiguous or unavailable lifetime facts.

#### Scenario: Expand implicit field parameters

- **WHEN** a current code-action request targets a struct with two independently omitted borrowed field lifetimes
- **THEN** the edit adds distinct lifetime binders and annotates each field while preserving comments and canonical semantic identity

#### Scenario: Disable a stale lifetime expansion

- **WHEN** the document changes between discovering and resolving Make lifetimes explicit
- **THEN** the action is disabled and no stale workspace edit is returned

#### Scenario: Avoid guessing an ambiguous result

- **WHEN** a declaration has two borrowed inputs and an unresolved elided result relationship
- **THEN** no semantics-changing Make lifetimes explicit edit is invented from its return body
