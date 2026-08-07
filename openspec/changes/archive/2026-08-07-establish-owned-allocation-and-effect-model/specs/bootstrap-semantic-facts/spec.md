## ADDED Requirements

### Requirement: Semantic facts expose Effect and owned allocation

Semantic analysis SHALL publish canonical Effect success/failure/requirement contracts, capture
access and repeatability, validated layouts, allocation and raw-buffer types, initialization
transitions, Drop restrictions, explicit drop consumption, and typed `OutOfMemory`. It MUST NOT
publish named allocation scopes or allocator-kind facts.

#### Scenario: Inspect a repeatable allocating Effect

- **WHEN** an effect function appends to a Vector through an explicit allocator requirement
- **THEN** facts expose its Effect contract, exclusive allocator access, repeatability, self-contained result ownership, and possible OutOfMemory without a retained-provider dependency
