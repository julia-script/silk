## ADDED Requirements

### Requirement: HIR retains Effect allocation and Drop semantics

HIR SHALL retain effect construction, capture mode, one-layer execution, retry boundaries, provider
capture or per-run acquisition, typed failure, validated allocation, raw-buffer initializedness,
explicit drop, and automatic Drop. It MUST NOT contain named scopes, allocator implementation kinds,
LLVM types, Wasm values, or dynamic finalizer records.

Each Effect construction SHALL retain its hidden source-site identity and ordered capture fields.
Calls and returns MUST preserve that identity rather than reducing the value to an Effect outcome.

#### Scenario: Inspect Vector growth rollback

- **WHEN** a generic Vector append may fail while moving initialized elements to a replacement buffer
- **THEN** HIR retains allocation, prefix ownership, commit, rollback, and cleanup provenance without a collection-shaped compiler intrinsic
