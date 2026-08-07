## ADDED Requirements

### Requirement: Driver acceptance covers Effect and owned allocation vertically

The compiler corpus SHALL cover Effect construction versus execution, capture modes, catch, retry,
provider placement, Layout validation, allocation success and exhaustion, partial initialization,
Vector growth, explicit drop, typed-failure cleanup, and trap separation across evaluator, native,
and Wasm where valid. Fresh runs SHALL preserve every textual and binary artifact deterministically.

#### Scenario: Compile the owned-token milestone

- **WHEN** a compiler-shaped program tokenizes borrowed runtime bytes into a growable owned Vector and returns it through an Effect
- **THEN** evaluation, native, and Wasm agree on tokens, ownership, allocation failures, cleanup, target layout, and emitted artifacts
