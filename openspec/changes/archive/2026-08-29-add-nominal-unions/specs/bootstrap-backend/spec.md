## ADDED Requirements

### Requirement: Backends realize verified nominal union layouts

Native LLVM and direct WebAssembly backends SHALL consume the compiler-owned nominal-union layout
and MIR decisions to emit construction, transport, tag dispatch, payload access, and active cleanup.
Backends MUST NOT choose variant order, tag identity, payload layout, hierarchical coverage, or
cleanup policy independently and MUST produce behavior equivalent to evaluation.

#### Scenario: Emit one mixed nominal union

- **WHEN** verified MIR passes and returns a union containing unit and aligned payload variants
- **THEN** both backends use the planned calling shape and produce the same selected variant and field values as evaluation

#### Scenario: Dispatch a direct nested variant arm

- **WHEN** verified MIR matches `HttpError.Dns` through an outer `HttpError | OutOfMemoryError`
- **THEN** both backends realize the complete outer and inner decision path without exposing either numeric tag

#### Scenario: Release only the active payload

- **WHEN** a union with distinct cleanup-bearing variants is dropped on each structured cleanup-bearing exit
- **THEN** native and Wasm release exactly the selected variant's fields once, agree with evaluation, and perform no unwind cleanup for a fatal trap
