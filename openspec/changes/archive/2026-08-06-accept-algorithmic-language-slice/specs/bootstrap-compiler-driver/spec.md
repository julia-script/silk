## ADDED Requirements

### Requirement: One compiler-shaped algorithm accepts the algorithmic language slice

The continuous compiler acceptance suite SHALL compile one unchanged multi-module Silk program that
combines cross-module declarations and calls, nominal structs, fixed arrays, checked indexing,
operators, mutable bindings, structured loops, normalized structural unions, guarded exhaustive
matching, and target-aware aggregate layout. Logical evaluation, native execution, and direct
WebAssembly execution MUST all complete with the same pinned result.

#### Scenario: Run the canonical remaining-member fold

- **WHEN** the acceptance suite compiles and runs the fixed-input remaining-member coverage fold
- **THEN** logical evaluation, native execution, and direct WebAssembly execution all return `42`

### Requirement: Composed acceptance artifacts are deterministic

The compiler SHALL retain deterministic source closure, semantic, HIR, ownership, instance, layout,
MIR, evaluation, native, and WebAssembly artifacts for the compiler-shaped acceptance program.

#### Scenario: Repeat the acceptance program in a fresh process

- **WHEN** equivalent acceptance module maps are compiled repeatedly in fresh processes
- **THEN** every compiler-owned encoding, evaluation trace, symbol set, target text, and binary hash agrees exactly
