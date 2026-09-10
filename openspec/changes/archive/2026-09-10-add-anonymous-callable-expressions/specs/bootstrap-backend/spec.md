## ADDED Requirements

### Requirement: LLVM targets execute verified exact anonymous callable environments

LLVM-native and LLVM-to-Wasm lowering SHALL consume the verified MIR target, explicit signature,
derived mode, and finite ordered environment for an anonymous callable. Both targets SHALL preserve source
acquisition order, authored parameter order, invocation-mode checks, and exactly-once cleanup. A
backend MUST NOT introduce a universal indirect closure ABI or merge distinct source targets solely
because their signatures or environments are equal. A backend MAY eliminate a nonescaping or empty
environment only when the optimization preserves all observable identity and ownership behavior.

#### Scenario: Preserve an environment-bearing callback across LLVM targets

- **WHEN** an anonymous callable captures values in an order different from its authored parameter order
- **THEN** independently pinned native execution and LLVM-to-Wasm execution retain the verified target, operand order, result, and cleanup
