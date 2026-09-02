## ADDED Requirements

### Requirement: Backends execute verified exact anonymous callable environments

The Wasm and native backends SHALL consume the verified MIR target, explicit signature, derived
mode, and finite ordered environment for an anonymous callable. Both backends SHALL preserve source
acquisition order, authored parameter order, invocation-mode checks, and exactly-once cleanup. A
backend MUST NOT introduce a universal indirect closure ABI or merge distinct source targets solely
because their signatures or environments are equal. A backend MAY eliminate a nonescaping or empty
environment only when the optimization preserves all observable identity and ownership behavior.

#### Scenario: Preserve an environment-bearing callback across backends

- **WHEN** an anonymous callable captures values in an order different from its authored parameter order
- **THEN** evaluator, Wasm, and native execution agree on its result and cleanup while retaining the verified target and operand order
