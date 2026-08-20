## MODIFIED Requirements

### Requirement: Backends realize the compiler-owned union plan

Native LLVM and direct WebAssembly emission SHALL consume the union's compiler-owned discriminant,
member tags, payload placement, padding, calling shape, exact executable representation plans, and
member-slot mappings without choosing a different representation. Injection, calls, returns,
struct/array storage, reads, moves, writes, invocation, and execution SHALL preserve the same active
ordinary member and complete payload as evaluation.

#### Scenario: Emit one union through both backends

- **WHEN** a program injects scalar, array, nominal, droppable, and represented executable values into unions and transports them through aggregates
- **THEN** native and WebAssembly execution agree with evaluation on results and active-payload cleanup
