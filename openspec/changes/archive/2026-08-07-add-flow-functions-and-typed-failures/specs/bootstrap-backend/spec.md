## ADDED Requirements

### Requirement: Backends realize explicit typed outcomes without unwinding

Native LLVM and direct WebAssembly SHALL realize the selected tagged success/failure shape through
ordinary returns, calls, tests, and branches. They MUST NOT use C++ exceptions, platform unwinding,
`setjmp`, `longjmp`, host exception objects, or backend-selected discriminants. Success, recovery,
propagation, cleanup, and traps SHALL agree with evaluation.

#### Scenario: Execute the same recovered failure

- **WHEN** a canonical flow fixture selects its failure path and catches the exact member
- **THEN** native, Wasm, and evaluation produce the same result and cleanup order
