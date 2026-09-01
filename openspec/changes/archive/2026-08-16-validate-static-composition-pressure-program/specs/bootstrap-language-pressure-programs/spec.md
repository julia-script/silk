## ADDED Requirements

### Requirement: Static composition pressures representation-dependent values

The repository SHALL maintain a complete ordinary-Silk pressure program that composes inspectable
nominal data with statically represented callable and Effect behavior, conditional compile-time
interfaces, and complete operation contracts. The program SHALL normalize different leaf results
before convergence and SHALL run equivalently through evaluation, native LLVM, and direct
WebAssembly without compiler-known library actor names.

#### Scenario: Pressure the complete capability set

- **WHEN** the static-composition program is compiled and executed through every engine
- **THEN** it exercises each enabling language capability in one connected source flow with equal results and cleanup

#### Scenario: Keep the CLI shape non-normative

- **WHEN** the example command and schema actor modules are renamed or replaced by equivalent ordinary source
- **THEN** the compiler continues to accept the program without actor-specific behavior
