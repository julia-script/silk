## ADDED Requirements

### Requirement: Differential gates cover generic specialization

The compiler driver corpus SHALL include valid inferred and explicit specializations, multiple
instances of one declaration, generic nominal layouts, recursive same-argument calls, invalid
arity and inference, and fresh-process determinism. Completing programs SHALL agree across
evaluation, native LLVM, and direct WebAssembly for their selected targets.

#### Scenario: Compare a multi-specialization program
- **WHEN** the corpus compiles and runs one declaration at two concrete argument types
- **THEN** all three engines agree on the result and the fresh-process artifacts remain identical

#### Scenario: Keep invalid inference out of lowering
- **WHEN** a corpus program cannot determine one type argument from supplied arguments
- **THEN** it produces the committed semantic diagnostic and no runtime instance, layout, or MIR function

