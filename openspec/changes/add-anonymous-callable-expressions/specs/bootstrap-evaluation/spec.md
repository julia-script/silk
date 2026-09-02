## ADDED Requirements

### Requirement: Evaluation executes anonymous callable values exactly

The evaluator SHALL construct anonymous environments in deterministic capture order, invoke their
statically identified bodies, enforce derived shared, exclusive, and consuming access, preserve
mutation between reusable invocations, and perform MIR-specified cleanup for invoked and
dropped-uninvoked values. It MUST NOT use JavaScript closure identity or garbage collection as
language semantics. Invoking an effectful anonymous callable SHALL construct its Effect lazily; the
anonymous body SHALL perform its effect work only when that Effect is run.

#### Scenario: Evaluate every capture mode

- **WHEN** anonymous callables using Copy, shared-borrow, exclusive-borrow, and moved-affine captures are invoked under their valid modes
- **THEN** evaluation returns the declared results with the same access, reuse, mutation, consumption, and cleanup behavior as equivalent exact callable environments

#### Scenario: Drop a consuming anonymous value

- **WHEN** a moved-capture anonymous callable is never invoked
- **THEN** evaluator traces show the captured owner cleaned exactly once and no body invocation

#### Scenario: Run an inline recovery Effect

- **WHEN** a failing Effect is recovered by `effect fn(error: Failure) -> i32 { return 42 }`
- **THEN** handler invocation constructs a lazy Effect and running the composition completes with `42` in deterministic trace order

