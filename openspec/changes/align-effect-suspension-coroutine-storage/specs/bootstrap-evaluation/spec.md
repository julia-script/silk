## MODIFIED Requirements

### Requirement: Evaluation executes suspension through logical activations

Evaluation SHALL execute explicit suspension by retaining the parent logical invocation as one
reusable coroutine frame in its heap activation machine, evaluating the deferred child, and
resuming the parent with the child's exact typed outcome. It SHALL model the same frame states,
ownership transfers, stable loans, provider access, cleanup, and release as native and Wasm without
requesting a source allocator, emitting continuation-allocation events, or producing typed storage
failure. A suspended parent SHALL remain one active source-logical invocation for `CallDepth`;
compiler-generated driver and resume work SHALL add no source-logical depth. Evaluation MUST NOT
recurse on the JavaScript stack or expose a pending value as a source result.

#### Scenario: Complete deep suspension under raised limits

- **WHEN** a terminating suspended Effect recursion is evaluated with step and call-depth limits above its logical work
- **THEN** evaluation completes with the same result, frame-state transitions, ownership, and cleanup trace as native and Wasm without depending on the JavaScript call stack

#### Scenario: Bound suspended logical recursion

- **WHEN** a suspended recursive Effect would add a source-logical invocation beyond `maxCallDepth`
- **THEN** evaluation returns the deterministic `CallDepth` blocked outcome at that exact source boundary rather than simulating allocation failure

#### Scenario: Preserve channels during evaluation

- **WHEN** evaluation runs `Effect.suspend` over `Effect<A ! E ? R>`
- **THEN** its source-visible outcome and requirements remain exactly `A ! E ? R` with no allocator request or `OutOfMemory` branch

#### Scenario: Exhaust private evaluator activation storage

- **WHEN** the evaluator cannot retain the private activation state required to continue suspended execution
- **THEN** evaluation terminates with a fatal host defect or trap-equivalent outcome outside the program's typed failure channel and does not synthesize `OutOfMemory`
