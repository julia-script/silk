## ADDED Requirements

### Requirement: Native and Wasm execute suspended Effects with bounded machine stack

Native LLVM and direct WebAssembly SHALL realize target-neutral suspension as private iterative
execution boundaries whose machine-stack usage is bounded by a constant independent of the number
of active suspended logical invocations. A suspended child SHALL complete or suspend through the
private runner, then resume its parent with the exact typed outcome and live continuation state.
An explicit suspension origin SHALL return transfer to the private boundary; an ordinary
suspendable runner SHALL be able to complete synchronously or relay transfer. Relaying callers
SHALL prepare their unpublished continuation state without recursively starting the deferred child,
and the driver SHALL begin that child only after the complete continuation chain is published.
Neither backend MAY depend on LLVM `musttail`, WebAssembly tail-call instructions, host exception
unwinding, a JavaScript promise, or recursive host calls to provide this guarantee.

#### Scenario: Run deep non-tail suspension on native

- **WHEN** a native release artifact executes one million non-tail recursive Effect levels separated by `Effect.suspend`
- **THEN** it returns the expected result without `SIGSEGV` and without machine-stack growth proportional to the logical depth

#### Scenario: Run deep non-tail suspension on Wasm

- **WHEN** a direct Wasm artifact executes one hundred thousand non-tail recursive Effect levels separated by `Effect.suspend`
- **THEN** it returns the expected result without a host `RangeError`, an `unreachable` trap, or host-stack growth proportional to the logical depth

#### Scenario: Preserve typed failure through the private runner

- **WHEN** a deep suspended child produces a typed failure
- **THEN** native and Wasm resume and clean the same logical continuations as evaluation before returning the unchanged failure member and payload

### Requirement: Suspension runner ABIs remain private and pay for use

Continuation frame headers, resume discriminants, step results, driver loops, target function
references, and storage layouts SHALL remain backend-private and unreachable from Silk source.
A compiled program whose reachable MIR contains no suspension operation MUST NOT emit or link those
forms, a continuation allocation path, or a complete-versus-pending branch, and its established
synchronous entry and Effect-call artifact shape SHALL remain unchanged.

#### Scenario: Inspect a non-suspending native artifact

- **WHEN** a closed synchronous Effect program is compiled to native release bitcode
- **THEN** structural inspection finds its established direct Effect calls and no suspension driver, continuation allocator, resume dispatch, or pending branch

#### Scenario: Inspect a non-suspending Wasm artifact

- **WHEN** the same closed synchronous Effect program is compiled to direct WebAssembly
- **THEN** structural and linkage inspection finds no suspension table, driver, continuation allocation path, or pending branch
