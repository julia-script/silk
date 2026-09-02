## ADDED Requirements

### Requirement: Bulk raw-storage operations lower to bulk memory instructions

The native backend SHALL lower the bulk copy to `llvm.memmove` and the bulk set to `llvm.memset`,
and the WebAssembly backend SHALL lower them to `memory.copy` and `memory.fill`. Neither backend MAY
lower the copy to a form that is undefined for overlapping ranges, and neither MAY expand a bulk
operation into a per-element loop.

#### Scenario: Emit overlap-defined native code

- **WHEN** the native backend emits a bulk copy
- **THEN** the emitted module calls `llvm.memmove` rather than `llvm.memcpy`

#### Scenario: Emit Wasm bulk memory

- **WHEN** the WebAssembly backend emits a bulk copy or a bulk set
- **THEN** the emitted function body uses `memory.copy` or `memory.fill` over the module's private memory

### Requirement: A bulk range that runs past its storage traps

Every engine SHALL trap when a bulk operation's range runs past the destination buffer's element
count or past the source range's length, matching the bounds behavior of the per-element raw-storage
operations.

#### Scenario: Reject an out-of-range destination

- **WHEN** a copy's destination offset plus its length exceeds the destination buffer's count
- **THEN** the evaluator, the native binary, and the WebAssembly module each trap
