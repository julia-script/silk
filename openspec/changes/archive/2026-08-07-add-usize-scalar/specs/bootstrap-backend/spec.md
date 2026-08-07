## ADDED Requirements

### Requirement: Backends realize the selected Usize lane exactly

Native LLVM lowering SHALL realize the compiler-selected native `Usize` lane as an unsigned 64-bit
integer and direct Wasm lowering SHALL realize the Wasm lane as `i32` with unsigned comparison,
division, remainder, and overflow behavior. Calls, parameters, returns, locals, aggregates, and
operators MUST preserve the compiler-owned calling shape. Neither backend may narrow native values
to `I32` or use signed Wasm instructions for unsigned operations.

#### Scenario: Lower unsigned Wasm comparison

- **WHEN** a Wasm-target function compares `Usize` values above the signed `i32` boundary
- **THEN** emitted code uses unsigned comparison semantics and matches logical evaluation

#### Scenario: Return a native 64-bit value

- **WHEN** a native function returns a `Usize` value above `2^32 - 1`
- **THEN** its signature and return operation preserve the selected 64-bit lane without truncation
