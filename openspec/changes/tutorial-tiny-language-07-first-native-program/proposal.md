## Why

Learners need a native success before the compiler becomes complex. Lowering `fn main() = 42` connects the earlier LLVM model to the package API and proves the complete IR-to-executable path.

## What Changes

- Add the first LLVM lowering lesson for an integer literal and zero-argument `main`.
- Use public Builder, Type, Function, Block, Constant, FunctionBody, and IrText actors.
- Explain builder ownership, typed signatures, blocks, instructions, and terminators at the point of use.
- Compile the generated `.ll` with Clang and verify exit code `42`.
- Document recovery for wrong signatures, unterminated blocks, and foreign handles.

## Capabilities

### New Capabilities

None. This change adds documentation and tutorial-example material, so the change opts out of behavioral specs with `skip_specs: true`.

### Modified Capabilities

None.

## Impact

Adds tutorial compiler code, a minimal source fixture, an IR expectation, and native validation. It consumes existing LLVM APIs without changing them.

