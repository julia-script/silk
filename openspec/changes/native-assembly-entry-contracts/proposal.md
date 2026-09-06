## Why

JUL-135 needs a checked machine boundary for source-owned Linux x86-64/ARM64 syscall wrappers and
entry fragments. LLVM assembly builders already exist; Silk lacks their source contracts.

## What Changes

- Add sealed `Intrinsic.assembly<Result>` with literal template/constraint/clobber/memory/
  side-effect/no-return metadata and a typed tuple of runtime scalar/pointer inputs.
- Admit only the initial Linux 64-bit register lanes and reject malformed or conflicting contracts.
- Add `with Intrinsic.machine(naked: true, noReturn: true)` for constrained zero-argument unit
  entry fragments, with exactly one terminal operand-free assembly operation and no compiler code.
- Propagate contracts through semantic analysis, MIR, native LLVM construction and identity.
- Verify real debug/optimized x86-64 and ARM64 objects and minimal independent machine consumers.

## Capabilities

### New Capabilities

- `native-assembly-contracts`: typed source assembly and constrained native entry properties.

### Modified Capabilities

None. Existing LLVM builders are reused. OS numbers, wrappers, allocation, startup composition and
complete no-libc executables remain JUL-136.

## Impact

Intrinsic inventory, declaration properties, semantic admission, HIR/MIR lowering and verification,
native function/assembly construction, tests, reference and generated documentation. Wasm, Darwin
assembly, arbitrary register classes/dialects and LTO remain unsupported by this source facility.
