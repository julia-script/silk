## Why

Module declarations alone cannot express executable LLVM IR. The builder needs a safe SSA construction model that preserves Zig's value-index behavior while preventing unfinished or structurally invalid function bodies from escaping.

## What Changes

- Add scoped function-body construction with arguments, basic blocks, insertion cursors, values, instruction names, and deterministic value numbering.
- Add integer and floating arithmetic, unary operations, casts, comparisons, select, and basic aggregate extraction/insertion.
- Add branches, conditional branches, switch, phi nodes, calls, returns, and unreachable terminators.
- Validate block termination, phi incoming edges, operand ownership, function signatures, and other locally checkable invariants through `SilkError`.
- Commit completed bodies atomically to their function declarations and prevent unfinished body state from becoming observable.
- Extend textual IR and bitcode output for the complete core instruction set and verify representative control-flow graphs with LLVM.

## Capabilities

### New Capabilities

- `llvm-core-function-bodies`: Safe construction and serialization of core SSA function bodies and control flow.

### Modified Capabilities

None.

## Impact

This adds public `FunctionBody`, `Block`, and `Value` actors and expands `Function`, `IrText`, and `Bitcode` behavior. It depends on `add-llvm-types-and-declarations`; the public API is Effect-native and does not expose Zig's `WipFunction` lifecycle or mutable internal arrays.
