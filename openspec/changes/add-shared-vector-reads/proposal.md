## Why

Both completed language-pressure programs independently found that observing a `Vector<T>` requires
an exclusive borrow and a temporary move of its storage, even when `T` is Copy. That blocks natural
nested reads, forces consumers to destructure owned results, and overstates the cost and authority
of a read-only operation.

## What Changes

- **BREAKING** Change `Vector.get` from `&mut Vector<T>` to `&Vector<T>` for supported Copy element
  types while preserving checked bounds and the element-by-value result.
- Add one unsafe, bounds-checked shared raw-buffer read primitive that copies an initialized element
  without creating a mutable slot, moving the buffer, allocating, or changing cleanup state.
- Preserve the existing `Slot<T>` model as an exclusive lexical capability for initialization,
  mutation, take, copy, and drop; shared reads do not make slots aliasable.
- Verify and execute the shared read identically in MIR, evaluation, LLVM, and direct WebAssembly.
- Replace the lexer and stack-VM read workarounds with ordinary shared Vector observation and record
  the repaired wall in their findings.
- Keep structural-union element copies outside this change. They remain rejected until the separate
  provenance defect in structural-union copy lowering is repaired.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-owned-allocation`: extend narrow unsafe raw typed storage with a Copy-only shared read
  that does not expose an aliasable mutable slot.
- `bootstrap-owned-sequence`: make checked Vector reads available through a shared borrow for
  supported Copy elements without mutation or allocation.
- `bootstrap-mir`: represent and verify the shared raw-buffer read with canonical element, storage,
  bounds, access, and Copy provenance.
- `bootstrap-evaluation`: execute shared raw-buffer reads without mutating logical storage or
  ownership state.
- `bootstrap-backend`: lower the verified shared read identically in LLVM and WebAssembly.
- `bootstrap-language-pressure-programs`: require the lexer and stack VM to consume their ordinary
  Copy observations through shared Vector access and update their findings.

## Impact

The public Silk standard-library signature of `Vector.get` changes. The intrinsic catalog, HIR/MIR
lowering and verification, deterministic evaluator, LLVM backend, Wasm backend, language tooling,
tests, pressure programs, and findings are affected. No runtime scheduler, synchronization
primitive, collection-specific compiler operation, initialization bitmap, shared-reference
representation, or additional allocation is introduced.
