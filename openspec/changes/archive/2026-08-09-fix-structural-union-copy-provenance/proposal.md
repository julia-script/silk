## Why

Silk already specifies that a structural union is Copy when every member is Copy and cleanup-free,
but raw-storage verification currently rejects every structural-union copy. The stack VM reached
this contradiction through its natural ordered event stream and had to split steps and diagnostics
into separate vectors, so the defect now has a small, real acceptance case.

## What Changes

- Make compiler verification recognize normalized structural unions as Copy exactly when every
  member is recursively Copy and cleanup-free.
- Preserve the union's canonical active member and complete payload when `Slot.copy` or the shared
  `RawBuffer.read` intrinsic copies it, with evaluator, native LLVM, and direct Wasm parity.
- Permit `Vector.get` to return supported structural-union elements through a shared vector borrow
  without allocation, mutation, or ownership changes.
- Restore the stack VM's single ordered `Vector<Step | VmDiagnostic>` observation stream and read it
  back through the public shared Vector API.
- Continue rejecting a union when any member is move-only, Drop-bearing, borrowed, or otherwise not
  a legal Copy value.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-structural-unions`: make the existing all-members-Copy contract executable for whole
  structural-union values.
- `bootstrap-mir`: verify structural-union copy provenance and preserve the canonical sum shape.
- `bootstrap-owned-allocation`: allow unsafe shared raw-buffer and Slot copies for recursively Copy
  structural-union elements.
- `bootstrap-owned-sequence`: extend shared checked Vector reads from nominal Copy records to
  recursively Copy structural unions.
- `bootstrap-evaluation`: copy union storage without changing active-member or ownership state.
- `bootstrap-backend`: lower the same tag-and-payload copy in native LLVM and direct Wasm.
- `bootstrap-language-pressure-programs`: use one ordered union event vector in the stack VM as the
  real-program acceptance proof.

## Impact

The change affects compiler Copy classification and MIR verification, raw-storage execution and
backend lowering tests, Vector acceptance, and the stack-VM pressure source/findings. It introduces
no new syntax, intrinsic, collection primitive, runtime allocation, dependency, or union ABI
promise.
