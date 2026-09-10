## Why

JUL-123 requires descriptor buffers, scalar C accessor results and nested entry pointers to retain their representation distinctions. The current raw pointer type only stores mutability and pointee. Native layout also still obtains scalar alignment from the scalar catalog instead of the audited JUL-120 target description.

## What Changes

- **BREAKING** Replace the raw-pointer representation with explicit nullability, single/many extent, minimum alignment and data address space, retaining invariant pointee identity and independent mutability.
- Keep bare pointers distinct from slices. Add only the qualifier conversion, unaligned access and raw-slot address primitives needed by ordinary source wrappers.
- Add ordinary source uninitialized/initialized output-storage states using existing allocations, RawBuffer and Slot. Taking an address does not prove initialization, ownership transfer or retained-address permission.
- Feed audited target facts into primitive and external aggregate layout and scalar ABI classification. LLVM consumes those semantic results.
- Extend independent C/object and shared native acceptance evidence for the admitted boundary on Darwin ARM64, GNU/Linux x86-64, GNU/Linux ARM64 and retained LLVM-to-Wasm behavior.

## Capabilities

### New Capabilities

- `native-pointer-boundary`: qualified pointers, nullable foreign boundaries, unaligned access and source-owned output-storage state.

### Modified Capabilities

- `bootstrap-raw-pointers`: replace uniformly nullable single-shape pointers and the old primitive contract.
- `bootstrap-target-layout`: derive primitive representation from audited target descriptions.
- `bootstrap-foreign-functions`: admit qualified pointer forms and retain qualifier contracts across scalar C classification.

## Impact

Compiler syntax and semantic types, intrinsic catalog, type compatibility and keys, MIR validation/lowering, LLVM memory operations, ordinary pointer/storage modules, docs, generated inventories, packaged exports and native conformance fixtures. No compatibility representation is retained. JUL-124 owns foreign-call retention, effects, unwinding and sequencing; JUL-138 owns general retained-address pinning and the remaining pointer roadmap. No new aggregate-by-value ABI, variadics, callbacks, union or packed layout is introduced.
