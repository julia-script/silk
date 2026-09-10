## Why

The existing libc qsort consumer passes a real exported Silk address, but its type cannot express callback access or the enclosing call's invocation promise. JUL-142 also requires calling native pointer values: contrary to its intake baseline, the current checker rejects that application with SEM0075 and has no indirect native MIR operation.

## What Changes

- **BREAKING**: require explicit synchronous, same-thread, nonescaping invocation assertions for foreign callback parameters; keep invocation obligations separate from callee ABI and access contracts.
- Extend the one native function-pointer type with exact behavioral identity and explicit nonnull representation. Admit complete-call borrowed references with the existing loan model; preserve unsafe raw-pointer obligations.
- Add unsafe native indirect invocation and enforce the existing fatal forbidden-unwind boundary for indirect calls and exported callback entry.
- Preserve named, nongeneric, noncapturing, synchronous export eligibility. Reject escaping/threaded/permitted-unwind requests and unsupported nullable invocation.
- Migrate qsort, interfaces, tooling, diagnostics and native fixtures together; verify exact C signatures, nested calls, source cleanup and three native lanes.

## Capabilities

### New Capabilities

- `synchronous-native-callbacks`: invocation/access composition, native indirect calls, reentrancy and enforcement evidence.

### Modified Capabilities

- `bootstrap-foreign-functions`: callback type contracts, export eligibility and indirect native invocation.
- `native-library-interface-artifacts`: callback behavioral and nullability identity in published interfaces.

## Impact

Compiler parsing, declaration/type completion, call checking, ownership, HIR/MIR, LLVM emission, semantic/interface/cache identity, C headers and source presentation. Reuses JUL-123 pointer/borrow support and JUL-124 foreign contracts, native fixtures and fatal personality. No new registration API, storage pinning, thread runtime, unwind provider or Wasm callback surface.
