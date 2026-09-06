## Why

LLVM legalization can introduce helper calls after the compiler's existing runtime report, while the driver currently adds libm to every hosted native link. JUL-127 requires an explicit account of actual helper needs and their providers before final linking, including a source support closure that cannot silently fall back to libc.

## What Changes

- Inspect admitted target objects after legalization and reconcile external symbols with declared foreign imports, language runtime contracts and typed LLVM helper requirements.
- Model memory, arithmetic, atomics, stack-probe, stack-protection, sanitizer and unwind families independently. Admit only independently verified initial helper contracts; reject unsupported families and unexplained symbols with their origins.
- Select source memory helpers and explicitly justified platform arithmetic providers only when needed. Define and validate a restricted freestanding support profile and detect direct, transitive and legalization-induced provider recursion.
- Integrate helper reports, retained provider objects and physical library inputs into artifact reporting and cache identity. Preserve the existing LLVM-to-Wasm closure and reject unverified LTO.
- **BREAKING**: remove unconditional libm selection and incomplete helper accounting. Execution-storage and hosted reporting policy remain the separate JUL-129/130 responsibilities.

## Capabilities

### New Capabilities

- `llvm-helper-capabilities`: post-legalization helper inventory, ABI/provider contracts, dependency closure, bootstrap restrictions and target conformance.

### Modified Capabilities

None. Existing artifact roots and native supply contracts are reused as integration boundaries.

## Impact

Compiler object emission, driver planning, native object inspection, ordinary Silk support source, public helper reports, generated compiler identities, native link/cache fixtures and the required Darwin ARM64/GNU x86-64/ARM64 conformance matrix. All implementation, migration, removal and validation remain within this ticket.
