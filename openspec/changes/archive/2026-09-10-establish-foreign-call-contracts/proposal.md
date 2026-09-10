## Why

JUL-124 requires an explicit behavioral contract for immediate C calls. The existing scalar/pointer ABI and conservative address-root reloads do not express call-only loans or make a forbidden foreign unwind terminate at the Silk boundary.

## What Changes

- Add a sealed `with Intrinsic.foreign(...)` declaration clause with conservative defaults and explicit unsafe memory, capture, borrowed-parameter, returned-alias and no-return assertions.
- Admit single-value borrowed C parameters only under an explicit call-only contract, preserving ordinary initialized-state, aliasing and complete-call loan checks.
- Carry normalized contracts through semantic surfaces, executable inventory, MIR, LLVM and native interface identity; reject visible mismatches and unsupported LTO/unwind/lifetime requests.
- Generate an exception boundary around foreign calls that terminates through the existing fatal trap mechanism when the platform unwinder reaches it, including when a foreign caller could otherwise catch the exception.
- Verify renamed operation/accessor ordering, attributes and foreign throws with independent native fixtures on all three admitted native targets.
- **BREAKING**: replace type-only native ABI manifest function records with contract-bearing records; update every producer and consumer.

## Capabilities

### New Capabilities

- `foreign-call-contracts`: unsafe declaration assertions, immediate loans, behavior identity, optimizer constraints and forbidden-unwind termination.

### Modified Capabilities

- `bootstrap-foreign-functions`: add the sealed contract tail and narrowly admitted call-only borrowed scalar/pointer parameters.
- `native-library-interface-artifacts`: version the behavioral function records and preserve contract identity in imports and exports.

## Impact

Parser, declaration completion and presentation, borrowing/admission, semantic/cache keys, executable/MIR/ABI records, LLVM exception instructions and foreign lowering, native interface consumers, conformance fixtures and prescriptive documentation. Existing raw pointers remain non-owning. Retained storage, source TLS, escaping or threaded callbacks, permitted unwind, aggregate/variadic ABI and new OS services remain outside this change.
