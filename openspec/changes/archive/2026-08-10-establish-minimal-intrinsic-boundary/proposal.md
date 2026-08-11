## Why

Silk currently presents services, standard-library policy, raw storage operations, Effect
machinery, and concrete scalar operations through one flat compiler-known actor catalog. Logging
and FileSystem would deepen that privilege unless the language first gains a small explicit
intrinsic boundary and ordinary source-defined services.

## What Changes

- **BREAKING**: Add one compiler-sealed `Intrinsic` namespace and move every source-callable
  compiler primitive beneath it; no other actor or operation name may select compiler behavior.
- Establish the minimal intrinsic rule: a compiler feature exposes only the smallest target-neutral
  primitive surface sufficient to implement its public abstraction in shipped Silk source.
- Add source declarations for `service` contracts, with operation signatures, interface
  conformance, service witnesses, requirement rows, and lexical provision handled by the general
  language model rather than a compiler-known service list.
- **BREAKING**: Replace compiler-defined `Allocator` and `StandardStreams` contracts with ordinary
  standard-library services. Their system implementations use explicit allocation, release, and
  standard-stream intrinsics; pure and user-defined implementations use the same contracts without
  compiler privilege.
- **BREAKING**: Move concrete scalar operations to per-type `Intrinsic` operations. Define generic
  numeric interfaces and actor-module wrappers in the standard library, with monomorphization
  selecting a concrete intrinsic through ordinary conformance.
- Audit every current intrinsic actor, nominal type, operation, lowering branch, and host import.
  Move validation, generic lifting, service policy, and safe abstractions into source; retain
  compiler ownership only for syntax, safety proofs, representation, irreducible primitives, and
  backend lowering.
- Classify intrinsic safety operation by operation. `Intrinsic` does not imply `unsafe`; raw
  allocation adoption, unchecked storage, and platform-pointer obligations require explicit
  unsafe boundaries, while scalar and other memory-safe primitives remain safe.
- Preserve completion, hover, occurrences, navigation, HIR, MIR, evaluation, native LLVM, and
  direct-WebAssembly agreement across intrinsic calls and their source-defined wrappers.
- Make this change an implementation prerequisite for portable Logging and FileSystem so neither
  feature introduces another compiler-known public abstraction.

## Capabilities

### New Capabilities

- `bootstrap-intrinsic-boundary`: The sealed namespace, minimal intrinsic rule, safety
  classification, complete catalog audit, and cross-engine primitive behavior.
- `bootstrap-service-declarations`: Source-defined service contracts, operation mappings,
  witnesses, requirements, and provision without compiler-known service names.

### Modified Capabilities

- `bootstrap-syntax`: Add lossless, recoverable `service` declarations and their operation
  contracts.
- `bootstrap-name-resolution`: Replace flat compiler-known callable actors with one sealed
  `Intrinsic` namespace while keeping language types and source declarations unambiguous.
- `bootstrap-semantic-facts`: Resolve intrinsic calls, service declarations, conformances, and
  service operation calls through distinct canonical facts.
- `bootstrap-integer-scalars`: Move concrete integer primitives under `Intrinsic` and expose
  generic standard-library numeric interfaces without runtime type dispatch.
- `bootstrap-floating-point-scalars`: Move concrete floating primitives under `Intrinsic` and
  expose their public actor-module operations through source.
- `bootstrap-owned-allocation`: Rebuild `Allocator`, `SystemAllocator`, layout policy, and safe raw
  storage APIs in source over the minimum allocation and storage primitives.
- `bootstrap-flow-functions`: Permit Effects to require and provide arbitrary source-declared
  services while keeping only irreducible Effect machinery intrinsic.
- `bootstrap-standard-streams`: Rebuild the service and its native implementation in source over
  one complete-write platform primitive.
- `bootstrap-silk-stdlib`: Ship the canonical service, numeric, layout, Effect, and storage wrappers
  that replace compiler-owned public abstractions.
- `bootstrap-hir`: Distinguish explicit `Intrinsic` calls, source calls, and general service-witness
  calls without recognizing standard-library names.
- `bootstrap-mir`: Retain only primitive intrinsic operations after source wrappers and service
  calls lower and specialize.
- `bootstrap-instances`: Discover and monomorphize source-defined interface and service
  conformances, including generic numeric calls.
- `bootstrap-analysis-facade`: Present the sealed intrinsic namespace and navigable source wrappers
  through one coherent analysis snapshot.
- `language-server-hover`: Show authoritative intrinsic signatures separately from source-defined
  wrapper and service declarations.
- `language-server-completion`: Complete `Intrinsic` operations only through the sealed namespace
  and prefer normal source APIs elsewhere.
- `language-server-navigation`: Keep source wrappers navigable while source-less intrinsic
  operations remain explicitly non-navigable.

## Impact

This change affects parsing, declaration indexing, name and type resolution, interface and service
conformance, requirement binding, intrinsic presentation, HIR, specialization, MIR, evaluation,
LLVM and direct-WebAssembly lowering, runtime imports, the embedded standard library, editor
tooling, examples, fixtures, and documentation. Existing intrinsic spellings are intentionally
broken rather than preserved with aliases. The active `add-portable-logging` and
`add-portable-file-system` changes remain planning artifacts but cannot enter implementation until
this change is archived.
