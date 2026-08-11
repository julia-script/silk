## 1. Inventory and Guardrails

- [x] 1.1 Generate a checked fixture of every current intrinsic actor, nominal, operation, signature, safety flag, HIR/MIR identity, evaluator branch, backend branch, and host import
- [x] 1.2 Add verification that every source-callable compiler operation belongs to the authoritative catalog and every catalog member has evaluator and supported-backend coverage
- [x] 1.3 Encode the minimal intrinsic admission categories and require one canonical source consumer or direct language use for every catalog member
- [x] 1.4 Add negative tests proving ordinary source names cannot select compiler behavior outside the sealed intrinsic namespace

## 2. Source-Defined Service Syntax

- [x] 2.1 Add lossless and recoverable `service` declaration syntax with visibility, generics, operation names, complete function contracts, documentation, and bounded recovery
- [x] 2.2 Index service declarations as canonical source identities distinct from structs, ordinary interfaces, namespaces, and implementation declarations
- [x] 2.3 Resolve service types and operations through ordinary imports, visibility, collision, completion, occurrence, hover, and navigation facts
- [x] 2.4 Reject service fields, initializers, operation bodies, hidden defaults, and malformed contracts with phase-owned diagnostics

## 3. General Service Semantics

- [x] 3.1 Generalize conformance validation so any source-declared service maps its complete operation set to existing provider actor functions
- [x] 3.2 Validate provider parameters, success types, access, and covariant failure and requirement rows; reject missing, duplicate, extra, or stronger mappings
- [x] 3.3 Shape service witnesses and slots from declarations rather than compiler-known capability names, preserving canonical role and access ordering
- [x] 3.4 Generalize `Effect.provide`, `provideMut`, `provideWith`, requirement inference, instance discovery, and specialization for arbitrary source services
- [x] 3.5 Add evaluator, LLVM, and direct-Wasm tests for shared and exclusive user services, provider replacement, missing provision, roles, nested override, and cleanup
- [x] 3.6 Prove ordinary interfaces remain static conformances and never create requirement rows, service slots, or provision behavior

## 4. Sealed Intrinsic Namespace

- [x] 4.1 Replace independent compiler-known callable actors with one unshadowable `Intrinsic` namespace and stable globally unique operation names
- [x] 4.2 Preserve primitive scalar type spellings and language syntax outside the callable intrinsic namespace while removing Drop and Report from the callable catalog
- [x] 4.3 Update intrinsic references, presentation, completion, hover, occurrences, analysis queries, HIR identities, and diagnostics for qualified `Intrinsic` calls
- [x] 4.4 Classify every surviving intrinsic as safe or unsafe from its caller obligations and add one invariant statement plus negative test for each unsafe member
- [x] 4.5 Add catalog-to-HIR/MIR/evaluator/backend/host-import verification and deterministic encoding for the final intrinsic inventory

## 5. Scalar Primitives and Numeric Source APIs

- [x] 5.1 Move every concrete integer operation to a type-specific `Intrinsic` member without changing value, checked-result, conversion, or trap semantics
- [x] 5.2 Move every concrete floating operation to a type-specific `Intrinsic` member without changing classification, ordering, bit, conversion, transcendental, or trap semantics
- [x] 5.3 Add canonical standard-library numeric interfaces, actor-module wrappers, and core-owned conformances for every admitted primitive scalar type
- [x] 5.4 Implement generic numeric wrappers such as `add<T: Integer>` through ordinary interface constraints and conformance selection
- [x] 5.5 Prove specialization emits one concrete primitive with no runtime type tag, union, switch, registry, service slot, or residual interface dispatch
- [x] 5.6 Migrate operators, examples, fixtures, pressure programs, documentation, and tooling expectations from compiler actor calls to source APIs or explicit intrinsics

## 6. Effect, Place, Layout, and Storage Migration

- [x] 6.1 Move `Effect.result` and requirement binding behind source wrappers over minimal `Intrinsic` Effect primitives and remove their old privileged actor entries
- [x] 6.2 Move `Place.replace` behind a source wrapper, removing its intrinsic if the final safe place substrate can express it
- [x] 6.3 Move layout construction and repeat validation to source, retaining only the representation-dependent `Intrinsic.layoutOf<T>` primitive
- [x] 6.4 Define Allocator as a source service and SystemAllocator as a source implementation over audited primitive acquire, adopt, and release operations
- [x] 6.5 Move RawBuffer and Slot public actors to source over the narrow initializedness and ownership intrinsics, keeping bounds and safe reusable policy outside the compiler
- [x] 6.6 Migrate allocation failures and validation data to source where representation permits, preserving ownership, cleanup, OOM, and three-engine behavior
- [x] 6.7 Prove SystemAllocator, a user quota allocator, and Vector use general services and source wrappers without nominal-name branches in semantic facts, HIR, MIR, evaluator, or backends

## 7. Standard Streams Migration

- [x] 7.1 Define StandardStreams, its destinations, complete-write contract, and StreamWriteFailure in canonical Silk source
- [x] 7.2 Implement the native/hosted provider over one `Intrinsic.standardStreamWrite` primitive and retain the private versioned ABI beneath it
- [x] 7.3 Implement or migrate the deterministic in-memory provider without platform intrinsics and verify provider replacement through general service provision
- [x] 7.4 Remove `StandardStreams`, `stdout`, `stderr`, and `writeAll` name recognition from the intrinsic catalog and every compiler phase
- [x] 7.5 Verify complete-write success and failure parity across evaluator, native LLVM, and direct WebAssembly without adding logging semantics

## 8. Tooling, Documentation, and Release Gates

- [x] 8.1 Update the standard-library manifest and embedded sources for services, numeric interfaces, layout, Effect, allocation, storage, and standard-stream wrappers
- [x] 8.2 Update the analysis facade, Labs, completion, hover, occurrences, and definition fixtures to distinguish navigable source wrappers from source-less intrinsic members
- [x] 8.3 Remove every compatibility spelling and verify no compiler branch recognizes Allocator, SystemAllocator, StandardStreams, Logger, FileSystem, or numeric interface names
- [x] 8.4 Update CONTEXT, README files, Wayfinder decisions, agent guidance, and roadmaps with the minimal intrinsic rule and the service/interface distinction
- [x] 8.5 Reconcile the deferred Logging and FileSystem OpenSpec artifacts against the archived intrinsic and service contracts before either implementation begins
- [x] 8.6 Run `pnpm typecheck`, `pnpm exec biome check .`, focused differential and tooling tests, `pnpm test`, `pnpm check`, and `pnpm release:candidate`
