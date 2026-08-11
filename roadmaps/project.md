# Roadmap — Silk Effect

> Direction, not commitment — Now is committed; Next is planned; Later is exploration.
> Only Now items may be promised to anyone. This document changes as we learn.
> Last reviewed: 2026-08-11 · Review cadence: after each OpenSpec archive, or monthly when no
> change ships · Scope: whole project

## Vision

Silk Effect will be a low-level systems language that combines explicit memory and execution
control with typed failures, explicit replaceable services, deterministic cleanup, and
tooling-friendly semantics. Portable program intent should remain stable across native and browser
hosts; lower-level platform facilities are explicit escape hatches rather than the default API.

**Current objective:** build file interaction through five deliberately ordered changes rather than
one platform-shaped feature. Returned lexical borrows unlock source-defined owned Bytes; Bytes unlock
the portable whole-file FileSystem contract. Generic target-restricted intrinsics can proceed
independently, and the OS provider joins both branches last. The compiler supplies only irreducible
unsafe primitives; public services, providers, values, helpers, and safe policy remain ordinary
navigable Silk source.

## Current baseline

The stage-0 compiler now runs lossless source through module analysis, HIR, ownership and cleanup,
specialization, target layout, MIR, logical evaluation, LLVM/native emission, LLVM WebAssembly, and
direct WebAssembly. The language has modules, scalar families and typed constants, structs, arrays,
slices, structural unions and exhaustive matching, generics, callables and pipelines, mutation,
loops, recursion, affine ownership, deterministic Drop, explicit allocation, static text/bytes, and
typed service-requiring Effects. Every source-callable compiler primitive belongs to the sealed
`Intrinsic` namespace, while services and provider mappings are ordinary Silk declarations.

Canonical `Result`, Effect transformations, `Vector<T>`, semantic `Logger`, `Effect.log`, and its
stdout and in-memory providers are navigable Silk source. Seven
algorithm examples plus Silk-written lexer and bounded stack-VM pressure programs provide
evaluator/native/direct-Wasm, failure-ordinal, and determinism evidence. The completed compiler
realignment remains documented in [compiler-realignment](compiler-realignment.md); the pressure-
program record remains in [real programs](real-programs.md).

## Column rules

- **Now** — problem validated, solution shaped, actively worked or next up. Committed.
- **Next** — problem chosen and understood; solution still in discovery. Planned, not promised.
- **Later** — problem worth solving, no solution chosen. Options, not a queue.

## Now

### Return conservative lexical borrows from ordinary functions

- **Problem:** source-defined owners can accept slice borrows but cannot return a view into their
  own storage, blocking honest `Vector.asSlice`, `Bytes`, and Path observation APIs.
- **Outcome & done-when:** an ordinary function may return a shared or exclusive lexical view from
  exactly one borrowed parameter. Multiple origins, effect/service results, captures, and stored
  borrows remain rejected; ownership suspends conflicting owner access through the view's last use.
- **Status:** shipped, synced, and archived on 2026-08-11.
- **Appetite:** one conservative borrow-provenance slice plus minimal unsafe RawBuffer view
  intrinsics and ordinary `Vector.asSlice`/`asMutSlice` wrappers.
- **Boundary:** no lifetime syntax, borrow-polymorphic results, stored references, or compiler-known
  Vector policy.
- **Links:** change:
  [`add-returned-lexical-borrows`](../openspec/changes/archive/2026-08-11-add-returned-lexical-borrows/proposal.md) ·
  [slice spec](../openspec/specs/bootstrap-runtime-slices/spec.md) ·
  [ownership spec](../openspec/specs/bootstrap-ownership/spec.md)

### Add source-defined owned Bytes

- **Problem:** portable I/O needs owned arbitrary octets, but returning `Vector<u8>` everywhere would
  expose a general collection as the permanent domain API and compiler privilege is unnecessary.
- **Outcome & done-when:** canonical `Bytes` is an encoding-neutral nominal wrapper over
  `Vector<u8>` with minimal construction, copy, append, length, lexical views, ownership, allocation,
  cleanup, tooling, and three-engine parity.
- **Status:** shipped, synchronized, and archived; portable FileSystem is now the next dependency.
- **Appetite:** one ordinary standard-library actor with no new compiler primitive.
- **Boundary:** no String, UTF-8 promise, formatting, filesystem policy, or broad byte algorithms.
- **Links:** change:
  [`add-owned-bytes`](../openspec/changes/archive/2026-08-11-add-owned-bytes/proposal.md) ·
  [standard library spec](../openspec/specs/bootstrap-silk-stdlib/spec.md)

### Enforce target availability only for reachable intrinsics

- **Problem:** native-only compiler primitives must not make portable programs target-specific merely
  because their source declarations are packaged or imported.
- **Outcome & done-when:** every intrinsic has an enforced supported-target set; validation runs over
  executable operation closure; unsupported reachable calls receive a stable diagnostic; unreachable
  calls contribute no runtime symbols, imports, or host adapters.
- **Status:** proposal complete and strictly validated; independent of the borrow/Bytes chain and a
  prerequisite for the OS provider.
- **Appetite:** one generic catalog, closure, diagnostic, evaluator, and backend mechanism with
  pay-for-use artifact tests.
- **Boundary:** no provider names, conditional parsing, hosted-Wasm ABI, or automatic backend switch.
- **Links:** change:
  [`add-target-restricted-intrinsics`](../openspec/changes/add-target-restricted-intrinsics/proposal.md) ·
  [intrinsic boundary spec](../openspec/specs/bootstrap-intrinsic-boundary/spec.md)

### Define portable whole-file interaction

- **Problem:** applications need stable file intent that user-defined native, Wasm, virtual, and test
  providers can satisfy without leaking handles, host paths, or ambient process state.
- **Outcome & done-when:** provider-absolute `Path`, allocation-free `FileError`, owned metadata and
  entries, seven mutable whole-file service primitives, and ordinary recursive/existence helpers
  agree across evaluation, native LLVM, and direct Wasm with pure user providers.
- **Status:** fully rescoped proposal complete and strictly validated; follows returned borrows and
  owned Bytes.
- **Appetite:** one portable contract and canonical source slice. No provider implementation belongs
  in this change.
- **Boundary:** writes receive a complete byte view but need not be physically atomic or transactional;
  there is no `PlatformFileSystem`, public handle, rename, recursive service primitive, implicit cwd,
  built-in in-memory provider, or hosted-Wasm ABI.
- **Links:** change:
  [`add-portable-file-system`](../openspec/changes/add-portable-file-system/proposal.md) ·
  [runtime and standard-library decision](../wayfinder/bootstrap-language/issues/07-minimum-runtime-and-standard-library.md)

### Supply a confined native OS provider

- **Problem:** the portable contract needs a useful native implementation without making OS handles,
  fallible close, root traversal, or native error codes part of the service API.
- **Outcome & done-when:** the compiler exposes only unsafe native-only affine handle/path primitives;
  ordinary `OsFileSystem` source brackets every resource, confines each operation beneath an owned
  native root, translates low-level statuses, and implements the seven portable operations. Pure
  direct-Wasm providers and no-filesystem programs retain zero OS imports.
- **Status:** proposal complete and strictly validated; starts after both portable FileSystem and
  target-restricted intrinsics land.
- **Appetite:** one low-level protocol, injected evaluator adapter, reachable native runtime, and
  ordinary provider with security and cleanup acceptance.
- **Boundary:** no public handles, `PlatformFileSystem`, symlink following, automatic fallible Drop,
  built-in browser VFS, WASI binding, or transactional writes.
- **Links:** change:
  [`add-os-file-system-provider`](../openspec/changes/add-os-file-system-provider/proposal.md) ·
  [portable contract](../openspec/changes/add-portable-file-system/proposal.md)

## Next

- **Add owning String and formatting** — after the filesystem sequence, shape a canonical owning
  UTF-8 String and formatting API that can compose runtime values and expose a borrowed message view
  without changing the Logger contract. Keep it a separate OpenSpec change so ownership, allocation,
  Unicode validity, formatting failures, cost, dispatch, and provider presentation remain explicit.
- **Resume recognizable pressure programs** — after the portable contract and native provider exist,
  select a program that uses Logging and FileSystem and promote only repeated language, library,
  compiler, tooling, or cost walls. Do not automatically port another compiler module.
- **Revisit general default providers** — decide whether application-boundary defaults are useful
  only after several explicit services exist. Defaults must apply uniformly; Logger and FileSystem
  receive no ambient exception.

## Later

- **Make Silk capable of expressing its own compiler** — progressively replace the TypeScript seed
  only when the language can express compiler modules without letting the port choose premature
  features.
- **Supply and prove the native compiler platform** — add the host runtime and private shims demanded
  by real Silk compiler modules, then reach stage 1, stage 2, and a byte-identical fixed point.
- **Preserve pay-for-use synchronization** — add sequential Stream demand, single-thread
  concurrency, and later parallel execution without charging synchronous programs for a scheduler.
- **Grow portable service families** — networking, schemas, serialization, observability, testing,
  clocks, and richer I/O become candidates after FileSystem completes the pattern started by Logging.
- **Deepen WebAssembly integration** — extend host bindings and generated Effect interop after the
  portable service boundary is accepted.

## Maintenance budget

Reserve approximately 20% of project capacity for keeping the foundation trustworthy.

- Keep `@silk-effect/llvm` aligned with its pinned upstream baseline, deterministic fixtures,
  Effect architecture rules, and packed release-candidate checks.
- Keep README, language context, Wayfinder decisions, OpenSpec capabilities, archived changes, and
  roadmaps synchronized so shipped behavior and future direction are never mixed.
- Keep evaluator/native/Wasm differential evidence and test runtime budgets meaningful as the
  pressure corpus grows.

## Not doing

- Treating `Effect.log` as stdout syntax, a byte stream, or an ambient global logger.
- Defining portable `FileSystem` in terms of native handles, implicit working directories, Unix path
  bytes, browser-only objects, or one privileged runtime implementation.
- Pretending every host feature is portable; native-only needs use explicit target-restricted
  intrinsics and ordinary platform providers rather than contaminating the portable service.
- General concurrency, atomics, async scheduling, networking, or broad user-facing FFI during this
  service slice.
- An owning String or formatting framework inside the completed minimal Logger change; those remain
  separate ordinary standard-library work over the accepted borrowed-message boundary.
- A package registry, dependency solver, production build system, or compatibility layer for
  unreleased APIs.
- Selecting a parser port or self-hosting sequence merely because file access becomes available.
- Adding another portable host service before the FileSystem sequence is validated and archived.

## Open questions

- What executable name and public analysis facade should eventually accompany
  `@silk-effect/compiler`?

## Changelog

- 2026-08-11: Shipped, synced, and archived source-defined owned `Bytes`, including stable
  field-projected returned borrows, ordinary `Vector<u8>` storage, move-only cleanup, mutable and
  shared slice views, editor navigation, allocation failure coverage, and evaluator/native/direct-
  Wasm parity. Promoted portable FileSystem to the next implementation item.
- 2026-08-11: Shipped, synced, and archived conservative returned lexical borrows, including
  last-use ownership, unsafe generic RawBuffer views, ordinary Vector slice accessors, tooling,
  and evaluator/native/direct-Wasm parity. Promoted owned Bytes to the next implementation item.
- 2026-08-11: Split the former all-in-one FileSystem plan into five strictly validated changes.
  Returned lexical borrows unlock source-defined owned Bytes; Bytes unlocks the portable seven-
  operation FileSystem contract; generic reachable-only intrinsic availability proceeds independently;
  and a confined native `OsFileSystem` joins both branches last. Removed `PlatformFileSystem`, built-in
  in-memory and hosted-Wasm providers, public/streaming handles, and transactional write implications
  from the portable cut. Recorded explicit-base Path resolution, allocation-free errors, ordinary
  recursive/existence helpers, manual fallible close, and no-use/pure-provider Wasm pay-for-use.
- 2026-08-10: Implemented the portable Logger slice in ordinary Silk source: complete borrowed
  messages with separate severity, explicit service requirements, bounded in-memory and direct
  stdout providers, typed failure, evaluator/native/direct-Wasm parity, editor navigation,
  determinism, Labs, and stack-VM pressure evidence. All typecheck, formatting, full-suite,
  repository, release-candidate, and strict OpenSpec gates pass. The change is synchronized and
  archived; FileSystem is next.
- 2026-08-10: Archived and merged `establish-minimal-intrinsic-boundary` in `e9ebe1b`/`6088060`,
  removed the completed foundation from Now, and unblocked Logging. Corrected the Logger contract:
  each call supplies one complete borrowed message, while providers own formatting, allocation, and
  physical writes. Promoted owning String and formatting as the natural follow-up to make dynamic
  logs useful; removed the resolved timeout investigation after the final 927-test check passed.
- 2026-08-10: Promoted `establish-minimal-intrinsic-boundary` ahead of portable services. Logging
  and FileSystem are now blocked until compiler-known operations move under one sealed namespace,
  the minimal intrinsic rule is enforced, and ordinary Silk source can declare service contracts.
- 2026-08-10: Implemented the minimal intrinsic boundary: one sealed callable namespace,
  source-defined services and providers, static numeric interfaces, generic integer addition,
  source-owned layout/allocation/storage/Effect/standard-stream policy, and hosted-write/storage
  primitives with no standard-library-name lowering branches. Reconciled the deferred Logging and
  FileSystem proposals with that boundary; archival remains their implementation gate.
- 2026-08-10: Synchronized the public project story after 119 archived changes. Removed completed
  work from Now, recorded the current compiler/language baseline, added shipped multiline text/byte
  literals from `5da21fd`, and reconciled README, compiler documentation, language context, and the
  minimum-runtime decision. Promoted portable semantic Logging and portable whole-file FileSystem
  interaction to Now from explicit user direction; both use replaceable Effect services, while
  stdout and native filesystem mechanisms remain lower-level providers. Created and strictly
  validated implementation-ready `add-portable-logging` and `add-portable-file-system` changes.
- 2026-08-10: Added general exclusive `Effect.provideMut` in `7186153`; allocator provision is now
  ordinary source-defined service provision with no allocator-specific alias.
- 2026-08-10: Closed the static-runner CFG-inlining spike in `62249a3`/`66d6137`. LLVM already erases
  measured synchronous Effect overhead; direct-Wasm runner inlining remains optional until browser
  performance evidence justifies its ownership and cleanup complexity.
- 2026-08-10: Shipped shared static Effect normalization in `3ec9e75` and the synchronous cost corpus
  in `318ccdc`; evaluator, LLVM, direct Wasm, verification, and labs consume one MIR verdict.
- 2026-08-10: Replaced privileged Effect recipes with visible source-defined combinators in
  `ca1dfa5`/`b08eec4`, including kinded channel rows, stored Effect values, outcome reification, and
  typed requirement binding.
- 2026-08-09: Completed shared Vector reads, structural-union Copy provenance, typed scalar
  constants, contextual integer refinement, and native address-root repair through the lexer and
  stack-VM pressure programs.
- 2026-08-09: Closed the first real-program milestone: seven familiar algorithms, static text,
  standard streams, integer and floating scalar families, recursion, and allocation pressure all
  reached evaluator/native/direct-Wasm parity.
- 2026-08-08: Shipped the self-contained allocation substrate and Silk-written `Vector<T>` plus
  scanner with failure-ordinal cleanup and fresh-process determinism.
- 2026-08-07: Settled the defining execution model around `Effect`, `effect {}`, `effect fn`,
  explicit service provision, affine owners, and deterministic restricted `Drop`.
- 2026-08-05: Completed the 13-change compiler realignment from lossless source through native
  executable, with deterministic encoders and facade-only inspector labs at every phase.
