# Roadmap — Silk Effect

> Direction, not commitment — Now is committed; Next is planned; Later is exploration.
> Only Now items may be promised to anyone. This document changes as we learn.
> Last reviewed: 2026-08-10 · Review cadence: after each OpenSpec archive, or monthly when no
> change ships · Scope: whole project

## Vision

Silk Effect will be a low-level systems language that combines explicit memory and execution
control with typed failures, explicit replaceable services, deterministic cleanup, and
tooling-friendly semantics. Portable program intent should remain stable across native and browser
hosts; lower-level platform facilities are explicit escape hatches rather than the default API.

**Current objective:** establish a minimal explicit Intrinsic boundary and general source-defined
services, then use that foundation for semantic Logging and whole-file FileSystem interaction. The
compiler supplies only irreducible primitives; public service contracts, implementations, generic
interfaces, and safe abstractions remain ordinary navigable Silk source.

## Current baseline

The stage-0 compiler now runs lossless source through module analysis, HIR, ownership and cleanup,
specialization, target layout, MIR, logical evaluation, LLVM/native emission, LLVM WebAssembly, and
direct WebAssembly. The language has modules, scalar families and typed constants, structs, arrays,
slices, structural unions and exhaustive matching, generics, callables and pipelines, mutation,
loops, recursion, affine ownership, deterministic Drop, explicit allocation, static text/bytes, and
typed service-requiring Effects.

Canonical `Result`, Effect transformations, and `Vector<T>` are navigable Silk source. Seven
algorithm examples plus Silk-written lexer and bounded stack-VM pressure programs provide
evaluator/native/direct-Wasm, failure-ordinal, and determinism evidence. The completed compiler
realignment remains documented in [compiler-realignment](compiler-realignment.md); the pressure-
program record remains in [real programs](real-programs.md).

## Column rules

- **Now** — problem validated, solution shaped, actively worked or next up. Committed.
- **Next** — problem chosen and understood; solution still in discovery. Planned, not promised.
- **Later** — problem worth solving, no solution chosen. Options, not a queue.

## Now

### Make compiler privilege explicit and minimal

- **Problem:** The compiler-known actor catalog currently mixes irreducible scalar, Effect,
  storage, and platform operations with public abstractions such as Allocator and StandardStreams.
  Silk source cannot declare a new service contract, so implementing Logger now would add another
  privileged name or build on a mechanism users cannot reproduce.
- **Outcome & done-when:** Every callable compiler primitive is an audited member of the sealed
  `Intrinsic` namespace. The compiler exposes only the smallest target-neutral operations needed to
  build features in the standard library. Silk source can declare services, map provider actor
  functions, require them in Effect contracts, and provide them lexically. Scalar interfaces,
  Allocator, SystemAllocator, StandardStreams, layout policy, Effect wrappers, and safe storage APIs
  are navigable source over those primitives, with no runtime generic dispatch or name-based
  compiler branches.
- **Status:** implemented — source, tooling, evaluator, LLVM, and direct-Wasm work is complete;
  final repository gates and OpenSpec archival are the remaining handoff steps.
- **Appetite:** one breaking foundation change delivered by catalog family, with differential
  evaluator/native/direct-Wasm evidence after services, scalars, Effect, allocation/storage, and
  standard streams migrate.
- **Links:** change:
  [`establish-minimal-intrinsic-boundary`](../openspec/changes/establish-minimal-intrinsic-boundary/proposal.md) ·
  [language context](../CONTEXT.md) ·
  [standard library spec](../openspec/specs/bootstrap-silk-stdlib/spec.md)

### Make semantic logging a portable Effect service

- **Problem:** Silk can write complete bytes to process stdout/stderr, but raw standard streams do
  not express semantic logging. Defining `Effect.log` as stdout would prevent tests, browsers,
  OpenTelemetry, fan-out, and alternative presentation from supplying honest implementations.
- **Outcome & done-when:** Ordinary Silk code dispatches one complete `LogEvent` through
  `Effect.log`, retaining an explicit `Logger` requirement until a provider is supplied. The first
  live provider renders complete events to stdout through `StandardStreams`; an in-memory provider
  proves deterministic capture without process output. Evaluator, native LLVM, and direct Wasm
  agree on event order, typed failures, provider replacement, and composed Effect behavior.
- **Status:** blocked only on archival — its artifacts are reconciled with the implemented
  source-defined service, static interface, and sealed Intrinsic contracts; no logging code starts
  until `establish-minimal-intrinsic-boundary` archives.
- **Appetite:** one focused OpenSpec change covering the event boundary, Logger capability,
  `Effect.log`, stdout-backed provider, in-memory provider, tooling navigation, and three-engine
  acceptance. Rich tracing, filtering policy, asynchronous export, and a complete OpenTelemetry
  schema remain outside this slice.
- **Boundary:** a log call submits a complete semantic message, never a byte-at-a-time append.
  `StandardStreams` remains raw process output. Browser console and OpenTelemetry providers are
  compatible future implementations, not special behavior in `Effect.log`.
- **Links:** [language context](../CONTEXT.md) ·
  change: [`add-portable-logging`](../openspec/changes/add-portable-logging/proposal.md) ·
  [standard streams spec](../openspec/specs/bootstrap-standard-streams/spec.md) ·
  [Effect model](../openspec/specs/bootstrap-flow-functions/spec.md)

### Make file interaction portable by default

- **Problem:** Compiler and application code need files, but a common service defined in terms of
  native handles, host path bytes, seeking, or implicit process state cannot run unchanged against a
  browser virtual file system. Conversely, hiding every platform distinction would make genuinely
  native requirements dishonest.
- **Outcome & done-when:** A portable `FileSystem` capability exposes explicit-path whole-file and
  directory operations with complete owned values, portable errors, deterministic ordering, and no
  hidden current directory. The same program runs against native and deterministic in-memory
  providers; direct Wasm can use a host-supplied virtual provider with no source change. A separate
  lower-level `PlatformFileSystem` boundary is available for native paths, handles, mapping,
  locking, and metadata that have no portable contract.
- **Status:** blocked — follows the intrinsic boundary and Logging changes so both services exercise
  one consistent source-defined service, requirement, and provider vocabulary.
- **Appetite:** one focused OpenSpec change for the smallest complete-file slice, portable Path and
  FileError semantics, native and in-memory providers, browser-host compatibility, tooling, and
  differential acceptance. Open handles, streaming, mapping, watchers, locking, and broad native
  metadata stay outside the portable slice.
- **Boundary:** standard-library and documentation examples prefer `FileSystem`. Depending on
  `PlatformFileSystem` is an explicit portability decision. The stage-0 compiler's TypeScript host
  wrappers do not become Silk's public filesystem design.
- **Links:** [language context](../CONTEXT.md) ·
  change: [`add-portable-file-system`](../openspec/changes/add-portable-file-system/proposal.md) ·
  [runtime and standard-library decision](../wayfinder/bootstrap-language/issues/07-minimum-runtime-and-standard-library.md) ·
  [source-resolution precedent](../openspec/specs/bootstrap-source-resolution/spec.md)

## Next

- **Resume recognizable pressure programs** — after Logging and FileSystem exist, select a program
  that uses both and promote only repeated language, library, compiler, tooling, or cost walls. Do
  not automatically port another compiler module.
- **Revisit general default providers** — decide whether application-boundary defaults are useful
  only after several explicit services exist. Defaults must apply uniformly; Logger and FileSystem
  receive no ambient exception.
- **Characterize full-suite timeout reliability** — the latest local `pnpm check` completed Biome,
  typechecks, and 879 compiler tests, but 20 compiler cases expired at their configured timeouts.
  Determine whether this is cold/concurrent execution pressure or a regression before changing
  budgets or claiming the checkout is fully green.

## Later

- **Make Silk capable of expressing its own compiler** — progressively replace the TypeScript seed
  only when the language can express compiler modules without letting the port choose premature
  features.
- **Supply and prove the native compiler platform** — add the host runtime and private shims demanded
  by real Silk compiler modules, then reach stage 1, stage 2, and a byte-identical fixed point.
- **Preserve pay-for-use synchronization** — add sequential Stream demand, single-thread
  concurrency, and later parallel execution without charging synchronous programs for a scheduler.
- **Grow portable service families** — networking, schemas, serialization, observability, testing,
  clocks, and richer I/O become candidates after Logging and FileSystem establish the pattern.
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
- Pretending every host feature is portable; native-only needs use an explicit lower-level service.
- General concurrency, atomics, async scheduling, networking, or broad user-facing FFI during this
  service slice.
- A package registry, dependency solver, production build system, or compatibility layer for
  unreleased APIs.
- Selecting a parser port or self-hosting sequence merely because file access becomes available.
- Implementing Logger or FileSystem before the minimal Intrinsic boundary and source-defined
  service mechanism are archived.

## Open questions

- What executable name and public analysis facade should eventually accompany
  `@silk-effect/compiler`?

## Changelog

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
