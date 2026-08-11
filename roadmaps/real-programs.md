# Roadmap — Real programs

> Direction, not commitment — Now is committed; Next is planned; Later is exploration.
> Only Now items may be promised to anyone. This document changes as we learn.
> Last reviewed: 2026-08-11 · Review cadence: after each pressure-program milestone
> Scope: completed real-programs initiative — part of [the project roadmap](project.md)

## Vision

Silk's language surface is judged by recognizable programs rather than compiler-shaped snippets
alone. This initiative established the pressure-program method: build an honest complete program,
compare evaluation/native/direct-Wasm behavior, sweep allocation failures, preserve deterministic
artifacts, and promote only repeated general walls.

**Current objective:** no active pressure program. The first initiative and portable Logging are
complete; the project roadmap now owns the returned-borrow, Bytes, portable FileSystem,
target-availability, and native-provider sequence. After that sequence exists, another recognizable
program should exercise Logging and FileSystem together before any compiler port or broader I/O
family is selected.

## Column rules

- **Now** — problem validated, solution shaped, actively worked or next up. Committed.
- **Next** — problem chosen and understood; solution still in discovery. Planned, not promised.
- **Later** — problem worth solving, no solution chosen. Options, not a queue.

## Delivered baseline

- Seven familiar algorithms—Game of Life, Sieve, matrix multiplication, CRC-32, breadth-first
  search, quicksort, and FFT—are executable through evaluation, native LLVM, and direct Wasm.
- A Silk-written lexer differentially covers the full token vocabulary, invalid recovery, owned
  output, allocation failure, and deterministic artifacts.
- A bounded stack VM covers arithmetic, branches, invalid bytecode, stack bounds, loops, typed
  diagnostics, and one ordered structural-union event vector.
- Repeated findings produced typed scalar constants, contextual integer refinement, shared Vector
  reads, structural-union Copy provenance, native address-root repair, source-defined Effect
  pipelines, and synchronous Effect cost evidence.
- The latest lexer pass added escaped single-line and triple-quoted text/byte literals with
  deterministic delimiter diagnostics, CRLF normalization, formatting, and editor highlighting.

## Now

No active item. The FileSystem dependency sequence is tracked in the
[project roadmap](project.md) because it establishes language and service foundations rather than
belonging to one pressure program.

## Next

- Select one recognizable program that uses semantic logs and whole-file interaction through
  replaceable providers, including a deterministic user-defined virtual configuration.
- Preserve evaluator/native/direct-Wasm parity, failure-ordinal cleanup, deterministic artifacts,
  and transparent cost evidence. Do not automatically port another compiler module.

## Later

- Shape Stream/Sink abstractions from actual I/O consumers rather than projecting them from raw
  standard streams or complete-file operations.
- Expand structured observability from accepted Logger evidence into tracing and OpenTelemetry.
- Revisit general default-overridable service providers only after several explicit services have
  demonstrated the same application-boundary need.

## Maintenance budget

Reserve approximately 20% of initiative capacity when pressure work resumes.

- Keep evaluator/native/Wasm evidence complete at every language or standard-library boundary.
- Keep canonical `.silk` examples, embedded standard-library bytes, manifests, findings, labs, and
  editor tooling synchronized.
- Keep local ergonomics classified as findings until a second program demonstrates a general wall.

## Not doing

- Treating one pressure program as an implicit self-hosting sequence.
- Algorithm-specific intrinsics, fake byte wrappers, or precomputed answers.
- Making Logger a stdout alias or making FileSystem a native-process singleton.
- Selecting an owning String, Stream/Sink, async runtime, or broad platform API without executable
  pressure evidence.

## Open questions

- Which recognizable program can exercise logging, confined native files, and a user-defined
  browser-compatible virtual filesystem without turning into a compiler-port milestone?

## Changelog

- 2026-08-11: Synchronized the paused initiative with the five-change FileSystem plan. The next
  pressure program now waits for returned lexical borrows, owned Bytes, the portable service,
  reachable-only intrinsic availability, and the confined native provider; its deterministic
  virtual provider will be ordinary user source rather than a built-in implementation.
- 2026-08-10: Added one semantic completion observation to the bounded stack VM through
  `Effect.log` and an in-memory Logger while preserving its ordered VM event vector as program
  data. This supplies real-program pressure evidence for the complete-message service boundary
  without turning logging into raw output.
- 2026-08-10: Corrected Logging to guarantee one complete message per Logger invocation while
  leaving allocation, formatting, and physical writes to providers. Promoted owning String and
  formatting to project-level follow-up work before the next pressure program so dynamic logs can
  supply useful runtime evidence.
- 2026-08-10: Marked the first real-program initiative complete and removed its finished items from
  Now. Recorded multiline static literals from `5da21fd`. Moved semantic Logger from Later to the
  project-level Now horizon alongside portable FileSystem, because both establish reusable service
  boundaries before the next pressure program is selected.
- 2026-08-10: Classified 13 `RunStaticEffect` roots and closed runner CFG inlining as backend-only
  future work; LLVM already removes measured production overhead.
- 2026-08-10: Migrated the lexer, stack VM, and allocation-bearing algorithm entries to ordinary
  source-defined Effect pipelines and captured deterministic synchronous cost evidence.
- 2026-08-09: Completed shared Vector observation and structural-union Copy provenance; the stack VM
  again uses one ordered `Vector<Step | VmDiagnostic>` across all engines.
- 2026-08-09: Completed typed scalar constants and contextual integer refinement from independent
  lexer and VM evidence.
- 2026-08-09: Completed the bounded stack VM pressure program and repaired the path-insensitive
  native address-root defect it exposed.
- 2026-08-09: Completed the Silk lexer pressure program with exact TypeScript differential parity
  across all 67 token kinds, invalid diagnostics, allocation rollback, and deterministic artifacts.
- 2026-08-09: Completed seven algorithm examples, including allocation-pressure BFS, recursive
  quicksort, and deterministic FFT.
- 2026-08-09: Created the initiative by splitting the former real-program umbrella into executable
  evidence milestones rather than one speculative language backlog.
