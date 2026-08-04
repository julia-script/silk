# Roadmap — Silk Effect

> Direction, not commitment — Now is committed; Next is planned; Later is exploration.
> Only Now items may be promised to anyone. This document changes as we learn.
> Last reviewed: 2026-08-04 · Review cadence: after each OpenSpec archive, or monthly when no
> change ships · Scope: whole project

## Vision

Silk Effect will be a low-level systems language that combines explicit memory and execution
control with typed failures, explicit service requirements, deterministic resource scopes, and
tooling-friendly semantics. The first destination is the smallest coherent language capable of
compiling its own compiler; broader language and ecosystem work follows evidence from that
self-hosting core.

**Current objective:** turn the first bootstrap function into recoverable syntax — measured by the
accepted fixture and a small malformed-input set producing deterministic, lossless concrete syntax
trees that can be inspected directly on the docs site.

## Column rules

- **Now** — problem validated, solution shaped, actively worked or next up. Committed.
- **Next** — problem chosen and understood; solution still in discovery. Planned, not promised.
- **Later** — problem worth solving, no solution chosen. Options, not a queue.

## Now

### Turn the first bootstrap function into recoverable syntax

- **Problem:** Source bytes now become trustworthy tokens, but those tokens do not yet express even
  one function declaration, a return statement, missing syntax, or parser recovery.
- **Outcome & done-when:** `pub fn main() -> I32 { return 42 }` becomes a deterministic, lossless
  concrete syntax tree; a bounded malformed-input corpus produces explicit missing and error nodes
  with stable diagnostics; and a direct-link-only docs page makes the tree inspectable without
  implying semantic analysis exists.
- **Status:** shaped — the grammar and recovery boundary are intentionally limited to one function
  and one integer return.
- **Appetite:** worth approximately one focused week; general expressions, multiple declarations,
  semantic facts, AST lowering, HIR, and MIR do not enter this scope.
- **Links:** change: `parse-first-bootstrap-function` ·
  [bootstrap lexer spec](../openspec/specs/bootstrap-lexer/spec.md) ·
  [bootstrap-language map](../wayfinder/bootstrap-language/map.md) ·
  [compiler pipeline decision](../wayfinder/bootstrap-language/issues/06-bootstrap-compiler-pipeline.md) ·
  [syntax decision](../wayfinder/bootstrap-language/issues/08-prototype-bootstrap-syntax.md)

## Next

### Give the first syntax tree semantic meaning

- **Problem:** A parsed `main` function and return expression still have no declaration identity,
  resolved type, or checked value.
- **Hypothesis:** We believe declaration collection plus the smallest `I32` type-checking slice will
  reveal the right semantic fact model before HIR is introduced or generalized.
- **Confidence:** medium.
- **Assumes:** A public parameterless `main` returning an `I32` constant is a useful permanent first
  semantic subset — decided, not yet implemented.
- **Open questions:** Should the first semantic change publish HIR, or only declaration and type fact
  tables until a second expression form requires a true semantic IR?

## Later

- **Lower checked programs into backend-neutral execution** — introduce HIR and MIR only when
  concrete declarations, types, control flow, ownership, failures, and services require those
  representations.
- **Produce the first native Silk program** — connect proven MIR to `@silk-effect/llvm`, scoped
  artifacts, Clang, native linking, and transactional output.
- **Supply the compiler's native platform** — add the minimum runtime, standard library, host
  services, and private C shim in response to real self-hosting compiler needs.
- **Make Silk capable of expressing its own compiler** — progressively replace the TypeScript seed
  implementation with Silk modules while preserving reference equivalence; revisit the exact port
  boundaries once the frontend, MIR, and runtime interfaces have survived real use.
- **Prove native self-hosting** — produce stage 1, stage 2, and a byte-identical fixed-point rebuild
  with complete conformance, failure, debug, resource, and performance evidence on all required
  native hosts.
- **Grow beyond the bootstrap subset** — concurrency, networking, schemas, observability, richer
  tooling, and broader standard-library families become candidates only after the self-hosting core
  exposes their real constraints.
- **Deepen WebAssembly integration** — preserve backend-neutral boundaries now; consider a direct
  WebAssembly backend and generated Effect interop after the native bootstrap path is accepted.

## Maintenance budget

Reserve approximately 20% of project capacity for keeping the foundation trustworthy.

- Keep `@silk-effect/llvm` aligned with its pinned upstream baseline, deterministic fixtures, Effect
  architecture rules, and packed release-candidate checks.
- Keep OpenSpec capabilities, archived changes, the Wayfinder decision index, and this roadmap
  synchronized with implementation discoveries instead of allowing multiple competing truths.

## Not doing

- General concurrency, atomics, async scheduling, networking, or a broad user-facing FFI during
  bootstrap — the compiler workload does not require them.
- A direct WebAssembly backend as a self-hosting prerequisite — backend-neutral MIR is sufficient
  protection until native self-hosting succeeds.
- A package registry, dependency solver, production build system, or full language server — none is
  required to prove the bootstrap language.
- A general incremental query engine — immutable analysis snapshots and localized deterministic
  worklists are the accepted bootstrap architecture.
- Backward compatibility for unreleased compiler APIs — early implementation evidence should be
  allowed to improve boundaries rather than fossilize them.

## Open questions

- What executable name and public analysis facade should eventually accompany
  `@silk-effect/compiler`?
- Should Silk compiler modules replace their TypeScript counterparts continuously as capabilities
  land, or should the first port begin after the stage-0 subset is feature-complete?
- What appetite should normally bound one bootstrap OpenSpec change when a semantic slice touches
  syntax, HIR, MIR, runtime, and fixtures together?

## Changelog

- 2026-08-04: Shipped exact source text and lossless lexing, then promoted a one-function concrete
  syntax tree and direct-link inspector to Now; semantic interpretation remains Next.
- 2026-08-04: Replaced the oversized end-to-end compiler-kernel initiative with source text and
  lexing. Moved parsing, semantic facts, HIR/MIR, native code generation, and runtime work behind
  evidence-producing capability boundaries.
- 2026-08-04: Created after completing the bootstrap-language Wayfinder map and archiving the LLVM
  builder and Tiny-language OpenSpec portfolios. The first bet is an end-to-end compiler kernel,
  followed by frontend semantics, ownership-aware lowering, and the native bootstrap platform.
