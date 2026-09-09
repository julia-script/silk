## Why

The self-hosted compiler CLI takes a 46.32-second median cold build in the fresh apply
baseline. A structural census identifies one 52-lane parser result type
as the source of 122,408 LLVM loads, including 69,952 with no consumers: aggregate
scalarization creates substantial avoidable work independently of JavaScript overhead.

## What Changes

- First add compiler-planned physical storage views for Effect outcomes and represented
  composite carriers. Calling shapes alone do not provide their canonical addressable
  storage; the backend must not invent it. Keep value storage, capture environments,
  and existing call/suspension transport explicitly distinct.
- Replace the universal per-local scalar-lane vector with a lowered-value model that
  distinguishes direct scalar/descriptor values from typed aggregate storage locations.
- Keep recursively growing aggregate representations in storage across local operations,
  joins, and possible alias writes. Project the fields actually accessed; use explicit
  destination construction and ownership-aware copy/move operations.
- Materialize lanes at existing compiler-planned call and return boundaries, not after
  every possible mutation. This change does not redesign the calling convention.
- Carry the same model through concrete Effect/callable environments, outcomes, cleanup,
  and suspension. Preserve compiler-owned layouts, union carrier mappings, and provenance.
- Remove the superseded aggregate lane-cache path and migrate all affected consumers,
  tests, and documentation together; no feature flag or compatibility implementation.
- Measure structural amplification and cold compilation using the unchanged real
  parser and full CLI, including hosted startup and excluding stdin.

## Capabilities

### New Capabilities

None. This is an internal lowering refactor, not a new source or artifact capability.

### Modified Capabilities

None. Existing `bootstrap-backend` and `bootstrap-target-layout` requirements remain the
contract, including compiler-owned calling shapes and union mappings. Adding missing
internal storage facts fulfills that contract without changing source behavior or the ABI.
The change declares
`skip_specs: true`; it does not invent a behavioral requirement to describe implementation.

## Impact

Primary scope: target layout planning, storage-view verification/encoding, and compiler
native value/storage, place, aggregate, operation, call/return,
Effect, diagnostic, cleanup, and suspension lowering. Shared LLVM-to-Wasm lowering must
also remain correct. Public LLVM construction APIs remain effectful and ownership-checked.

No language syntax, ownership semantics, runtime selection, C ABI, new intrinsic, JS-specific
optimization, global liveness pass, or compact LLVM-builder rewrite is included. The parser
source and hosted runtime stay unchanged as acceptance workloads. Any newly required LLVM
memory helper capability must be accounted for through existing target/toolchain planning.

The investigation and proposed design are recorded in `evidence.md` and `design.md`.
No speedup is claimed until the replacement is implemented and measured.
