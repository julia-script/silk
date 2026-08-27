## Context

The compiler already accepts each feature in isolation and in smaller pairings. See `proposal.md`
for why one composed program is required. The accepted language has only fixed-size inline arrays;
lexical slices, generic collection actors, owned dynamic vectors, and scoped allocation remain
Wayfinder decisions rather than implemented capabilities.

The workbench already models every preset as a root identity plus module map, while the current
shared three-engine corpus is mostly single-source. The acceptance fixture therefore needs an
explicit multi-module harness without changing compiler APIs or creating another inspector.

## Goals / Non-Goals

**Goals:**

- Exercise the complete first algorithmic-language slice in one understandable compiler-shaped
  fold.
- Reuse the production source resolver, analysis facade, evaluator, native backend, and direct
  WebAssembly backend.
- Make the browser preset byte-identical to the fixture accepted by automated tests.
- Record the first boundary that prevents the fixed program from becoming a realistic unbounded
  compiler pass.

**Non-Goals:**

- Add syntax, scalar types, generics, slices, collections, allocation, failures, services, or host
  access.
- Reimplement coverage semantics in tooling or make the acceptance fixture part of the compiler's
  public package API.
- Claim that a fixed-input fold is a self-hosted compiler pass.

## Decisions

### Use a remaining-member fold over closed nominal members

The fixture models three canonical nominal members, decodes a fixed array of candidate codes into a
normalized structural union, and loops through the candidates. A guarded same-member decision
suppresses one candidate; mutable seen flags and a nominal state struct subtract each first-seen
member exactly once and count later duplicates. The pinned sequence produces score `42`.

This algorithm is small enough to audit but composes modules, factories, structs, arrays, indexing,
operators, mutation, loops, unions, guards, matching, ownership, layout, and calls. A lexer was
rejected for this ticket because source bytes, slices, dynamic token storage, and typed failures
would make it a memory/runtime feature proposal rather than acceptance of the existing slice.

### Keep one canonical file-backed module fixture

The acceptance program lives as three `.silk` module fixtures: nominal member construction, the
coverage fold, and the entry module. Node-based compiler tests load those exact bytes into the
ordinary source resolver. The browser preset embeds the same module map because it cannot read the
filesystem, and a preset test compares every embedded byte with the canonical fixture files.

Publishing an example module from `@silklang/compiler` was rejected because it would expand the
public API for test-data sharing. Allowing unrelated copies was rejected because they could drift
into different programs.

### Add composed gates without changing phase contracts

The harness queries existing analysis artifacts and runs the same resolved closure through logical,
native, and WebAssembly execution. It adds parity and fresh-process determinism assertions around
the composed program; it does not add fields to HIR, MIR, layout, traces, or backend artifacts.

### Treat fixed cardinality as the evidence boundary

The program deliberately succeeds with `Array<T, N>`. Its demonstrated limitation is that a real
compiler pass cannot accept source-dependent candidate counts or grow its result set without
lexical slices, generic element abstraction, an owned dynamic sequence, and scoped allocation.
After acceptance passes, the project roadmap records that problem in Next without prematurely
freezing the exact memory-feature sequence.

## Risks / Trade-offs

- [Risk] The fixture is too synthetic to pressure composition meaningfully. → Keep the fold
  source-ordered, guarded, duplicate-aware, multi-module, and inspectable across every phase.
- [Risk] Browser and Node fixtures drift. → Compare the preset root, module identities, and exact
  bytes against the canonical file-backed fixture in tests.
- [Risk] A failing fixture tempts an opportunistic semantic change. → Stop and record the precise
  unsupported composition; any language change requires a separate proposal.
- [Trade-off] Fixed arrays let the acceptance program pass without memory infrastructure. → Treat
  the inability to generalize cardinality as the explicit finding, not as evidence that memory is
  unnecessary.

## Migration Plan

Add the fixture and gates without changing existing corpus expectations, then add the verified labs
preset. If the fixture exposes a compiler defect within already-specified behavior, fix that defect
under the existing contracts; if it exposes unsupported behavior, leave the change unimplemented
at that point and use the finding to shape a separate proposal. Removing the acceptance fixture and
preset fully rolls back the change.
