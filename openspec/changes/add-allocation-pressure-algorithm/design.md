## Context

See proposal.md — Why. `silk.vector` already implements allocation-free empty vectors, geometric
growth, atomic replacement, initialized-element migration, and Drop in ordinary Silk source. The
compiler tests exercise those properties, but the public algorithm harness currently validates
only entry results and frontier diagnostics.

## Goals / Non-Goals

**Goals:**

- Add one readable algorithm whose normal workload necessarily allocates and reallocates.
- Make allocation behavior durable manifest evidence rather than an assertion hidden in one test.
- Exercise the public Vector/Allocator surface and all three execution engines.

**Non-Goals:**

- Expose `RawBuffer`, `Slot`, or allocator test doubles in the example.
- Turn the algorithm corpus into a general performance benchmark.
- Duplicate the existing allocation-failure ordinal sweep in user-facing source.

## Decisions

### D1: Use breadth-first search with an append-only Vector queue

The example searches an unobstructed 5×5 grid from cell 0 to cell 24. `QueueEntry` carries a cell
index and distance; a fixed visited array prevents duplicates. A cursor advances through the Vector
instead of removing its front element, which fits the current `make`/`append`/`get` surface and is a
normal compact BFS representation.

Visiting all 25 cells forces capacity transitions 0→4→8→16→32. A smaller graph would not exercise
enough growth; a custom linked structure would test more unfinished APIs than the allocator itself.

### D2: Put optional resource evidence under the manifest's expected result

The manifest gains an optional allocation record:

```json
{
  "allocation": {
    "acquires": 4,
    "releases": 4,
    "peakLive": 2
  }
}
```

Examples without allocation evidence remain unchanged. Keeping expectations beside the result makes
the resource contract reviewable and lets later allocation-sensitive examples reuse the same
schema.

### D3: Derive peak liveness from evaluator allocation tickets

The harness folds the ordered evaluation trace, adding tickets on `AllocationAcquire`, removing
them on `AllocationRelease`, and recording the maximum set size. It also rejects duplicate acquire,
unknown release, and unreleased final tickets when allocation evidence is declared. Exact resource
events are evaluator evidence; native and Wasm continue to prove the same observable algorithm
result while their existing allocator acceptance tests cover release lowering.

### D4: Keep failure injection out of the example

The BFS Effect handles `OutOfMemory` at its entry boundary so its type is honest, but it uses
`SystemAllocator` for the committed run. A quota allocator would make the source read like a compiler
fixture. Existing Vector and scanner acceptance tests retain exhaustive failed-growth coverage.

## Risks / Trade-offs

- [A future Vector growth policy changes the exact allocation counts] → Treat that as an intentional
  resource-contract change and update the manifest in the same review.
- [Trace-only release evidence misses a backend cleanup regression] → Keep the existing native/Wasm
  Vector cleanup tests; this example adds realistic integration pressure rather than replacing them.
- [An early target return avoids enqueuing all cells] → Terminate when the target is dequeued, not
  discovered; the chosen grid guarantees all 25 entries have been appended first.
