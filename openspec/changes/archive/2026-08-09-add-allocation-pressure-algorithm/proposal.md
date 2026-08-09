## Why

The algorithm corpus currently proves scalar, control-flow, slice, and fixed-array behavior, but no
user-facing example needs owned allocation. The newly shipped allocator and Silk-written `Vector`
need a recognizable program that exercises growth and cleanup through the same APIs users will use.

## What Changes

- Add a complete breadth-first search over a deterministic 5×5 grid under
  `examples/algorithms/breadth-first-search`.
- Use `Vector<QueueEntry>` as the BFS queue so visiting all 25 cells forces capacity growth through
  4, 8, 16, and 32 elements.
- Keep allocation behind ordinary `silk.vector` and the explicit `Allocator` requirement; the
  example will contain no raw-buffer operations or compiler-shaped allocation shortcuts.
- Extend algorithm manifests and the harness with optional allocation evidence, including exact
  acquire/release counts and peak live allocations during evaluation.
- Execute the example through evaluation, native LLVM, and direct WebAssembly with one exact
  shortest-path result and deterministic allocation evidence.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-algorithm-examples`: Add an executable allocation-pressure algorithm and resource
  evidence to the corpus contract.

## Impact

- Adds one example directory and updates the algorithm manifest schema and acceptance harness.
- Exercises `silk.vector`, `Allocator`, `SystemAllocator`, `OutOfMemory`, Drop cleanup, aggregate
  element migration, and three-engine execution without changing their public APIs.
