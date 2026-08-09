## 1. Manifest resource evidence

- [x] 1.1 Extend the algorithm manifest schema with optional expected allocation acquire, release, and peak-live counts
- [x] 1.2 Add a trace-folding resource verifier that rejects duplicate acquisition, unknown release, leaked final tickets, and mismatched counts
- [x] 1.3 Add focused harness tests for valid evidence and each malformed allocation trace condition

## 2. Breadth-first-search example

- [x] 2.1 Write the readable 5×5 grid BFS using `Vector<QueueEntry>`, a fixed visited array, an append-only cursor, and an explicit Allocator requirement
- [x] 2.2 Handle `OutOfMemory` at the example entry boundary without introducing a quota allocator or raw-buffer operation
- [x] 2.3 Add the example manifest with exact input/result, capability inventory, executable status, and four-acquire/four-release/peak-two evidence
- [x] 2.4 Add a README explaining the search, queue growth sequence, expected shortest path, and allocation behavior

## 3. Corpus integration

- [x] 3.1 Update the committed algorithm id/status inventory to include executable breadth-first search
- [x] 3.2 Assert that evaluation visits the allocation evidence while returning the committed result
- [x] 3.3 Execute the example through direct WebAssembly and a native process and require exact result parity
- [x] 3.4 Confirm the example imports physical `silk.vector` source and introduces no vector- or BFS-shaped compiler primitive

## 4. Verification

- [x] 4.1 Run the focused algorithm, Vector, scanner, and allocation acceptance tests
- [x] 4.2 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`
