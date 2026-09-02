## ADDED Requirements

### Requirement: The WebAssembly heap reclaims released storage

Direct WebAssembly SHALL return released storage to the heap that issued it, so that repeated
acquire and release cycles keep a bounded heap. The bound SHALL hold for arbitrary interleaved
acquire and release, not only for nested ones: a release whose block is not the most recent
acquisition MUST still make that storage available to a later request. Reclaimed storage SHALL keep
the alignment guarantee the request was served under. Reclaim SHALL be driven entirely by the owner
that consumes the reclaim ticket, at the point ownership ends; the backend MUST NOT introduce a
scheduler, a garbage collector, a background task, a moving allocator, or compaction, and MUST NOT
publish a `free` operation to Silk.

Release SHALL be emitted for every cleanup plan that consumes a reclaim ticket, including one that
invokes no Drop hook. Where a plan carries both, the Drop hooks SHALL run before the storage they
own is reclaimed.

#### Scenario: Bound an interleaved allocate-and-drop loop

- **WHEN** a Wasm program repeatedly acquires several blocks and releases them in an order that is not the reverse of their acquisition
- **THEN** the final `memory.size` stays under a fixed limit that does not grow with the cycle count

#### Scenario: Reclaim a bare allocation drop

- **WHEN** an owner whose cleanup plan invokes no Drop hook and holds only a reclaim ticket is dropped
- **THEN** Wasm emits the release rather than nothing, and the block becomes available to a later request

#### Scenario: Run Drop hooks before reclaiming

- **WHEN** an owner's cleanup plan carries both a Drop hook and a reclaim ticket
- **THEN** the hook observes the storage before it is released, matching the order native LLVM produces

### Requirement: Release-count parity is distinct from memory parity

Wasm and native LLVM SHALL report equal acquire and release counts for the same program. That
property is a consequence of ownership-driven cleanup rather than of physical reclamation, and it
SHALL be pinned independently of any claim about how much memory either backend holds while running.
A test asserting equal counts MUST NOT be read as asserting equal or bounded memory, and a heap
bound MUST NOT be inferred from count parity.

#### Scenario: Agree on release counts

- **WHEN** the same allocate-and-drop program runs on Wasm and on native LLVM
- **THEN** both report the same acquire and release counts, whatever each backend does with the storage
