## 1. Verified Runtime Operations

- [ ] 1.1 Add target-neutral MIR operations for local-shared layout, initialization, clone, callback
      access, and opaque drop; verify MIR rejects mismatched provenance, malformed callbacks, escaping
      results, unavailable types, and reused or unconsumed initialization inputs with stable
      diagnostic identities and source provenance before any engine or partial artifact is entered.
- [ ] 1.2 Encode deterministic MIR inspection data without actor names, addresses, or backend field
      offsets; verify committed goldens are byte-identical across repeated analysis.

## 2. Evaluator Oracle

- [ ] 2.1 Model logical block identity, target layout, bounded strong count, independent access state,
      initialized `T`, and reclaim authority without JavaScript object identity or garbage collection;
      verify deterministic transition unit tests cover every logical state edge.
- [ ] 2.2 Emit bounded logical events for initialization, clone, access, conflict, restoration, decrement,
      payload cleanup, and release; verify nested conflict records no state change and last drop records
      cleanup before release.
- [ ] 2.3 Add evaluator acceptance cases for sequential mutation, clone/non-last drop during access,
      two-frame typed failure, affine movement through user state, construction exhaustion, and strong
      cycle retention; verify one analysis snapshot is shared per source program.

## 3. Native and Wasm Lowering

- [ ] 3.1 Lower planned control-block storage and every verified transition in native LLVM using
      non-atomic local operations; verify compare/trap dominates the clone count store and last drop
      invokes canonical payload cleanup before reclaim.
- [ ] 3.2 Lower the same operations in direct Wasm using its existing reclaim path; verify non-LIFO
      final release returns storage and no scheduler, collector, atomics, or background work appears.
- [ ] 3.3 Verify both backends keep conflict observation non-mutating, restore access only after normal
      callback return, preserve the language's no-unwind trap behavior, and use structural/runtime
      probes to prove clone and access perform no private allocation or reallocation and introduce no
      lock, atomic, scheduler, collector, background work, or source-spelled runtime actor.

## 4. Differential Acceptance

- [ ] 4.1 Add the local-shared programs to the designated native differential acceptance corpus and
      verify evaluator, native, and Wasm agree on results and logical ownership events without a
      per-feature native compile test.
- [ ] 4.2 Cover the four nested access combinations, clone/drop during access, last cleanup, typed
      failure, construction exhaustion, and strong-cycle non-release; verify evaluator and Wasm at
      every relevant failure ordinal and native only at boundary ordinals.
- [ ] 4.3 Unit-test overflow with a reduced private maximum and structurally verify compare-before-store
      in both backends; verify no correctness test loops to the public target maximum or uses timing,
      byte-count, or instruction-count assertions.
- [ ] 4.4 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`; verify every
      command passes and report any failure exactly before handoff.
