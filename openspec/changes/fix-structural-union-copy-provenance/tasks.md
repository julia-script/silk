## 1. Copy eligibility and MIR provenance

- [x] 1.1 Add focused characterization for `Slot.copy` and shared `RawBuffer.read` over an all-Copy structural union, including distinct member shapes and exact active-member payloads.
- [x] 1.2 Extend the canonical recursive MIR Copy decision to accept structural unions only when every member is Copy and cleanup-free.
- [x] 1.3 Verify and deterministically encode union Slot/shared-buffer copies with matching element, result, layout, tag, payload, and access provenance.
- [x] 1.4 Add negative verifier coverage for unions containing move-only or Drop-bearing members and prove evaluation/backends never receive them.

## 2. Engine parity

- [x] 2.1 Preserve the evaluator's canonical active union member and immutable payload across repeated Slot and shared-buffer copies without storage or cleanup mutation.
- [x] 2.2 Exercise native LLVM and direct Wasm tag/payload lane loads for all-Copy unions with different member layouts and zero-sized payloads.
- [x] 2.3 Add evaluator/native/Wasm parity for repeated aliases, bounds traps, later buffer move/drop, and absence of read-side allocation or cleanup.

## 3. Vector and pressure-program proof

- [x] 3.1 Extend Vector acceptance to read all-Copy structural unions repeatedly through shared borrows and continue rejecting a union with any move-only member.
- [x] 3.2 Restore the stack VM's single ordered `Vector<Step | VmDiagnostic>` result and fingerprint it after execution through shared `Vector.get` plus exhaustive matching.
- [x] 3.3 Update the VM differential, engine-parity, allocation-failure, and fresh-process determinism coverage for the unified event stream.
- [x] 3.4 Update pressure findings and roadmaps to mark structural-union copy provenance repaired and select no speculative self-hosting successor.

## 4. Acceptance and publication

- [x] 4.1 Run focused Slot/raw-buffer, Vector, structural-union, and stack-VM tests across evaluation, native, and Wasm.
- [ ] 4.2 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and `pnpm release:candidate` if public package contents or exports changed.
- [ ] 4.3 Validate the OpenSpec change strictly, synchronize its delta specs, archive it, and merge the completed branch to main.
