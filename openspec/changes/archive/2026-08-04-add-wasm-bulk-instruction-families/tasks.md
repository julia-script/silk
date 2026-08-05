# Tasks — add-wasm-bulk-instruction-families

## 1. SIMD

- [x] 1.1 Add uniform SIMD rows (arithmetic, comparison, bitwise, conversion, splat/extract-free
      families) to the instruction table under the `0xFD` prefix and extend `PlainMnemonic`
- [x] 1.2 Add `V128Const`, `Shuffle`, `SimdLane`, `SimdMemoryAccess`, and `SimdMemoryLane`
      variants with constructors, table entries, and freeze semantics
- [x] 1.3 Validate lane indices, shuffle selectors, `v128.const` byte length, and SIMD memarg
      alignment; unit tests per rule
- [x] 1.4 Add relaxed SIMD rows and any relaxed-specific typing rules

## 2. Threads

- [x] 2.1 Add `shared` to memory declaration/import options with the maximum-required check
- [x] 2.2 Add the atomic access table (`0xFE` prefix), `AtomicAccess`/`AtomicFence` variants,
      and wait/notify typing
- [x] 2.3 Enforce exact natural alignment for atomic accesses; unit tests including atomics on
      unshared memory

## 3. memory64

- [x] 3.1 Add `addressType` to memory and table declarations/imports; 64-bit limits validation
- [x] 3.2 Thread address types through the validator (memarg addresses, size/grow, bulk memory,
      table operations) and widen memarg offsets for 64-bit memories
- [x] 3.3 Unit tests: i64 addressing accepted, i32 address against 64-bit memory rejected,
      mixed-address `memory.copy` typing per spec

## 4. Emitters

- [x] 4.1 Binary: `0xFD`/`0xFE` prefixed encodings, `v128.const` and shuffle immediates, lane
      bytes, shared and 64-bit limits flags, u64 offsets
- [x] 4.2 Text: mnemonics and immediate syntax for all new forms; shared/`i64` declaration text
- [x] 4.3 Byte-stability: existing fixtures verify unchanged against the rebuilt package

## 5. Parity

- [x] 5.1 Extend oracle `FEATURES` with `simd`, `relaxed-simd`, `threads`, `memory64`
- [x] 5.2 New fixture modules: `simd`, `atomics` (shared memory), `memory64`; regenerate
      manifest; oracle validation and round-trip green
- [x] 5.3 Negative corpus entries per new validator rule (lane bounds, shuffle bounds, atomic
      alignment, shared-without-max, address-type mismatches) with oracle agreement

## 6. Docs and release

- [x] 6.1 Update README feature baseline and UPSTREAM.md feature list
- [x] 6.2 JSDoc for new public constructors and options
- [x] 6.3 Changeset for the feature release
