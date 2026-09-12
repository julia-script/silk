## Context

JUL-178 is admitted at base `991386ae75fe3037e70da1cde9dc71d91dbc3e67`. The existing exclusive Random service supplies complete fresh bytes and fails fatally. No X25519 module exists at the base; the graph does not index Silk source, so direct manifest/source inspection supplements discovery.

## Goals / Non-Goals

Provide the selected RFC 7748 operation, including public-key derivation and permissive mandated coordinate decoding. No Edwards conversion, public bigint API, on-curve validation, protocol/transcript state, TLS trust or native crypto provider is added.

## Decisions

`X25519` is one owned key with private clamped scalar and cached public bytes. `fromSecret(&[u8])` returns `Result<X25519, X25519Error>`, `publicKey(&X25519)` returns `[u8;32]`, `generate()` returns `X25519 ? &mut Random`, and `agree(X25519, &[u8])` returns `Result<[u8;32], X25519Error>`. Consuming agreement prevents accidental reuse of the same owner; caller-created copies of original scalar inputs remain caller policy. The owner is consumed even on rejected agreement. A raw-slice agreement API was considered but does not express ephemeral ownership as clearly.

Use private 16-limb radix-2^16 field elements held in u64, with a 15-bit top limb, modulo 2^255−19. This avoids requiring u128 or a new compiler operation. Normalize after each operation, with fixed carry passes and a mask-selected conditional subtraction of p. Multiplication uses 32 u64 accumulators and folds high limbs using 2^256 = 38 mod p. Canonical input limbs keep unreduced accumulators far below u64 overflow. Subtraction adds 2p before subtracting, avoiding secret-dependent borrow handling. Document the bounds beside each operation.

Use RFC 7748's 255-step Montgomery ladder with mask swaps and the 121665 formula. The inversion exponent p−2 uses a fixed public square/multiply schedule. Mask the encoded peer top bit, then reduce; do not call a canonical-only or on-curve rejection helper. Check all 32 result bytes using a bounded sum before returning a shared secret, avoiding the Wasm optimizer's early-exit OR-reduction pattern observed during AEAD work.

Generate through one `Random.fillBytes` call into a local 32-byte buffer. Reuse the same private clamping/public-key construction as deterministic import. No entropy retry or fallback is needed because clamping always yields an admitted scalar.

## Verification

Pin RFC 7748 input/output selections and the RFC text checksum. Verify them and supplemental masking, p+9, twist and low-order cases with Zig revision `e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa`. One shared native program covers distinct success/rejection and scripted generation claims. Structured analysis proves consumption, private storage and Random requirements. One Wasm witness exercises field arithmetic. Inspect nonempty generated code in all supported target/optimization modes; keep long iteration vectors out of default tests.

## Risks / Trade-offs

- Fixed limb arithmetic is easy to overflow if invariants drift: maintain explicit bounds and independent known answers.
- Compiler optimizations may introduce secret-dependent work: inspect final target instructions, retaining the actual cryptographic functions. Functional success alone does not prove constant time.
- Ownership does not erase scalar, stack or spill copies: document the physical-retention boundary and make no production assurance claim.
- Canonicalizing every field result favors simple bounds over peak performance; performance studies remain opt-in.
