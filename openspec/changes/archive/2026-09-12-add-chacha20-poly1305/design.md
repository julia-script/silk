## Context

See proposal.md for motivation. Work base is `991386ae75fe3037e70da1cde9dc71d91dbc3e67`. Ordinary fixed arrays, slices, u32 wrapping arithmetic and u64 arithmetic are sufficient. No external crypto provider or new compiler primitive is required.

## Goals / Non-Goals

Implement detached one-shot seal/open and their exact failure guarantees. No public unauthenticated cipher, standalone MAC, streaming AEAD, in-place aliasing, nonce generator or generic cryptographic interface is added.

## Decisions

- One public empty actor `ChaCha20Poly1305`, plus an `AeadError` enum. Slice widths permit typed argument rejection. Output capacity may exceed payload size; only the payload prefix is written. This fits callers that reuse buffers.
- Private ChaCha state uses 16 u32 words, 20 rounds of fixed-index quarter rounds, and wrapping addition/rotations. Counter zero derives the first 32 key-stream bytes for Poly1305; payload starts at one. Preflight avoids wrap; the loop does not increment a final exhausted counter.
- Private Poly1305 uses five 26-bit u64 limbs. Each block multiplies with a fixed 5-by-5 schedule and modulo-2^130−5 reduction. Products/sums fit u64. This avoids wide-integer compiler support and secret-indexed tables. A 16-byte buffer handles component tails; AEAD framing pads AAD and ciphertext independently to 16-byte boundaries before the two LE64 byte lengths.
- Seal preflights before writing either destination. Open computes and compares the full tag before decrypting. A failed comparison never writes plaintext; no scratch plaintext proportional to the message is needed.
- Compare lengths after widening usize to u64, with a literal u64 payload ceiling. No addition/round-up on potentially maximal slice lengths. Process chunks using remaining-length subtraction.
- Keep component RFC vectors and composed API/negative cases in one shared native corpus program. The test source incorporates the module source to reach private component functions without exporting them. A separate minimal import-based wasm witness verifies public module registration and wasm32 behavior. No live-network tests.

## Risks / Trade-offs

- Secret arithmetic depends on compiler lowering → inspect native debug/optimized and Wasm output and record remaining assurance limits.
- Copying oracle validation can weaken the API → Zig's undefined destination on authentication failure and assertion-only preconditions are deliberately not adopted.
- Limb arithmetic mistakes → RFC 8439 component vectors and pinned independent Zig outputs exercise carry, reduction, padding and block boundaries.
- Caller nonce reuse breaks confidentiality and integrity → document caller ownership on each public operation; TLS-specific accounting remains JUL-171.

## References

[RFC 8439](https://www.rfc-editor.org/rfc/rfc8439.html) is normative algorithm evidence. Independent computation uses Zig `e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa` [chacha20.zig](https://codeberg.org/ziglang/zig/src/commit/e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa/lib/std/crypto/chacha20.zig) and [poly1305.zig](https://codeberg.org/ziglang/zig/src/commit/e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa/lib/std/crypto/poly1305.zig). Fixture provenance and reproduction commands accompany the acceptance source.
