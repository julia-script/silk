## Context

The admitted base is `991386ae75fe3037e70da1cde9dc71d91dbc3e67`. The profile is recorded in JUL-166/175. No AES or GHASH exists. Integer operations, fixed arrays and exclusive slices are sufficient; no compiler privilege or provider is needed.

## Goals / Non-Goals

Provide detached, allocation-free seal/open with transactional failure. Keep secrets in bounded local state. Do not expose raw AES/GHASH, in-place aliasing, random nonces, TLS policy or a cryptographic security certification.

## Decisions

- One `AesGcm` actor accepts borrowed key/nonce/AAD/input and exclusive output/tag slices. A public semantic error union distinguishes key, nonce and tag widths, output capacity, algorithm limits and authentication failure. Both key sizes share the same implementation. No reusable secret context is needed by this initial API.
- Validate every public length before key expansion or destination mutation. Compare lengths after widening usize to u64, never narrow algorithm maxima to wasm32 usize. Output may exceed input; only the input-length prefix changes on success. The detached tag destination must be exactly 16 bytes.
- Expand AES keys into a fixed 60-word array. Compute the S-box by fixed-schedule inversion in GF(2^8) and the affine transformation. This is slower than table or hardware AES but avoids secret-selected memory accesses with a small, inspectable implementation.
- Implement GHASH with two u64 limbs and 128 fixed bit steps per block. Public message lengths determine the number of blocks. This avoids secret-indexed tables and general bigint arithmetic.
- Open computes and compares the full tag before CTR decryption. It needs no plaintext-sized temporary allocation and leaves the entire destination unchanged on failure. Seal has no fallible step after initial admission.
- Use pinned NIST response cases and Zig positive computation checks. Zig software AES uses tables and Zig GCM destroys rejected output, so neither behavior is copied. Runtime successes/rejections share one native corpus program; private length admission can follow the existing source-section fixture pattern to exercise huge numeric boundaries without fake slices.

## Risks / Trade-offs

- Compiler transformations can introduce secret-dependent work → inspect supported optimized native and Wasm output and record limits separately from functional vector results.
- Portable GF arithmetic costs more than hardware AES → keep default fixtures small; leave performance studies opt-in.
- Nonce reuse defeats GCM → document caller-owned nonce uniqueness and per-key usage accounting. JUL-171 owns TLS-specific bounds.
- Source lifetimes do not guarantee physical erasure → make no secure-erasure, FIPS or production-security claim.
