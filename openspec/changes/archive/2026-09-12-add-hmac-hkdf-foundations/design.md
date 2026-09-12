## Context

See proposal.md for motivation. `Sha256` and `Sha384` already own inline streaming state and consume it at finish. Silk supports fixed arrays, borrowed slices, ordinary typed Result values, and owner-qualified inherent operations.

## Goals / Non-Goals

Use those existing semantics without compiler changes or heap allocation. Keep cryptographic composition inspectable and test the distinct algorithmic boundaries. The proposal defines the excluded protocol and security capabilities.

## Decisions

- Put the two HMAC actors in `silk/hmac` and two HKDF actors plus shared `OutputTooLongError` in `silk/hkdf`. Concrete actors preserve exact fixed-array result and PRK types; a generic hash abstraction is outside this slice.
- HMAC stores inner and outer SHA states initialized with the normalized key XOR ipad/opad. Long keys are hashed once. Finish consumes the inner state, updates the outer state with that digest, and consumes the outer state. This avoids retaining a message or requiring state cloning.
- HKDF extract calls HMAC with salt as key. Empty HMAC keys normalize to the same padded block as HashLen zero bytes, so empty salt needs no separate allocation or branch.
- HKDF expansion validates output length first. It copies the fixed 32/48-byte PRK once through ordinary postfix dereference (`prk.*`) because fixed-array references do not currently reborrow as slices; all variable-length inputs remain borrowed and streamed. It streams the previous digest, borrowed info, and one counter byte into HMAC, then writes only the admitted prefix to output. A usize counter avoids an increment overflow after block 255; conversion to u8 occurs only for admitted blocks.
- Use the existing shared native acceptance corpus for runtime known answers and boundaries. Fixed literals are pinned to primary RFCs and an independent SHA-384 corpus. Use analysis assertions for ownership/API rejection only where existing generic evidence does not cover the specific admission contract. Do not add per-feature compiler or native process harnesses.

## Risks / Trade-offs

- Incorrect block sizes, key normalization, or counter chaining → independent vectors for both hash families and segmented updates crossing their block boundaries.
- Partial writes on length failure → length guard precedes all output writes; verify sentinel preservation and exact typed error data.
- Maximum expansion costs 255 HMAC operations → run one maximum case per hash in the existing native corpus; avoid evaluator stress sweeps or redundant backends.
- HMAC retains derived secret state → document that zeroization and constant-time verification are not provided. Inherit the SHA bit-length domain, counting the initial key pad.
- Duplicate concrete composition bodies → accept the small duplication to keep exact fixed-size signatures without a premature generic hashing protocol.

## Migration Plan

Add the source modules and manifest entries, regenerate derived surfaces, and update the prescriptive reference in one change. No existing APIs are replaced or migrated.
