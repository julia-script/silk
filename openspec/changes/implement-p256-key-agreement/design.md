## Context

Work base 991386ae75fe3037e70da1cde9dc71d91dbc3e67 supplies Random and SHA/HKDF but no elliptic-curve implementation. JUL-177's accepted Linear contract and proposal define the gap. Certificate decoding recently landed and retains opaque key/signature material; it does not own arithmetic.

## Goals / Non-Goals

Goals: strict byte admission, allocation-free deterministic operations, one consumed ephemeral scalar per agreement, private reusable curve arithmetic, and reproducible native/Wasm evidence.

Non-goals: ECDSA verification (JUL-179), signing, general bigint, certificates, TLS state machines, external crypto, zeroization or a production-security claim.

## Decisions

- Public `P256` owns private 32-byte scalar storage. `fromBytes` returns `Result<P256,P256Error>`; `generate` returns P256 with exclusive Random; `publicKey` borrows; `agree` consumes. This prevents accidental repeated agreement through one owner while permitting caller-controlled deterministic scalar import. No mutable output buffer can expose partial secrets.
- Errors use a copyable P256Error enum: InvalidLength, InvalidScalar, InvalidEncoding, InvalidPoint. Rejection order is length, prefix, coordinate range, curve equation. Scalar import checks length then 1..n−1. No secret values occur in errors.
- Field arithmetic uses eight 32-bit limbs and bounded 64-bit products, with Montgomery reduction modulo the P-256 prime. Arithmetic masks select reductions. Complete projective coordinates (X/Z, Y/Z) avoid per-step inversion; final inversion uses the public fixed exponent p−2. Secret scalar multiplication processes all 256 bits with doubling, addition, and mask selection. Renes–Costello–Batina Algorithm4 handles equal, inverse and identity points without exceptional-case classification. Generated-code review found x86 LLVM rewriting masked exceptional Jacobian selection into a secret-dependent branch; complete addition removes that condition at its source.
- Field and point helpers remain private in the owning p256 module. JUL-179 can add a narrow verification operation at this owner and reuse the arithmetic; it must not publish a general integer facility or loosen secret multiplication.
- Reference precedence: RFC 8446 section 4.2.8.2 owns peer format; RFC 5903 section 8.1 and NIST ECCCDH own vectors. Zig e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa pcurves/p256.zig is an independent arithmetic oracle. Its broader SEC1 parser is not the admission policy.
- Tests consolidate public/secret KATs and admission boundaries into shared native fixtures. Analysis proves consumed ownership and explicit requirements; a small Wasm witness proves intended portability. No exhaustive vector sweep or timing assertion enters correctness tests.

## Risks / Trade-offs

- Compiler transformations can introduce secret-dependent behavior → inspect generated native optimization modes and Wasm, document precise limits, and never infer security from vectors.
- Pure source arithmetic can be slow → use bounded Montgomery arithmetic, measure focused costs, and keep stress/performance work opt-in.
- Scalar rejection has probabilistic work and fatal provider failure → preserve Random's established contract; do not claim a fixed-work generator.
- Secret copies can remain in compiler/runtime storage → owned lifetime controls usage only; no physical erasure guarantee.

## Delivery

Register the new source and generate embeddings/reference from canonical input. Publish all code, specs, docs and evidence in one change. There is no obsolete implementation to preserve or migrate.
