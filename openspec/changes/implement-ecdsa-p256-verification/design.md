## Context

See proposal.md for motivation. The admitted JUL-177 base contains private eight-limb Montgomery field arithmetic and complete projective addition. SHA-256 and certificate AlgorithmView/BitStringView already exist.

## Goals / Non-Goals

Keep arithmetic at its existing owner, with a sibling public EcdsaP256Sha256 actor. Keep message verification usable without certificate parsing. No public prehashed operation, signing, trust-chain policy, new entropy or allocation requirement is introduced.

## Decisions

- `verify(publicKey, message, signature)` borrows byte slices and returns Result<(), P256SignatureError>. It checks message byte length at 2^61−1 before SHA-256, hashes exactly once, and forwards the fixed digest to a private verifier. A public digest API would invite message/digest ambiguity without an accepted use case.
- Errors distinguish InvalidLength (message domain), InvalidKey (SEC1 point admission), InvalidEncoding (DER/scalar admission), UnsupportedAlgorithm, InvalidParameters and AuthenticationFailed. No success or error owns input bytes.
- Strict DER needs only short-form lengths because the largest permitted signature is 72 bytes. Require exactly a SEQUENCE of two minimally encoded positive INTEGERs; remove a necessary sign-padding zero, then admit 1..n−1. High-s remains valid. Reject trailing bytes and all nonminimal lengths rather than using Zig's permissive parser as policy.
- Generalize private Montgomery reduction/multiplication by modulus and low-word inverse; keep fixed P-256 wrappers for existing secret operations. Private scalar-order inversion computes s^(n−2), then multiplies the generator and admitted point, adds, rejects identity, and compares r with affine x reduced modulo n. Zero scalar terms are valid. A generic public bigint actor would enlarge the contract unnecessarily.
- `verifyCertificate` accepts borrowed AlgorithmView and BitStringView values. Match id-ecPublicKey and full DER named-curve P-256 parameters, ecdsa-with-SHA256 with absent parameters, and both unused-bit counts zero. The wrapper does no ASN.1 extraction, certificate construction, chain validation or identity policy. Its metadata inputs are the retained views already supplied by the decoder.
- Native acceptance executes the canonical p256 owner source with a same-module private zero-digest seam; registered public imports are exercised by structured analysis and a small Wasm witness. This avoids exposing a test-only prehashed API. Independent NIST fixtures and pinned Zig e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa reproduce arithmetic; malformed DER policy and zero-digest validity have separate standard-based expectations because Zig differs there.

## Risks / Trade-offs

- Sharing modular arithmetic can alter secret code generation → rerun JUL-177 witnesses and inspect its generated multiplication schedule across supported targets/modes before delivery.
- Verification handles public values and need not promise secret-independent work → preserve the stronger existing secret agreement path and scope assurance explicitly.
- Mathematical zero-digest cases are impractical message preimages → exercise the private fixed-digest seam, without pretending a public message produces zero or weakening API semantics.
- Pure source verification has bounded but material compiler/runtime cost → consolidate native cases, keep one Wasm witness, and require independent test-economics review.

## Migration Plan

Add the sibling actor and manifest aliases, regenerate documentation and source embeddings together, and deliver after the JUL-177 prerequisite. Existing public agreement behavior and call sites require no migration.
