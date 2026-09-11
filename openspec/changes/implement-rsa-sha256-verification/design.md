## Context

See proposal.md for motivation. JUL-180 is admitted at main
991386ae75fe3037e70da1cde9dc71d91dbc3e67. Certificate AlgorithmView retains raw algorithm DER,
OID and optional parameter TLV; BitStringView retains bytes and unusedBits. The decoder does not
admit RSA policy or verify signatures. SHA-256 is delivered ordinary source.

## Goals / Non-Goals

**Goals:** an owned admitted public key, explicit verification operations, complete encoded-message
checks and typed bounded failures. All operations process public data; variable work is permitted.

**Non-Goals:** private-key operations, signing, key generation, generic arithmetic, BER acceptance,
PSS-restricted key OIDs, certificate parsing beyond RSA key/algorithm fields, TLS state, path or trust.

## Decisions

- `RsaPublicKey` in `silk/rsa` owns at most 128 little-endian u32 limbs plus its exact bit and byte
  widths. Construction accepts canonical unsigned big-endian modulus/exponent bytes or admitted
  rsaEncryption certificate metadata. Retained key data has no input lifetime. An opaque owned key
  prevents subsequent calls from bypassing modulus policy; a raw signature API alone would repeat it.
- Public verification borrows the key, message and signature. `verifyPss` selects SHA-256,
  MGF1-SHA-256 and salt length 32 for TLS CertificateVerify or an already selected profile.
  `verifyCertificate` admits the existing decoder's signature algorithm and byte-aligned payload,
  then selects PSS or strict PKCS#1 v1.5. There is no raw PKCS#1 TLS operation and no public prehash
  entry point merely for testing. Messages are hashed exactly once, after public input admission.
- Private arithmetic uses bounded u32 limbs with u64 carry intermediates. Modular multiplication
  uses public-data binary double/add and modular addition; exponent 65537 needs sixteen squares
  and one multiply. This avoids a public bigint dependency and complex Montgomery setup. The
  fixed 4096-bit ceiling bounds its cost; focused measurements must confirm the supported budget.
- A private definite-length DER cursor admits only expected primitive tags and minimal lengths,
  tracks offsets within the supplied borrowed slice, and limits key/parameter envelopes before
  scanning. It parses RSAPublicKey and RSA algorithm fields only. It never traverses certificates.
- PSS decoding treats the recovered integer as emLen bytes, rejecting nonzero discarded leading
  bytes when a 2049-bit key has signature width emLen+1. MGF1 counter framing is big-endian; every
  padding byte, delimiter, salt, leading-bit constraint, trailer and recomputed hash is checked.
- Algorithm admission follows RFC 4055 rather than a blanket rejection of explicitly encoded
  defaults. SHA-256 AlgorithmIdentifier parameters admit absence or NULL, including nested MGF1.
  PSS fields must be ordered and unique. Effective default SHA-1/MGF1-SHA-1/salt20 are rejected;
  default or explicit trailer1 is accepted. Key rsaEncryption parameters require NULL; signature
  sha256WithRSAEncryption parameters accept absence or NULL.
- Errors use one bounded semantic enum with distinct invalid key, encoding, parameters, length,
  message-domain and signature failures. No error copies caller bytes or promises trust.

## Risks / Trade-offs

- Public arithmetic can consume measurable CPU → reject lengths and key policy before exponentiation;
  bound limbs/iterations and measure representative 2048/2049/4096-bit cases.
- Reference verifiers can be permissive → RFC 8017/4055 define negative expectations; Zig is only a
  positive arithmetic oracle where its profile agrees. Specifically do not inherit its last-PS-byte
  check or byte-aligned emLen assumption.
- Valid signatures can be mistaken for trusted certificates → keep operation names and docs explicit
  about the absence of path, identity and trust checks.
- Portable source is not complete target assurance → execute native and a small Wasm witness;
  inspect retained emitted functions for selected GNU targets without claiming cross execution.
