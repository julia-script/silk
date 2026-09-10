## Status and authority

[fixtures.json](fixtures.json) is the authoritative, byte-pinned fixture manifest for the
implementation follow-up. It contains 60 inputs with exact expected outcome class/reason and
retention observations where relevant. These are proposed decoder expectations, not results from
a shipped parser. No executable parser tests or runtime benchmark are added by this design.

The baseline is [RFC 7468 §5.1, Figure 1](https://www.rfc-editor.org/rfc/rfc7468.html#section-5.1),
retrieved from the RFC Editor's plain-text publication on 2026-09-10. The manifest embeds the
complete DER and PEM bytes as base64, so reproduction requires no live network dependency.
PEM uses the printed body, LF separators and exactly one final LF. DER is its base64-decoded body.

| Baseline | Bytes | SHA-256                                                            |
| -------- | ----: | ------------------------------------------------------------------ |
| DER      |   560 | `ff2d1b4ee9cd625a52ca49afa1974ea33f09ed35db8e554df0ec7d4c73a772f2` |
| PEM      |   814 | `5a835eeab533da03447de6c6e1fa6cf362f99894acbdd76df05b2ef5f3c5f803` |

The source identifies the example as a functional certificate. Decoder success does not establish
Web PKI validity: this fixture contains serial zero and historical validity dates. Mutated cases
deliberately do not regenerate signatures. Signature verification is outside this contract.

RFC 7468 is copyright © 2015 IETF Trust and the persons identified as its authors, under BCP 78 and
the [IETF Trust Legal Provisions](https://trustee.ietf.org/trust-legal-provisions.html). Preserve
that attribution and the applicable Simplified BSD notice when redistributing extracted Code
Components. The baseline is an attributed standards example, not generated Silk test material;
mutation recipes are local design material.

## Reproduction and expected outcomes

For each manifest entry, decode its named baseline. A patch replaces the half-open byte interval
`[start,end)` with the decoded `base64` payload. Apply patches from highest offset downward;
all offsets refer to the unmodified baseline. Empty patches mean identical input. Verify the
complete resulting input against `byteLength` and `sha256`, then call the named API with the
listed limit overrides and all other limits at their specified defaults.

`Success` means structural success and full owned retention only. `Malformed`, `Unsupported` and
`ResourceLimit` map directly to `DecodeClass`; their reason strings map to `DecodeReason`.
Expected observations are additional requirements, not replacements for exact byte preservation.
All rejected bundles must expose no certificate prefix. The two-block malformed-base64 case
specifically fails at certificate index 1 after a structurally valid first certificate.

| Group                    | Manifest coverage                                                                                                                        |
| ------------------------ | ---------------------------------------------------------------------------------------------------------------------------------------- |
| Valid schema             | Original v3; absent-version v1; v2 without extensions                                                                                    |
| Retained policy evidence | Negative/zero serial, unequal signature algorithms, duplicate extensions, unknown critical extension, opaque malformed extension payload |
| DER malformed            | Explicit defaults, v2 extensions, negative version, BOOLEAN, time/calendar, indefinite/nonminimal length, truncation and trailing data   |
| Unsupported              | Canonical version above v3 and a well-formed PUBLIC KEY PEM label                                                                        |
| BIT STRING               | Invalid unused-bit count, valid nonzero count, nonzero discarded tail bit                                                                |
| PEM                      | LF/CRLF/CR, horizontal whitespace, EOF after END, forbidden preamble/VT, marker errors and base64 padding errors                         |
| Atomic bundle            | Duplicate certificates retained, single-input trailing block, invalid second block, count and aggregate limits                           |
| Inclusive budgets        | Every DecodeLimits field at the exact baseline requirement and one below                                                                 |

The baseline accounts for 73 visited TLVs at maximum depth 6, three extensions and maximum
primitive content length 72. Certificate and aggregate DER size are 560. The outer Certificate is
depth 1; opaque key/signature bits and extension OCTET STRING payloads are not recursively visited.
The 72-octet signature BIT STRING content includes its initial unused-bit count octet.

Important retained baseline spans, all half-open: TBSCertificate `[4,474)`, issuer `[28,155)`,
validity `[155,187)`, subject `[187,314)`, SPKI `[314,405)`, inner signature AlgorithmIdentifier
`[16,28)`, outer signature AlgorithmIdentifier `[474,486)`, and signature payload `[489,560)`.
The public-key payload is `[340,405)`; its count octet is at 339, while the signature count is at 488. These spans include TLVs unless explicitly described as payloads.

## Follow-up verification requirements

The implementation must reconstruct the manifest cases without network access and assert their
typed outcomes and retained observations. Supplement them at the cheapest parser tier with schema
tables from the design, checked-size-overflow paths, arbitrary OID groups, SET ordering and bounded
open-value traversal. Do not infer complete malformed-input coverage from this finite manifest.

Owned-lifetime and allocation behavior additionally require a replaceable allocator: after success,
release or overwrite the input and observe unchanged owned results; inject refusal at each
allocation boundary reached by the valid one- and two-certificate fixtures, expect only the existing
`OutOfMemoryError` channel, and verify that no partial result or allocation survives. Allocation
ordinals depend on the implementation and therefore are not falsely pinned as input-byte fixtures.
Use bounded structural tests; no per-fixture native compilation, network, clock or verifier is needed.
