# Separately scoped runtime follow-up

Proposed title: **Implement bounded HTTPS DNS-ID/IP-ID matching over explicit origin and SAN data**.
Estimate: **5 points**, standard-library feature, Low priority. Tracked separately as
[JUL-183](https://linear.app/juliaortiz/issue/JUL-183), created in Triage pending design acceptance
and technical admission. This is subsequent implementation, not runtime work in JUL-169.

The implementation can proceed once this design is accepted; URI, certificate, trust and TLS
integration are not prerequisites for the core API. JUL-164's URI API and JUL-182's implementation
of JUL-167 certificate decoding already exist at the refreshed base; neither supplies the planned
SAN descriptor adapter. Use `design.md` Decision 1 as the exact public contract
and `fixtures.md` v1 as the fixture oracle. No new intrinsic or external dependency is expected.

## Scope and acceptance

1. Add ordinary-source `silk.https_identity` with the proposed data declarations, `reference`,
   `verify` and `IdentityLimits.standard`. Verify canonical import, public types, precise Result
   arms and borrowing through one shared semantic analysis fixture with positive and negative
   lifetime cases. Estimated 1 point.
2. Implement ASCII admission, wildcard skipping, full SAN validation, binary IP equality and
   deterministic errors/limits. Verify core D/R/I/M/L matrix cases in one consolidated shared
   native acceptance program; prove accounting overflow through bounded arithmetic/structural
   evidence, not huge allocations. Estimated 2 points.
3. Add public doc comments, generated stdlib documentation and manifest registration; demonstrate
   explicit caller storage, binary IP inputs and the identity-versus-trust distinction. Verify
   documentation, package contents and release candidate. Estimated 1 point.
4. Run strict OpenSpec validation and required repository checks in order; obtain independent
   correctness and test-economics review. Measure the focused affected test cost against its
   verified base. Confirm no per-fixture compilation or duplicate native/Wasm matrix.
   Estimated 1 point.

Steps total 5 points as a single useful matcher delivery. They are not separate queue promotions.
No runtime checklist item is marked complete by publishing this design.

## Ownership boundaries

| Owner                                                                                                                         | Retained work                                  | Integration obligation                                                                                                                   |
| ----------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------- |
| [JUL-164](https://linear.app/juliaortiz/issue/JUL-164)                                                                        | Generic URI syntax and lossless components     | Preserve host classification and source data; HTTPS caller owns stricter admission and binary conversion                                 |
| [JUL-167](https://linear.app/juliaortiz/issue/JUL-167), implemented by [JUL-182](https://linear.app/juliaortiz/issue/JUL-182) | PEM/DER/X.509 envelope decoding                | Preserve every raw extension and duplicate OID; SAN GeneralNames interpretation remains with a separate identity-consumer adapter        |
| [JUL-168](https://linear.app/juliaortiz/issue/JUL-168)                                                                        | Certification path, trust and name constraints | Require independent identity result for the same leaf; use separate constraint encodings                                                 |
| [JUL-171](https://linear.app/juliaortiz/issue/JUL-171)                                                                        | TLS state/record/handshake composition         | Retain original origin, reject unsupported forms upstream and gate application data on identity plus trust plus handshake authentication |

URI/IP text conversion, bounded SAN adapter implementation and full DER fixtures belong to future
integration with those owners, not the five-point pure matcher follow-up. The SAN adapter is
separate identity-consumer work tracked as [JUL-184](https://linear.app/juliaortiz/issue/JUL-184),
a provisional five-point Triage intake before JUL-171 certificate integration; it must not be
mistaken for work already delivered by JUL-182. It consumes `Certificate.extensionCount` and
`Certificate.extension`, selects OID `2.5.29.17`, rejects duplicates, and owns bounded GeneralNames
decoding and caller-visible descriptor storage as described in design.md Decision 1. Adapter
integration tests must include a matching SAN followed by a structural failure, duplicate SAN
extensions retained by the envelope decoder, and malformed opaque SAN contents after successful
certificate decoding. HTTP integration must prove no DNS alias/address substitution and keep
port-based origin separation. Neither integration is implied to exist by this design or by the
pure matcher follow-up.
