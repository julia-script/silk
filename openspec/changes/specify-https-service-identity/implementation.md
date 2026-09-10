# Separately scoped runtime follow-up

Proposed title: **Implement bounded HTTPS DNS-ID/IP-ID matching over explicit origin and SAN data**.
Estimate: **5 points**, standard-library feature, Low priority. Tracked separately as
[JUL-183](https://linear.app/juliaortiz/issue/JUL-183), created in Triage pending design acceptance
and technical admission. This is subsequent implementation, not runtime work in JUL-169.

The implementation can proceed once this design is accepted; JUL-164/167/168/171 implementations
are not prerequisites for the core API. Use `design.md` Decision 1 as the exact public contract
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

| Owner                                                  | Retained work                                  | Integration obligation                                                                                                                   |
| ------------------------------------------------------ | ---------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------- |
| [JUL-164](https://linear.app/juliaortiz/issue/JUL-164) | Generic URI syntax and lossless components     | Preserve host classification and source data; HTTPS caller owns stricter admission and binary conversion                                 |
| [JUL-167](https://linear.app/juliaortiz/issue/JUL-167) | PEM/DER/X.509 decoding                         | Bound decoding; expose complete leaf SAN data or structural failure, never prefilter invalid names                                       |
| [JUL-168](https://linear.app/juliaortiz/issue/JUL-168) | Certification path, trust and name constraints | Require independent identity result for the same leaf; use separate constraint encodings                                                 |
| [JUL-171](https://linear.app/juliaortiz/issue/JUL-171) | TLS state/record/handshake composition         | Retain original origin, reject unsupported forms upstream and gate application data on identity plus trust plus handshake authentication |

URI/IP text conversion and full DER fixtures belong to future integration with those owners,
not the pure matcher follow-up. Decoder integration tests must include a matching SAN followed
by a structural failure. HTTP integration must prove no DNS alias/address substitution and keep
port-based origin separation. Neither integration is implied to exist by this design or by the
pure matcher follow-up.
