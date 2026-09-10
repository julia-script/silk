# Implementation decomposition

JUL-166 delivers this design only. These independently estimated implementation issues were created
in the canonical Silk project and read back in native Triage. They must pass independent triage
before claiming; estimates are relative points, not elapsed-time commitments. They depend on the
parent profile's acceptance, recorded with native `blockedBy` relations. No new issue duplicates
JUL-162, and no follow-up includes TLS record/handshake or certificate trust machinery.

| Owner                                                  | Points | Deliverable                                                                | Additional native prerequisite                      |
| ------------------------------------------------------ | ------ | -------------------------------------------------------------------------- | --------------------------------------------------- |
| [JUL-175](https://linear.app/juliaortiz/issue/JUL-175) | 8      | Table-free AES-128/256, GHASH and GCM authenticated seal/open              | None                                                |
| [JUL-176](https://linear.app/juliaortiz/issue/JUL-176) | 5      | IETF ChaCha20, Poly1305 and authenticated seal/open                        | None                                                |
| [JUL-177](https://linear.app/juliaortiz/issue/JUL-177) | 8      | Bounded P-256 arithmetic, validation and ephemeral ECDH                    | Existing Random contract                            |
| [JUL-178](https://linear.app/juliaortiz/issue/JUL-178) | 5      | X25519 arithmetic, decoding and ephemeral agreement                        | Existing Random contract                            |
| [JUL-179](https://linear.app/juliaortiz/issue/JUL-179) | 5      | Strict ECDSA-P256/SHA-256 verification only                                | JUL-177 (native blocking relation)                  |
| [JUL-180](https://linear.app/juliaortiz/issue/JUL-180) | 8      | Bounded RSA public arithmetic, PSS and certificate-only PKCS1 verification | Delivered SHA-256                                   |
| [JUL-181](https://linear.app/juliaortiz/issue/JUL-181) | 3      | Both TLS label and secret-derivation hash variants                         | JUL-162 (Done; native relation preserves ownership) |

AES/GHASH/GCM and ChaCha/Poly1305 stay grouped because each useful authenticated API requires its
components; splitting them would expose partial unauthenticated substitutes. P-256 arithmetic is
shared with ECDSA, whose verification-only API is independently deliverable after agreement lands.
RSA public arithmetic and its two small encoding verifiers share one bounded implementation and
review surface. Labels are separate from generic HKDF and from the protocol state machine.

All seven require actor/source documentation, manifest/generated-reference integration, precise
errors, fixture provenance and independent review. Side-channel assurance remains explicitly
limited as described in the design. Implementation does not begin in JUL-166.

[JUL-167](https://linear.app/juliaortiz/issue/JUL-167) retains PEM/DER/X.509 decoding and parameter
retention. [JUL-168](https://linear.app/juliaortiz/issue/JUL-168) retains certification paths and
policy. [JUL-169](https://linear.app/juliaortiz/issue/JUL-169) retains identity,
[JUL-170](https://linear.app/juliaortiz/issue/JUL-170) trust sources, and
[JUL-171](https://linear.app/juliaortiz/issue/JUL-171) transport-independent TLS lifecycle.
The existing native JUL-166 → JUL-168/JUL-171 design dependencies remain intact. Do not block those
design-only issues on completed primitive implementations; their later implementation decomposition
must link the appropriate primitive prerequisites.
