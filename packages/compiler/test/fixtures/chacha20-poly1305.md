# ChaCha20-Poly1305 fixture provenance

`chacha20-poly1305.json` SHA-256:
`916889966763785f7cf047c99e50a6b26a604df768c955c486320c86d54f227a`.

The fixture contains the exact inputs and expected outputs. The correctness suite reads these
committed bytes; it never generates expected results with the implementation being tested or host
cryptography.

| Cases                             | Source and distinguishing claim                                                                                                                                                                            |
| --------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `RFC8439-2.8.2`                   | [RFC 8439 §2.8.2](https://www.rfc-editor.org/rfc/rfc8439#section-2.8.2): full ciphertext and detached tag, including nonempty AAD and partial payload blocks.                                              |
| `RFC8439-A.1-1`                   | [Appendix A.1](https://www.rfc-editor.org/rfc/rfc8439#appendix-A.1): complete ChaCha20 counter-zero block.                                                                                                 |
| `RFC8439-2.5.2`                   | [§2.5.2](https://www.rfc-editor.org/rfc/rfc8439#section-2.5.2): clamped Poly1305 key and partial final block.                                                                                              |
| `RFC8439-A.3-5` through `-11`     | [Appendix A.3](https://www.rfc-editor.org/rfc/rfc8439#appendix-A.3): incomplete reduction, pad overflow, all-ones limb/carry, results equal to or above the modulus, and 130/131-bit reduction boundaries. |
| `boundary-0`, `-15`, `-16`, `-17` | Supplemental independent oracle values: key bytes `0..31`, nonce bytes `0..11`; AAD lengths `0/15/16/17`, payload lengths `0/63/64/65`, each filled with ascending bytes starting at zero.                 |
| `empty-0-17`, `empty-17-0`        | Empty AAD and empty payload independently, preserving the other input's length contribution.                                                                                                               |

Every AEAD fixture was verified with both the pinned Zig source and Python's independent OpenSSL
provider. Every Poly1305 fixture and the ChaCha20 block were also cross-checked with both providers.
The Zig source is revision `e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa`, specifically
[`lib/std/crypto/chacha20.zig`](https://github.com/ziglang/zig/blob/e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa/lib/std/crypto/chacha20.zig)
and [`lib/std/crypto/poly1305.zig`](https://github.com/ziglang/zig/blob/e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa/lib/std/crypto/poly1305.zig).

Reproduce from the repository root:

```sh
python3 -m venv /tmp/chacha-oracle
/tmp/chacha-oracle/bin/pip install cryptography==48.0.0
/tmp/chacha-oracle/bin/python packages/compiler/test/fixtures/verify-chacha20-poly1305.py /path/to/pinned/zig
```

The optional Zig argument must name a checkout at the exact revision; the script checks it before
using that checkout's `lib` with `zig build-obj`. Compile-time assertions avoid dependence on the
installed compiler's host-runtime interfaces. Recorded tool versions: CPython 3.9.6,
cryptography 48.0.0, OpenSSL 4.0.0 (14 Apr 2026), Zig compiler
`0.17.0-dev.1503+1f1bee62e`. Omitting the Zig argument runs only the independent Python check.

The negative cases are derived by flipping the last tag, AAD, nonce, or ciphertext byte separately.
They require `AuthenticationFailed` and preservation of the complete sentinel-filled destination.
Wrong key/nonce/tag widths, insufficient capacity, and ordered failure precedence are checked on
both public operations. A scalar invocation of the exact private preflight helper checks payload
lengths `274877906880` and `274877906881` without constructing invalid enormous slices.

The native corpus includes the canonical source text to access private component functions. It
does not duplicate or expose those functions as a public API. The public-import Wasm witness uses
only the 65-byte boundary fixture; native tests carry the full vector matrix. Structured ownership
analysis rejects overlapping input/output and output/tag borrows, asserting diagnostic codes and
spans from one shared snapshot.
