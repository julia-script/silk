# X25519 fixture provenance

The 16 committed cases select the two RFC 7748 section 5.2 multiplications, its one-iteration
case, and the section 6.1 Alice/Bob public keys and both agreement directions. Supplemental
cases distinguish scalar clamping, peer high-bit masking, reduction of p+9 to 9, admission of
twist coordinate 2, and all-zero rejection for 0, 1, p−1, p and p+1. No oracle runs in the suite.
The 1,000- and million-iteration stress cases are deliberately outside the default suite.

[RFC 7748](https://www.rfc-editor.org/rfc/rfc7748.txt), downloaded 2026-09-10, SHA-256:
`279ca0ecc5e92e2962e27b846986aeb74729d9dd34bd4a04a362f80dcb596ad3`.
Committed vectors.json SHA-256: `42c1dfc52050cd83cd9badd45c2ab7d728256c3a101340b27394fc048d817ba2`.

`verify.zig` independently checks every expected result or low-order rejection using the Zig
standard library pinned at `e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa`:

- [X25519](https://codeberg.org/ziglang/zig/src/commit/e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa/lib/std/crypto/25519/x25519.zig)
- [Curve25519](https://codeberg.org/ziglang/zig/src/commit/e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa/lib/std/crypto/25519/curve25519.zig)

The twist-u2 expected result comes from that oracle. Its curve polynomial
u³+486662u²+u is a quadratic nonresidue modulo p=2^255−19; it is intentionally accepted when
the resulting shared bytes are nonzero. Zig's `IdentityElement` failures correspond to Silk's
`AllZeroSharedSecret`; Silk does not call Zig's optional canonical-encoding rejection.

Reproduce with Zig compiler `0.17.0-dev.1503+1f1bee62e` and the pinned checkout:

```sh
git -C "$X25519_ZIG_SOURCE" rev-parse HEAD
zig version
zig build-obj --zig-lib-dir "$X25519_ZIG_SOURCE/lib" \
  packages/compiler/test/fixtures/x25519/verify.zig \
  -fno-emit-bin -target x86_64-linux -mcpu baseline
```

This command passed on 2026-09-10. Compile-time evaluation avoids host runtime/compiler enum
mismatches with the historical library. These are functional cross-checks, not proof of
constant-time behavior, physical erasure, protocol authentication or production security.
