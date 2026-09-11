# AES-GCM fixture provenance

The committed `vectors.json` contains eight NIST CAVP cases and six supplemental boundary cases.
No fixture oracle runs in the correctness suite.

NIST source: [GCM test vectors](https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Algorithm-Validation-Program/documents/mac/gcmtestvectors.zip),
downloaded 2026-09-10. ZIP SHA-256:
`f9fc479e134cde2980b3bb7cddbcb567b2cd96fd753835243ed067699f26a023`.
Each NIST entry names its archive member, PTlen, AADlen, IVlen, Taglen and Count.
The members are `gcmEncryptExtIV128.rsp` and `gcmEncryptExtIV256.rsp`; all selections use
96-bit IVs, 128-bit tags and Count 0. Selected (PTlen, AADlen) pairs are (0, 0),
(128, 128), (256, 0) and (0, 128), in bits, for each key width.

The supplemental cases use ascending key bytes starting at 0, nonce bytes 0 through 11,
plaintext bytes starting at 0, and AAD bytes starting at 32. Both plaintext and AAD lengths
are 15, 16 and 17 bytes for each key width. Expected ciphertext and tags came from the pinned
Zig implementation. This adds the byte boundaries absent from the selected NIST groups.

`verify.zig` independently verifies every committed ciphertext and tag using Zig's standard library
at revision `e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa`. Source references:

- [AES-GCM](https://codeberg.org/ziglang/zig/src/commit/e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa/lib/std/crypto/aes_gcm.zig)
- [software AES](https://codeberg.org/ziglang/zig/src/commit/e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa/lib/std/crypto/aes/soft.zig)
- [GHASH](https://codeberg.org/ziglang/zig/src/commit/e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa/lib/std/crypto/ghash_polyval.zig)

Reproduce with Zig compiler `0.17.0-dev.1503+1f1bee62e` and the pinned repository checkout:

```sh
git -C "$AES_ZIG_SOURCE" rev-parse HEAD
zig version
zig build-obj --zig-lib-dir "$AES_ZIG_SOURCE/lib" \
  packages/compiler/test/fixtures/aes-gcm/verify.zig \
  -fno-emit-bin -target x86_64-linux -mcpu baseline
```

The command passed on 2026-09-10. Compile-time evaluation avoids host compiler/runtime enum
mismatches with this historical library; the baseline target selects its software AES path.
It asserts the expected values rather than executing target machine code. The software AES
oracle uses tables, unlike Silk's algebraic S-box. Zig's authentication failure invalidates its
output; Silk's independently tested contract instead preserves the complete destination.

The algorithm and domain limits follow [NIST SP 800-38D](https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication800-38d.pdf),
sections 5.2.1, 6.3, 7.1 and 7.2, and AES follows
[FIPS 197](https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.197-upd1.pdf).
These functional checks establish neither constant-time execution nor production security.
