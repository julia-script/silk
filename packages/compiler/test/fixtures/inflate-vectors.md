# DEFLATE fixture provenance

`inflate-vectors.json` is the committed byte-level input oracle for `silk.inflate`.
`expected` is the complete output. Alternatively, repeat `expectedPattern` and truncate to
`expectedLength` bytes. No compression or oracle subprocess runs in the default test suite.

## Published source and independent verification

The published `ENCODED` vector and its `Hello, zlib!` expectation come from
[miniz_oxide's inflate tests at e2214d401a59e91537838cc16eba82454044044f](https://github.com/Frommi/miniz_oxide/blob/e2214d401a59e91537838cc16eba82454044044f/miniz_oxide/src/inflate/mod.rs#L336).
`published-zlib-hello` copies those 20 bytes; `published-fixed-hello` extracts its 14-byte
RFC 1951 payload, removing the two-byte zlib header and four-byte Adler-32 trailer.
The upstream project is licensed under MIT, Zlib, or Apache-2.0.

All twelve vectors were actually decoded and compared byte-for-byte using the independent Rust
`miniz_oxide` implementation at **e2214d401a59e91537838cc16eba82454044044f** (crate 0.9.1,
`adler2` 2.0.1), with rustc 1.96.0 and Python 3.14.6 on 2026-09-10. Python's zlib 1.2.12
also decoded every payload. Miniz validates complete zlib streams itself. Miniz does not expose
a gzip wrapper decoder: the verification script validates gzip headers, FHCRC, CRC-32, and
ISIZE using Python/zlib, then passes every member's exact raw payload and expected bytes to
Miniz. Concatenated gzip includes an empty member. This is two independent DEFLATE engines;
it is not a claim that Miniz independently validates gzip framing.

To reproduce (Git, Python 3, Rust/Cargo, and network access required):

```sh
git clone https://github.com/Frommi/miniz_oxide.git /tmp/silk-inflate-miniz
git -C /tmp/silk-inflate-miniz checkout e2214d401a59e91537838cc16eba82454044044f
python3 packages/compiler/test/fixtures/verify-inflate-vectors.py /tmp/silk-inflate-miniz
```

The script requires a clean checkout at the exact revision, builds an isolated temporary Rust
oracle, and asserts each result. Its final successful line reports all 12 vectors verified.
It neither regenerates nor edits fixtures. The committed bytes, rather than an encoder's
potentially version-dependent compression choices, define the fixtures.

## Constructed vectors

The remaining vectors were constructed for distinct protocol boundaries using Python zlib
1.2.12 `compressobj(level, DEFLATED, -15, 8, Z_DEFAULT_STRATEGY)` followed by `Z_FINISH`.
The level is 6 except for `stored-bytes`, which uses level 0. The low three bits of each
initial byte were checked to establish the intended BFINAL/BTYPE values.

| Vector                 | Construction and distinct claim                                                                                                                                                                                                               |
| ---------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `stored-bytes`         | `[0, 1, 127, 128, 255]`; stored LEN/NLEN and arbitrary literal octets.                                                                                                                                                                        |
| `empty-fixed`          | Empty source; fixed end-of-block without emitted output.                                                                                                                                                                                      |
| `fixed-overlap`        | `abcde` repeated 400 times; fixed-Huffman lengths and overlapping matches.                                                                                                                                                                    |
| `dynamic-text`         | `The quick brown fox jumps over the lazy dog.\n` repeated 64 times; dynamic code-length tables and matches.                                                                                                                                   |
| `dynamic-history-wrap` | `abcdefghij` repeated 3,300 times; 33,000 output bytes force the 32,768-byte circular history to wrap during match copying.                                                                                                                   |
| `gzip-optional-header` | `Hello, gzip!`; flags `0x1e`, zero MTIME, XFL 0, OS 255, three extra bytes `[17, 34, 51]`, name `fixture`, comment `header checksum`, both NUL-terminated, then little-endian low 16 bits of header CRC-32.                                   |
| `gzip-concatenated`    | Three independently checksummed members containing `first`, empty bytes, and `second`; flags 0, zero MTIME, XFL 0, OS 255.                                                                                                                    |
| `cross-block-overlap`  | Hand-packed nonfinal stored block containing `A`, then final fixed block with length symbol 257 (length 3), distance symbol 0 (distance 1), and end-of-block. Output is `AAAA`; the match references the preceding block and overlaps itself. |

Both constructed gzip forms wrap the level-6 raw payload, followed by little-endian
`CRC32(output)` and `len(output) mod 2^32`. `FHCRC` covers all header bytes before its own field.

Wire-format references:
[RFC 1951, DEFLATE](https://www.rfc-editor.org/rfc/rfc1951),
[RFC 1950, zlib](https://www.rfc-editor.org/rfc/rfc1950), and
[RFC 1952, gzip](https://www.rfc-editor.org/rfc/rfc1952).

Two hand-packed dynamic vectors additionally distinguish legal incomplete alphabets:
`legal-empty-distance-single-eob` contains only a one-bit end-of-block code and no distance codes;
`legal-single-distance` emits `A` with a one-bit distance alphabet that is never used. Both pass the
same pinned miniz_oxide and zlib verification above. All 12 positive vectors are independently
verified. The native corpus also includes small malformed coding vectors independently rejected
by zlib 1.2.12: reserved symbols, distance before history, oversubscribed/incomplete trees, missing
end-of-block, repeat overflow, and repeat without a preceding length. Wrapper corruption and limit
cases derive from these positive fixtures and the explicit public decoder contract.
