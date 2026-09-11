# RSA SHA-256 evidence

## Inputs and independent oracle

The committed supplemental fixtures use independently generated public keys and OpenSSL signatures.
They cover 2048-, 2049- and 4096-bit moduli, each with PSS and PKCS#1 v1.5. The exact key bit length,
signature representative and recovered encoding are checked by `verify-oracle.py`; private keys
are not stored. The 2049-bit key uses independently generated 1025- and 1024-bit prime factors,
with their product checked to contain exactly 2049 bits. This avoids a provider behavior that
rounded a requested 2049-bit key to 2048 bits; the original rounded fixtures were discarded.

- `fixtures.json` SHA-256: `5ee41e4edbb7db266da582187f9e1ef40ebd9e9c3f24c9df0d79287e1846304c`.
- `nist-encoding-fixtures.json` SHA-256: `f1fc15cf6dad3e9374ef54e2406da23b3a2c37a6c39ab9a8077a80142a939dfa`.

The NIST source archive is
[186-3rsatestvectors.zip](https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Algorithm-Validation-Program/documents/dss/186-3rsatestvectors.zip),
SHA-256 `8405aeb3572a4f98ed4b1a3ccb3f2f49e725462dd28ec4759d6a15d88855d19c`.
The selected CAVS 11.0/11.1 response members, exact member checksums, and one-based `Result`
record ordinals are retained in the JSON. Selection is SHA256/2048 bits; PSS salt length is 32.
The selected exponents are 0x49d2a1 and 0x10e43f, outside this issue's 65537 profile. Therefore
these cases exercise the private encoding checks after independent public exponentiation; they
do not falsely assert admission by the public key API. The generated 65537 fixtures exercise
complete public verification. OpenSSL independently confirms every selected NIST P/F result.

Reproduce both independent checks from the repository root:

```sh
python3 openspec/changes/implement-rsa-sha256-verification/verify-oracle.py /path/to/zig-source
```

The optional argument must be a Zig checkout at
`e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa`; the script verifies HEAD. Recorded tools are
CPython 3.9.6, cryptography 48.0.0/OpenSSL 4.0.0 and Zig 0.17.0-dev.1503+1f1bee62e.
The script copies the pinned RSA block and exact finite-field implementation into a temporary
module, changes only the finite-field import, and supplies the installed compiler's standard
library for runtime support and SHA-256. No RSA or finite-field algorithm is patched. The DER
parser declaration uses the host standard-library type but is never invoked by this oracle.
All five compatible RSA positive fixtures and the pinned finite-field tests pass.

The pinned PSS convenience wrapper has a value/pointer mismatch, so the oracle calls its public
`concatVerify` operation directly. Pinned PSS also assumes emLen equals signature width and cannot
handle the valid 2049-bit case. That case is verified by OpenSSL and the RFC 8017 contract, not by
claiming the pinned PSS routine accepted it. The pinned PSS padding routine does not inspect every
PS byte; standards-derived mutations independently require rejection of early and late padding
corruption. The compile-time route through pinned finite-field code fails a pointer/slice type
inference in the newer compiler; runtime evaluation of the unchanged arithmetic avoids that
compatibility issue.

## Coverage and resource boundary

One shared native corpus program owns complete verification, metadata/key rejection, retained
BIT STRING alignment, exact message-length admission, and strict padding checks. Numeric-only
message-length probes do not manufacture oversized slices. A separate public-import Wasm
witness performs one 2048-bit PSS verification. One structured analysis snapshot proves the
admitted key representation is private. No test performs network access or host cryptography.

Private arithmetic uses at most 128 32-bit limbs, fixed 512-byte encoding buffers, and bounded
public-data double/add multiplication with exponent 65537 (16 squares and one multiply). Inputs to
modular addition are reduced, so one subtraction suffices. Carries use widened 64-bit arithmetic;
all narrowing of arithmetic words masks off excess bits first. There is no signing, entropy
provider, compiler crypto primitive, bigint API, allocation or certificate trust decision.

Authoritative contracts are [RFC 8017](https://www.rfc-editor.org/rfc/rfc8017.html) and
[RFC 4055](https://www.rfc-editor.org/rfc/rfc4055.html). Certificate operations accept the decoder's
original AlgorithmIdentifier DER, BIT STRING bytes and unused-bit count. No certificate handle
or container reparse is required.

## Verification status

The final shared native corpus, public-import Wasm witness and private-key-representation
analysis snapshot all passed together: three existing files, three selected tests, 49.49 seconds
process and 43.53 seconds test work. This run includes the corrected exact 2049-bit fixtures,
complete NIST encoding checks, padding mutations, DER/key negatives and numeric length seam.
The earlier Wasm timeout under heavier concurrent load did not recur; no timeout was increased.
Full repository typechecking, formatting and lint passed. Generated documentation policy passed
for all 104 registered modules. Source formatting and strict OpenSpec validation passed.

A retained public PSS witness compiled to nonempty LLVM bitcode and target assembly for all
12 combinations of aarch64-apple-darwin, aarch64-unknown-linux-gnu,
x86_64-unknown-linux-gnu and wasm32-unknown-unknown with debug, release and
release-with-debug. Inspection of recovery, modular multiplication and DER decoding on native
and Wasm output confirms bounded stack storage, public-data loops and checks before indexed
reads. The optimized Darwin recovery/multiplication frames are approximately 5264/2912 bytes.
The exponentiation executes 16 squares and one multiplication after signature-width and
representative admission. GNU output was compiled and inspected, not executed. This is bounded
implementation evidence, not an independent production security audit.

Independent correctness review approved implementation commit
`1e78af36688a4911061006047d97660fe0176d06` after three additional private encoding
rejection checks: mismatched PSS digest, changed PKCS#1 digest byte and forbidden PSS high bit.
The existing native program with these checks passed (122.16 seconds process, 85.23 seconds
test work under concurrent load); no additional compiler pipeline was introduced.

Dedicated economics review approved `5a915463bee775b0352f7291ca38f38d263d45f9` after
removing the redundant default PKCS4096 execution and duplicate DER-length case, and adding
canonical duplicate PSS hashAlgorithm rejection. The complete fixture/oracle collection remains
unchanged. The final default native program executes five complete public verifications:
PSS2048/2049/4096 and PKCS2048/2049. Its complete review and timing caveats are in
`test-economics-review.md`. General correctness review approved this final test-only delta too.

The final three new tests passed in 52.37 seconds process/42.27 seconds test work after the
broad competing pools stopped. The comparable base selection contained no matching tests
and took 37.33 seconds in startup/import; this is an observed increment, not a precise prediction
of whole-suite wall time. Full tests/check/release remain delivery gates.
