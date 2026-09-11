# Implementation evidence

Initial admitted base: b7d31aa1f34bd971cb55f5b60e629615ed976c61 (JUL-177 implementation). Final stacked review base: b7c5bb41e16783e951abc6df24bfb315f4622ab5, including the parent’s approved test consolidation and audit/documentation fixes. Rebase applied cleanly; no179 arithmetic or fixtures changed. JUL-177 remains the delivery prerequisite. This change reuses its field and point arithmetic, generalizes private Montgomery multiplication/reduction for the scalar order, and shares its SEC1 admission helper. It adds no compiler privilege or public bigint/prehashed API.

## Functional evidence

- Shared native program: NIST P-256/SHA-256 SigVer case3 valid (through retained metadata adapter), its n−s alternative, case0 changed-s rejection, changed-message rejection, strict DER boundaries and selected certificate metadata failures.
- One same-module private equation witness combines z=0, a zero generator term, and x(R)≥n reduction. One separate witness rejects the identity result. The canonical p256 source is read directly into this single native program; arithmetic is not copied into a test implementation or exposed publicly for testing.
- Exact private message-domain predicate boundaries cover 2^61−1, one beyond and u64 maximum without oversized buffers.
- One structured analysis snapshot proves pure provider-free calls borrow their inputs and return no retained input loans. One small registered-import LLVM-to-Wasm witness proves deterministic verification with no host imports.
- JUL-177 native and Wasm witnesses pass after shared arithmetic/key-admission changes.

Focused commands use PATH=/opt/homebrew/opt/llvm/bin:$PATH and --maxWorkers=1. Initial complete179 native program:35.80s wall/29.36s bodies. After shared admission cleanup, combined177+179 native:84.19s/71.77s bodies under concurrent machine load. Combined179 analysis/Wasm and177 Wasm:56.12s/39.53s bodies. These measurements are evidence, not timing assertions or comparative performance claims.

## Independent provenance

`fixtures.json` pins the official NIST186-4 archive, SigVer.rsp member and section/case indices with SHA-256 checksums. NIST provides message, affine coordinates and r/s integers; fixture conversion adds SEC1 prefix04 and canonical DER INTEGER/SEQUENCE framing without changing the mathematical values.

Run `python3 openspec/changes/implement-ecdsa-p256-verification/tools/verify-oracle.py /path/to/pinned/zig /path/to/zig-compiler` to reproduce against exact Zig revision e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa. An optional third argument is the downloaded NIST ZIP; it verifies archive/member checksums and selected case fields. It verifies source checksums, copies exact ecdsa/P256 arithmetic source into a temporary directory, and uses the installed compiler's standard runtime. Compiler0.17.0-dev.1503+1f1bee62e passed both NIST cases, the alternate-s signature and the synthetic identity fixture’s generator check. The full pinned standard library cannot be used with that newer compiler's changed builtin calling-convention enum; this does not replace the pinned cryptographic source.

Zig rejects a zero reduced digest and its DER admission is not this actor's strict encoding policy. Synthetic equation fixtures and malformed DER therefore have explicit mathematical/standard-based expectations; they do not claim agreement with Zig on those behaviors. The selected certificate policy follows RFC5480§2.1 and RFC5758§3.2.

## Generated output and remaining gates

The P-256 inspection script reproduces the agreement witness across DarwinARM64, GNU/LinuxARM64, GNU/Linuxx86_64 and LLVM-to-Wasm, each in debug/release/release-with-debug. All twelve combinations compiled and were inspected in `/tmp/ecdsa-p256-codegen` using LLVM22.1.8 and wasm2wat1.0.41. Native object lowering uses actual -O0/-O2 and --no-default-config. Across all six optimized native objects, pointAdd, reduceMod and selectors contain no conditional branches; multiply and invert each retain only their public fixed-counter branch. ARM64 mulMod has three loop branches; x86_64 has five, including public reduction-index parity from loop unrolling. No branch depends on a scalar bit, limb value or carry. Debug retains public loop/bounds and mathematically bounded conversion checks; wrappers pass the fixed field modulus and inverse. Optimized Wasm has the same bounded multiplication/inversion loops and public reduction-index parity branches, no conditional point addition and no indirect arithmetic calls. Debug Wasm retains corresponding named helpers and checks. Effective addresses use fixed offsets and public limb/iteration indices. The review covers this compiler/toolchain output, not host execution timing or a general compiler guarantee.

GNU/Linux artifacts were inspected as machine code; they were not run on a GNU/Linux runner. Native runtime evidence is DarwinARM64, and Wasm executes in the test host. Clang warns that generated debug metadata version0 is ignored in debug modes; this limits debug-information evidence without preventing inspection of generated instructions. It evaluates preservation of JUL-177's secret schedule; ECDSA verification itself operates on public values.

Typecheck passed18 tasks in1m17.597s. Format check passed. Generated documentation checks all104 modules with no policy violations; symbol links and signatures were inspected, and no new fenced code examples add default-suite work. Lint passed. Required remaining full checks, exact committed-diff correctness review, test-economics review and draft handoff are pending. Passing vectors are not a constant-time proof, erasure guarantee, CAVP/FIPS validation or production-security approval.
