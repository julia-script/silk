# Verification evidence

Base: `991386ae75fe3037e70da1cde9dc71d91dbc3e67`.

## Functional and ownership evidence

- Strict OpenSpec validation passed.
- Shared native corpus `chacha20-poly1305`: all RFC component/composition vectors, supplemental
  boundary vectors, distinct authentication failures, buffer rejection, and scalar counter bounds
  passed. Every successful operation preserves spare capacity; failures preserve destinations.
- One public-import LLVM-to-Wasm execution passes the 65-byte boundary fixture.
- One analysis snapshot rejects overlapping input/output and output/tag arguments with `OWN0010`
  at each later exclusive borrow. The public entry points are ordinary synchronous functions with
  no service requirement or allocation dependency.
- Independent Python and exact-revision Zig oracle checks passed. See
  `packages/compiler/test/fixtures/chacha20-poly1305.md` for immutable fixture bytes, checksum,
  sources, tool versions, and reproduction commands.
- Focused post-review invocation: three existing test files, three selected tests, 57.16 seconds
  total (39.32 seconds test work), measured while repository checks and other independent agents
  were active. This is evidence of completion, not a clean comparative performance measurement.

## Generated-code review

Inspected compiler-emitted LLVM bitcode and Clang 22.1.8 assembly for
`aarch64-apple-darwin`, `aarch64-unknown-linux-gnu`, `x86_64-unknown-linux-gnu`, and
`wasm32-unknown-unknown`, using each shipped profile: debug (`-O0 -g`), release (`-O2`), and
release with debug (`-O2 -g`). `ToolchainPlan.compilationArguments` selects these optimization
levels. The inspection retained an ordinary `main` calling public `open`; native inputs came from
an external byte supplier to avoid constant-folding input data. The Wasm witness used an ordinary
supplier because arbitrary foreign functions are unavailable on that target. Emitted hidden
crypto functions remain separately inspectable.

Reproduction used `Analysis.makeRealized` with an object/no-runtime profile, retained `main`, and
`Analysis.codegen` in debug/release mode, writing `artifact.bitcode` and `artifact.ir`, followed by:

```sh
clang --target=TARGET -S -O0 -g program-debug.bc -o debug.s
clang --target=TARGET -S -O2 program-release.bc -o release.s
clang --target=TARGET -S -O2 -g program-debug.bc -o release-with-debug.s
```

Darwin additionally selected `-mmacosx-version-min=11.0.0`. Local inspection artifacts are named
`/tmp/jul176-bitcode-TARGET-MODE.s`; the driver script is `/tmp/jul176-inspect.mjs`. Clang warns
that this direct codegen path emits debug info with version zero and ignores that metadata. The
assembly inspection still covers the requested optimization modes; it makes no debug-metadata
correctness claim.

The review found and repaired a relevant optimizer transformation: the initial byte XOR/OR tag
reduction became per-byte early-exit comparisons in optimized Wasm. The final source sums the
16 byte XORs in `u32`; its maximum is 4080, so zero means every byte matches. Inspected optimized
Wasm evaluates every XOR and aggregate arithmetic before one mismatch branch (the final add may
become an OR). AArch64 and x86-64 combine SIMD/scalar reductions without an intermediate mismatch
branch. Debug output loops through all 16 bytes before testing the result. The branch deciding
whether to decrypt follows complete authentication.

ChaCha quarter rounds use fixed word positions, wrapping additions, XORs, and fixed rotations.
Poly1305 has five bounded 26-bit limbs and a fixed 5-by-5 product schedule; no integer division,
variable-size arithmetic, secret-indexed lookup, or secret-dependent reduction loop is used.
Optimized multiplication uses fixed loads/multiply-adds, and final modulus selection uses masks
or conditional selects. Remaining bounds checks, loop branches, and padding branches depend on
public lengths and indices. Secret arithmetic uses wrapping operations to avoid data-dependent
overflow guards. The full per-block product sum is below `2^59`.

This is a source and generated-output review for the recorded compiler and targets. It does not
establish microarchitectural timing, secure erasure, arbitrary future optimizer behavior, or a
production-readiness claim. Nonce allocation and per-key usage accounting belong to callers.

## Repository checks and independent review

Typechecking, formatting, and lint passed. The default-path full test run failed in five
existing CLI native build/run cases: Apple Clang cannot read the emitted LLVM bitcode
(`Unknown attribute kind (102)`). A minimal `main` returning 42 reproduces the failure without
crypto imports; the failing CLI run test passes with Homebrew LLVM on `PATH`. The full run is
being repeated with `PATH=/opt/homebrew/opt/llvm/bin:$PATH`. At revision `13e7b911`, the compiler parallel phase passed all 209 files and 2453 tests
(699.91 seconds). The subsequent native phase was stopped during execution after independent
review found an orphaned documentation example. This partial run is historical regression
evidence, not a complete or final-revision gate. The orphaned example was removed; public API
contracts and runtime implementation are unchanged.

On corrected revision `9f835ff11135e73ee5da30305d81a9d4442f15a0`, typecheck passed in 26.097
seconds, format:check in 8.191 seconds, and lint in 26.356 seconds. Documentation policy checked
104 modules without violations, and executable documentation completed before the compiler
parallel phase. During concurrent full-suite pools, existing SelectiveCatch and Suspendability
cases failed around the 60-second test timeout. The coordinator stopped the owned run
after those failures; the partial log is `/tmp/jul176-final-test.log` and the cancellation
record is `/tmp/jul176-final-cancelled.json`. This is an incomplete test gate, not a pass.
The same existing files passed in the earlier complete parallel phase; a fresh run with one
broad compiler pool is queued. `pnpm check` and `pnpm release:candidate` remain pending.

Independent general correctness review approved `13e7b911f55d0887e2620247b8d02ca1dd1ec82f`
and extended approval to `9f835ff11135e73ee5da30305d81a9d4442f15a0` after inspecting the
orphan-example deletion and generated digest update; source from the first import onward is
byte-identical.
The reviewer inspected the complete source, specification, documentation, ownership and runtime
integration, independently reproduced every committed fixture with Python cryptography 48 /
OpenSSL 4, and checked the counter schedule, Poly1305 bounds and authentication/write ordering.
There were no blocking findings. The generated-output conclusions remain bounded to the recorded
inspection; the independent reviewer did not repeat every assembly inspection.

The coordinator will record exact final check results and comparative test costs before handoff.
No verification task is marked complete on the strength of this partial check record.

## Test-economics review

The independent reviewer measured three matched base/branch selections with one worker and the
same LLVM PATH. Namespace ownership: 9.621 to 12.193 seconds process, 5.86 to 7.83 seconds test
work. Certificate plus ChaCha Wasm: 13.404 to 16.614 seconds process, 8.98 to 12.01 seconds test
work. HMAC/HKDF plus ChaCha native: 21.105 to 30.211 seconds process, 16.62 to 25.52 seconds test
work. Summed increments are 14.888 seconds process and 13.90 seconds test work; these are loaded
machine observations, not a parallel full-suite wall-time prediction.

The review retained the consolidated distinct AEAD/component boundaries, one native program,
one small Wasm witness and one shared ownership snapshot. A subsequent documentation audit found
that the unrendered example was both orphaned and syntactically invalid. Removing it resolves the
finding without adding a redundant doctest compilation; no new executable documentation example
remains. Final independent approval covers `9f835ff11135e73ee5da30305d81a9d4442f15a0`;
the complete dedicated report is [test-economics-review.md](test-economics-review.md).
