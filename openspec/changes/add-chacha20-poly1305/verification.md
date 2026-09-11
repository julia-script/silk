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
being repeated with `PATH=/opt/homebrew/opt/llvm/bin:$PATH`. The remaining required repository
checks and independent correctness/test-economics review are pending. The coordinator will record exact reviewed commits and the clean comparative test costs
before handoff. No task is marked complete on the strength of this partial check record.
