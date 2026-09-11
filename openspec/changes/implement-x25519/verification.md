# X25519 verification evidence

Base: `991386ae75fe3037e70da1cde9dc71d91dbc3e67`.

## Functional evidence

The shared native `x25519` corpus covers all 16 committed oracle claims: the two RFC 7748
multiplications, one iteration, both public keys and agreement directions, scalar-clamping
equivalence, peer high-bit masking, p+9 reduction, nonzero twist-u2 agreement, and low-order
0/1/p−1/p/p+1 rejection. Separate calls check scalar and peer widths 0/31/33. A scripted Random
provider proves exactly one 32-byte draw per generation, two successive acquisitions, corresponding
public keys, and working agreement in both directions. It is test-only entropy.

A single structured analysis snapshot proves moved-owner reuse (`OWN0001`), inaccessible scalar
storage (`SEM0028`) and missing Random provision (`SEM0071`), including exact source spans.
There is no second runtime engine or per-feature native Driver test. The compact Wasm witness
checks one full RFC output through the public import, with no host imports. Both native and Wasm
passed after the arithmetic formulation below: 34.39 seconds invocation, 21.37 seconds test work
while other agents were active. The later ownership invocation passed in 27.65 seconds including
9.44 seconds test work under heavy external checkout contention. These are completion evidence,
not clean comparative economics measurements.

The independent Zig verifier passes all 16 cases at pinned revision
`e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa`; see the fixture README for source/corpus checksums,
exact inputs/outputs, tool versions and commands. No long-iteration or live oracle work is in the
correctness suite. Random's fatal-failure semantics are inherited directly: generation makes one
fillBytes call and has no handler, retry or fallback; existing Random tests own provider failure.

## Arithmetic and emitted output

Each canonical field element has 15 radix-2^16 limbs and one 15-bit limb. Convolution sums are
below 2^36; folding high limbs by 38 keeps low limbs below 2^42. Adding two canonical elements,
subtracting with redundant 2p limbs, and multiplication by 121665 all fit that same normalization
admission bound. Three fixed carry passes leave the number below 2^255; a mask selected subtraction
of p makes it canonical. Inversion uses the public exponent p−2. The ladder always processes 255
bits; secret scalar bits only form XOR/AND swap masks and never select addresses.

The initial ordinary checked u64 product failed Wasm lowering with
`UnsupportedFamily: arithmetic:__multi3`. Replacing private field arithmetic with existing
wrapping u64 operations avoids overflow guards and widened helper dependencies. The explicit
bounds prove no significant bits wrap. This required no compiler change or new intrinsic.

All twelve nonempty Driver-produced bitcodes were lowered to target assembly with Homebrew
Clang 22.1.8: Darwin ARM64, GNU/Linux x86-64, GNU/Linux ARM64 and wasm32, each in debug,
release and release-with-debug. The Driver uses `stage: llvm-bitcode`, retains `main` with
NativeExecutable (WebAssemblyModule for Wasm), and disables caches. The witness calls public
fromSecret and agree; hidden arithmetic functions remain present and are inspected directly.

Reproduction script: `/tmp/jul178-inspect.mjs`; input `/tmp/jul178-wasm.silk`. For each target/mode:

```sh
clang --no-default-config --target=TARGET -S -x ir program.bc -O0 -g -o debug.s
clang --no-default-config --target=TARGET -S -x ir program.bc -O2 -o release.s
clang --no-default-config --target=TARGET -S -x ir program.bc -O2 -g -o release-with-debug.s
```

Artifacts are `/tmp/jul178-TARGET-MODE.bc` and `.s`; function excerpts cover swap, normalize,
product, multiply, inverse, encode and agree. Optimized native swap uses scalar/vector mask
operations; address-overlap checks select vectorization based on public storage addresses, not
scalar bits. Wasm uses a fixed loop with i64 XOR/AND masks. Normalization uses fixed carry passes
and mask selection, with no data-dependent reduction loop. Multiplication uses fixed-index
multiply/add schedules; the remaining loop branches are public counters. Debug output retains
public array bounds/index arithmetic and conversion guards whose admitted values satisfy the
stated invariants. Optimized Wasm agree consumes all 32 output bytes before its single all-zero
result branch; its bounded sum cannot wrap (maximum 8160), and the last add may become OR.

The GNU artifacts were inspected, not executed on GNU hosts. This review does not establish
microarchitectural timing, future optimizer/JIT behavior, physical erasure, authenticated peer
identity or production readiness. Ownership bounds use and does not erase compiler-generated
scalar/intermediate copies. Callers still own secure Random provision and protocol key derivation.

## Remaining delivery gates

Documentation generation passed policy validation for 104 modules. Repository checks remain in progress. Full test/check/release runs
are serialized by the coordinator. Independent correctness and economics reviews, including a
matched base measurement, remain required before handoff. No complete task claim is made from
focused checks alone.

Economics consolidation keeps all 16 independent oracle fixtures but proves the four Alice/Bob public/agreement claims inside the scripted generation exchange. Both shared results compare with the pinned RFC bytes. This removes six repeated ladder executions from standalone cases while retaining fromSecret public derivation through the scalar-clamping fixture and raw agreement through RFC section 5.2. Final focused validation and comparative cost are recorded by the independent reviewer.
