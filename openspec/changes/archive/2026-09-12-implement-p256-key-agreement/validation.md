# Validation evidence

Implementation base: `991386ae75fe3037e70da1cde9dc71d91dbc3e67`. Implementation is reviewable;
full-suite/release validation and independent reviews remain pending coordination. This document
records completed focused evidence without treating those pending gates as passed.

## Functional and ownership checks

- `pnpm --filter @silklang/compiler exec vitest run test/DriverNativeAcceptance.test.ts -t p256-key-agreement --maxWorkers=1`: one consolidated native program passes RFC5903 both directions, NIST P-256 COUNT0/24, scalar1/n−1, leading-zero shared x, length/scalar/SEC1/coordinate/curve rejection, scripted0/n/1 rejection, generated-key agreement and fresh subsequent generation. Runtime expectations are committed independently of the compiler.
- `pnpm --filter @silklang/compiler exec vitest run test/StdlibNamespaceAcceptance.test.ts test/Driver.test.ts -t P-256 --maxWorkers=1`: consuming-owner diagnostic OWN0001 and missing-Random diagnostic SEM0071 assert exact source spans. A single RFC exchange runs through LLVM-to-Wasm and has no host imports. Three tests pass.
- `python3 openspec/changes/archive/2026-09-12-implement-p256-key-agreement/tools/verify-oracle.py /path/to/pinned-zig`: all seven vector computations pass against exact copied Zig curve/field/scalar sources, plus Zig's imported empty root test. `fixtures.json` pins the source hashes, revision, compiler version and exact command. The installed compiler uses its matching standard runtime; the pinned whole stdlib cannot be mixed with its different builtin calling-convention enum.
- Documentation generation checks104 modules and reports no policy violations. The generated P256 page and index are committed with the actor.

Measured focused test-body cost before final additional length cases: native9.88s; combined
Wasm/analysis10.80s (two files, three tests) under parallel machine load. Earlier isolated Wasm
witness5.04s and ownership1.89s. The final native length-boundary rerun also passed (40.92s body,64.23s process) during a concurrent full compiler suite; the load-sensitive increase is not attributed to the two added length checks. These are observations, not timing assertions. The existing native
corpus receives one compilation regardless of vector count. Baseline comparison and independent
test-economics review are pending and must be recorded before final handoff.

## Generated code inspection

Reproduce after building the compiler:

```sh
node packages/compiler/scripts/inspect-p256-codegen.mjs /tmp/p256-codegen
```

The opt-in command retains executable main roots, emits bitcode, lowers native objects with Clang,
and writes disassembly/WAT for Darwin ARM64, GNU/Linux ARM64, GNU/Linux x86-64 and wasm32, each in
debug, release and release-with-debug modes (-O0, -O2 and -O2 respectively). Native execution is witnessed only on Darwin ARM64;
GNU evidence is source selection and emitted machine code. Native rootless object requests would
produce empty artifacts and are not used as inspection evidence.

The reproduction completed successfully for all twelve target/mode combinations. Its initial
native release lowering used -O3; all six optimized native artifacts were then relowered and
inspected at the compiler's actual -O2 setting. The committed script now selects -O2 directly.

Tool versions: Homebrew LLVM/Clang22.1.8, wasm2wat1.0.41, Node26.7.0. Native debug and
release-with-debug bitcode currently prompts Clang's warning that debug information has version0;
Clang ignores that metadata and still emits machine code. This is a limitation of debug metadata,
not an omitted debug-mode instruction inspection. LLVM objdump22.1.8 crashes on the linked Wasm
release-with-debug symbol metadata; wasm2wat successfully decodes the actual module instead.

Inspection covered field reduction/add/subtract/multiply, zero tests, inversion, point addition,
point/field selection, scalar iteration and indexing, plus admission/serialization boundaries.
Complete Renes–Costello–Batina Algorithm4 replaces exceptional Jacobian classification: an initial
masked implementation was rejected when x86 optimized code introduced a branch on intermediate
point equality. The final complete formula has no exceptional selector to transform.

All three optimized native targets preserve straight-line point addition and field/point mask
selection. Scalar multiplication's remaining conditional branch tests the public256-iteration
counter. Inversion tests its public256-bit exponent schedule. Field multiplication branches on
fixed loop indices; x86 unrolling additionally branches on the public limb-index parity. Limb and
scalar addresses derive from those counters, not scalar bits. Debug builds retain bounds,
divisor, conversion and arithmetic checks tied to public counters or mathematically bounded
values; field operations explicitly use wrapping arithmetic. No secret-value branch in the
complete point formula was found in the inspected output.

Optimized Wasm strips internal names. Its remaining seven-function call graph identifies field
multiplication, projective addition, scalar multiplication and inversion by their retained
signatures and operations. Point addition has no conditional branch; the scalar/exponent loops
use fixed counters and masks. Debug Wasm retains names and the source's public bounds checks.
These observations do not prove properties of later JIT lowering or every compiler transformation.

The inspected source avoids secret lookup tables and variable-magnitude arithmetic. Scalar
admission rejects invalid values with branches, and generation has probabilistic rejection work.
Those admission boundaries are distinct from the fixed secret-multiplication schedule. No claim
is made about formal constant time, physical/power channels, secure erasure, secret-copy retention,
production security or FIPS/CAVP validation.

## Repository gates

`pnpm typecheck` passed (18 tasks,15.389s on the final arithmetic design). `pnpm format:check`
passed before this evidence update. Initial `pnpm lint` exposed Effect script-boundary warnings;
the opt-in script now uses Effect services and scoped child processes. Its focused lint and the full lint rerun pass.
`pnpm test`, `pnpm check`, `pnpm release:candidate`, exact-diff correctness review and independent
test-economics review remain pending the coordinator's shared validation lane.

Economics review consolidated ownership and missing-Random diagnostics into one analysis snapshot and removed NIST COUNT24 from the default native matrix. COUNT0 and both RFC directions remain; COUNT24 stays in the opt-in independent oracle fixture manifest. This removes one analysis and two scalar multiplications without losing a distinct boundary claim.

The dedicated independent [test-economics review](test-economics-review.md) approves final implementation b7c5bb41 after removing one duplicate vector and consolidating two analysis snapshots. The matched selections added17.268s process/15.46s test bodies; this is not a full parallel-suite wall-time prediction. Full test/check/release gates remain pending coordinated execution.
