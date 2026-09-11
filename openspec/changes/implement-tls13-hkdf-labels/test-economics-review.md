# JUL-181 independent test-economics review

Reviewer: investigator_b, distinct from implementer root and general reviewer investigator_a.
Base: 991386ae75fe3037e70da1cde9dc71d91dbc3e67.
Reviewed head: 9e5f18a14076edf7ff08337a8ab3a724945ab198.

## test_inventory

- `test/StdlibNamespaceAcceptance.test.ts`: one added analysis test, two diagnostic assertions.
- `test/Driver.test.ts`: one added LLVM-to-Wasm test with two compact Expand-Label operations.
- `test/support/tlsHkdfAcceptance.ts` and `test/support/corpus.ts`: one added native corpus program containing seven known-answer groups and framing/length rejection cases. Native-only scalar admission checks reuse the exact private SHA-256 domain helper; this entry is in `nativeCorpus`, not the general corpus used by module-verifier shards.
- No new test file, fresh-process determinism test, default oracle subprocess, timing assertion or backend matrix. Opt-in oracle and recorded code inspection add no default runtime. Generated documentation participates in the existing shared documentation gates; the module adds no fenced doc-comment example and therefore no new executable doctest.

## justifications

### Analysis

Reason to exist: enforce fixed digest widths and prohibit simultaneous secret/output aliasing at the new public entry points.
Distinct failure: accidentally accepting a SHA-384 transcript hash in the SHA-256 actor or weakening a shared/exclusive borrow signature would survive known-answer execution with valid disjoint inputs.
Complexity justification: one realized Analysis snapshot holds both invalid calls and asserts exact diagnostic codes/spans. No runtime or backend is needed.
Optimization evidence: both claims share parsing and realization; no repeated analysis, dynamic fixture sweep or new worker file. Nearby namespace tests exercise other actors and cannot falsify these signatures.
Measured cost: full existing file, base 3 tests to branch 4 tests: 46.36 to 46.18 seconds Vitest wall; test work 30.37 to 31.20 seconds (+0.83 seconds). Process time 51.349 to 49.712 seconds. Shared-load noise exceeds the wall delta.
Benefit vs cost: the single cheapest-tier snapshot protects two caller-facing compile-time contracts for less than one additional measured second of test work in this paired run.

### LLVM-to-Wasm

Reason to exist: witness both concrete digest-width wrappers executing through the intended wasm32 backend without host imports.
Distinct failure: wasm32 pointer/length framing or target-specific lowering of either SHA-256 or SHA-384 composition can fail despite Darwin native success.
Complexity justification: one source realization/compilation/module and two short known answers. No native boundary matrix is repeated.
Optimization evidence: initial review found four operations, including redundant SHA-384 message/from-hash derivations. The coordinator removed those two operations and unused imports/helper in 9e5f18a1. Final witness executes exactly two Expand-Label calls; transcript semantics remain in the native program. There is no second backend, fresh-process determinism or repeated compilation.
Measured cost: existing certificate witness control plus new TLS witness, base 1 test to branch 2: 50.16 to 58.54 seconds Vitest wall (+8.38); test work 37.77 to 48.73 seconds (+10.96). Process time 54.505 to 62.619 seconds (+8.114).
Benefit vs cost: this is the sole requested portable runtime witness for two concrete implementations. Keeping both widths is justified; a SHA-256-only witness would not exercise SHA-384's separate wrapper/codegen path.

### Shared native program

Reason to exist: validate exact TLS framing and derivation composition, including Hash(empty) versus empty context, raw prefixed/NUL-containing labels, maximum label/context encodings, digest-block crossing, both output domains and typed rejection before writes.
Distinct failure: generic HKDF's corpus accepts arbitrary info and cannot falsify an incorrect TLS prefix, u16 output field, u8 framing length, transcript hashing choice or wrapper admission order.
Complexity justification: one native compilation and one execution handle all cases. RFC SHA-256 and independent SHA-384 cases cover different concrete actors; empty and nonempty transcript derivations cover distinct semantics. Fixed output maxima 8160/12240 and their one-excess failures use actual valid allocations; SHA-256's unaddressable message boundary uses three scalar calls to the exact canonical admission helper, avoiding fabricated enormous slices.
Optimization evidence: no repeated analysis/compilation per vector; the shared native harness owns execution. Large output fixtures use Bytes allocation and fixed 32-byte independent fingerprints, avoiding tens of thousands of AST literals or committed output-array comparisons. Max-output hashing is a bounded total 20,400 bytes, not a sweep or growing cross-product. The maxima additionally protect the TLS wrapper's encoded length and public admission contract; the complete generic HKDF corpus is not copied. Maximum label/context, embedded bytes and multi-block cases are consolidated. Error branches assert typed requested/maximum values and preserved output. The optional oracle runs outside the default suite.
Measured cost: existing hmac-hkdf control plus new tls-hkdf entry, base 1 test to branch 2: 73.29 to 129.64 seconds Vitest wall (+56.35); test work 55.45 to 114.90 seconds (+59.45). Process time 80.879 to 135.977 seconds (+55.098).
Benefit vs cost: a single native compilation is the repository's lowest admissible runtime tier for this contract. The added measured minute under heavy shared load buys all framing/derivation and admission boundaries; replacing it with analysis would lose known-answer execution. Splitting it by API/hash/vector would increase compiler and process costs. The runtime loop sizes remain explicitly bounded.

## findings

- Resolved material simplification, `test/support/tlsHkdfAcceptance.ts`: initial Wasm fixture repeated SHA-384 deriveSecret and deriveSecretFromHash operations already owned by native. Removed both in reviewed head 9e5f18a1; final witness retains exactly two compact expansions.
- No unresolved material finding. No further case deletion preserves all distinct selected contract boundaries.

## timing

All measurements ran on the same host with separate clean worktrees, fresh Vitest processes, built dependencies, the same LLVM PATH and `--maxWorkers=1`. Root paused RSA focused execution. A full JUL-176 native worker and an unrelated external checkout's tests remained active; these are paired measurements under shared load, not isolated benchmark claims. Base/branch import/transform costs are of the same order; no decision-sensitive noise warrants a repeat.

Commands ran from each worktree's `packages/compiler` directory, with `PATH=/opt/homebrew/opt/llvm/bin:$PATH`, `SILK_NATIVE_FIXED_TESTS=false`, and no `SILK_NATIVE_CORPUS_CASES` override:

1. `pnpm exec vitest run test/StdlibNamespaceAcceptance.test.ts --maxWorkers=1`
2. `pnpm exec vitest run test/Driver.test.ts -t 'bounded certificate decoding through LLVM-to-Wasm|TLS HKDF' --maxWorkers=1`
3. `pnpm exec vitest run test/DriverNativeAcceptance.test.ts -t 'hmac-hkdf|tls-hkdf' --maxWorkers=1`

All six commands passed. Summed paired focused wall time: 169.81 to 234.36 seconds (+64.55 seconds); process time: 186.733 to 248.308 seconds (+61.575 seconds); measured test work: 123.59 to 194.83 seconds (+71.24 seconds). This estimates the added default test work for the new selected cases; the full default suite is parallel and its critical-path delta was not measured. No default oracle runtime was added. The shared documentation inventory checks the new module; its comments add no fenced executable example, so there is no extra doctest compilation. Catalog traversal overhead was not separately isolated.

Raw evidence: `/tmp/jul181-economics.json`, `/tmp/jul181-economics-{analysis,wasm,native}-{base,branch}.log`; exact runner/environment `/tmp/jul181-economics.py`.

## verdict

approve
