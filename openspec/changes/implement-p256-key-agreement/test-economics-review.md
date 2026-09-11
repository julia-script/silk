Review scope: 991386ae75fe3037e70da1cde9dc71d91dbc3e67..5bf047366cdd04cab656336053c7c8c77b2be0af. Current Linear acceptance read, exact source/test/spec/provenance diff inspected. Read-only reviewer; implementer applied accepted findings.

test_inventory:

- StdlibNamespaceAcceptance.test.ts: one added analysis source/snapshot/test with two distinct diagnostic code/span assertions, consolidated from two.
- DriverNativeAcceptance through nativeCorpus: one new native program/process with six vector rows (reduced from seven), runtime scalar/SEC1 rejection, scripted rejection sampling and fresh acquisition.
- Driver.test.ts: one LLVM-to-Wasm execution using one complete RFC exchange.
- Codegen inspection and Zig oracle scripts are opt-in evidence tooling, not new default correctness tests.

justifications:

- Analysis protects consuming agreement and explicit exclusive Random provision. Neither runtime KAT nor existing namespace tests detects a signature that accidentally copies the owner or hides entropy. One snapshot now shares all compiler analysis; exact codes/spans distinguish both failures. No runtime ownership test or duplicate source remains.
- Native covers RFC initiating/responding public keys and shared result, independently pinned NIST COUNT0, scalar1/n−1 domain edges and a leading-zero-x result. Distinct scalar width/domain and peer length/prefix/coordinate/on-curve errors are checked through runtime API outcomes. The scripted provider rejects0 andn, then admits1; a second call acquires2. It checks draw counts, expectedfirstpublic, workingagreement and secondpublicdifference, so the expensive production rejection loop is exercised with the minimum two rejected domain endpoints. One shared executable compiles the source once, then checks every case. Private helpers or analysis cannot establish these public byte/runtime contracts.
- Wasm covers full-width field products, point arithmetic and fixed encodings on wasm32 using one RFC exchange. It is not the complete native matrix; one compiler/module/instance with full expected bytes is enough. Native success alone cannot detect target-specific lowering helpers or usize-width failures.

optimization_evidence:

- Accepted two concrete findings: collapsed two analysis snapshots into one; removed NIST COUNT24 from the permanent runtime table because it added no distinct boundary beyond COUNT0/RFC and the separate leading-zero fixture. The external oracle may retain COUNT24 at zero default-suite runtime.
- Revised diff preserves all ownership/Random code+span claims and repairs shifted vector indexes. Native table now contains exactly six rows; no repeated backend matrix, stress iteration, live oracle, timing assertion or fresh-process determinism test.
- Rejected-peer cases reimport a key because agreement consumes its owner; sharing an owner or constructing private storage would weaken the public API claim. Runtime negative branches belong in this one corpus process.
- Native corpus consumers do not replay these vectors through retired engines. No new feature test file/worker added.

findings: both material simplifications resolved in5bf04736; no remaining test-shape finding.
timing: completed matched controls/branch selections below.
verdict: approve.

Completed timing (all PASS; same machine/worker1/Homebrew LLVM PATH; SILK_NATIVE_FIXED_TESTS=false; native selection unset):

- Namespace fullfile: base9.621s process/5.86s body; branch12.192/7.95; delta+2.571/+2.09.
- Driver certificate control plus P-256 selector: base13.404/8.98; branch16.700/11.73; delta+3.296/+2.75.
- Native hmac-hkdf control plus p256 selector: base21.105/16.62; branch32.506/27.24; delta+11.401/+10.62.
- Summed added work: +17.268s process, +15.46s test bodies. This does not predict parallel full-suite wall time.
  Exact commands/results are /tmp/jul177-economics.json and /tmp/jul176-economics-base.json. Base selectors use the absent ChaCha feature alternative and branch selectors use absent/base P-256 alternative; the selected base controls are identically namespace3tests, certificateWasm1test and hmacnative1test. Both branches immediately followed the same freshly measured base trio. The earlier highly loaded181 controls were discarded. Residual shared-machine noise does not change the bounded-review conclusion.
  No fenced Silk examples were added to p256.silk, so no additional doctest program is omitted from this test inventory. Documentation collection/policy already runs over the shipped module set; this review measures the new explicit test-program boundaries.
  Benefit versus cost: approximately15.5s extra test work protects both admitted curve operations, every required scalar/point rejection class, rejection sampling and owner requirements, and the intended wasm32 arithmetic boundary. Removing more runtime cases would lose an explicitly distinct admission/encoding/domain claim. Two redundant costs were actually removed before this approval; no cheaper execution tier preserves the remaining runtime outcomes.
  Final verdict: APPROVE exact5bf047366cdd04cab656336053c7c8c77b2be0af. Both findings resolved, no remaining economics blocker.

## Final review scope

The dedicated reviewer extended APPROVE to b7c5bb41e16783e951abc6df24bfb315f4622ab5 after inspecting its sole follow-up: the prescriptive native fixture inventory changed COUNT0/24 to COUNT0. That documentation-only correction adds no runtime. The implementer copied this report without changing the reviewer’s findings or timings.

## Reproducible timing commands

Run each command from `packages/compiler` in the stated base or branch checkout, using Homebrew LLVM in PATH, `SILK_NATIVE_FIXED_TESTS=false` and no native corpus selection override. These are the exact recorded commands; the absent feature alternatives select identical existing base controls.

base analysis (9.621s process, exit0):

```sh
pnpm exec vitest run test/StdlibNamespaceAcceptance.test.ts --maxWorkers=1
```

base wasm (13.404s process, exit0):

```sh
pnpm exec vitest run test/Driver.test.ts -t 'bounded certificate decoding through LLVM-to-Wasm|ChaCha20-Poly1305' --maxWorkers=1
```

base native (21.105s process, exit0):

```sh
pnpm exec vitest run test/DriverNativeAcceptance.test.ts -t 'hmac-hkdf|chacha20-poly1305' --maxWorkers=1
```

branch analysis (12.192s process, exit0):

```sh
pnpm exec vitest run test/StdlibNamespaceAcceptance.test.ts --maxWorkers=1
```

branch wasm (16.7s process, exit0):

```sh
pnpm exec vitest run test/Driver.test.ts -t 'bounded certificate decoding through LLVM-to-Wasm|P-256' --maxWorkers=1
```

branch native (32.506s process, exit0):

```sh
pnpm exec vitest run test/DriverNativeAcceptance.test.ts -t 'hmac-hkdf|p256' --maxWorkers=1
```
