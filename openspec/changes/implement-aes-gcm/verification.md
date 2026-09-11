# AES-GCM verification

Work base: `991386ae75fe3037e70da1cde9dc71d91dbc3e67`.
The change adds ordinary Silk source only; it changes no compiler privileges or intrinsic inventory.

## Functional and diagnostic evidence

- Twelve committed fixtures: eight NIST CAVP selections and four pinned Zig boundary cases.
  Inputs, outputs, archive checksum, member/case selectors and a standalone independent verifier
  live in `packages/compiler/test/fixtures/aes-gcm/`.
- The shared native corpus covers both key widths; empty, AAD-only and multi-block inputs;
  15/16/17-byte plaintext and AAD; suffix preservation; wrong key, nonce, AAD, ciphertext and
  each of sixteen tag bytes; invalid key/nonce/tag lengths; and short output for both operations.
  It also extracts the actual private numeric admission predicate to exercise exact limits,
  one excess and `u64.MAX` without fabricating invalid slices.
- One structured analysis program rejects input/output and ciphertext/tag overlap, asserting
  `OWN0010` and source spans. The runtime corpus proves successful borrowing and target-neutral
  behavior; it is not duplicated in a feature-local native compiler test.
- One LLVM-to-Wasm witness covers a partial AES-256 block, its full tag, successful opening,
  and an authentication failure that preserves output. This is not a second full corpus.

Focused commands (from the repository root):

```sh
pnpm --filter @silklang/compiler exec vitest run \
  test/DriverNativeAcceptance.test.ts test/RuntimeSliceOwnership.test.ts test/Driver.test.ts \
  -t 'aes-gcm|AES-GCM' --maxWorkers=1
```

The three cases passed. After the comparison hardening described below, the two runtime cases
passed again. The initial isolated native case took 10.30 seconds within a 13.84-second invocation;
subsequent combined measurements ran alongside other agent work and are not comparable performance
measurements. Independent test-economics approval and the full repository gate remain delivery tasks.

## Generated-code inspection

Inspected the reachable AES functions using Silk's LLVM bitcode and Homebrew Clang 22.1.8 on
2026-09-10. For each of Darwin ARM64, GNU/Linux x86-64, GNU/Linux ARM64 and wasm32, examined
`debug`, `release` and `release-with-debug`. These correspond to Clang `-O0`, `-O2` and `-O2`;
the last mode retains debug information. This was an opt-in inspection, not twelve permanent
correctness-suite compilations.

The inspection root is `aesGcmWasmAcceptanceSource` from the shared fixture module. Emit it with
`Driver.compile`, supplying `packageName: 'aes-inspection'`, `stage: 'llvm-bitcode'`, the selected
target and optimization, `cache: false`, `SourceResolver.empty` and `NodeHeapObservation.layer`.
Use artifact kind `NativeExecutable` for native targets and `WebAssemblyModule` for wasm32 so
`main` and its reachable cryptographic functions are retained. `NativeObject` without explicit
export roots is insufficient: it can emit an empty object. Verify that AES function definitions
are present before inspection. Then lower each bitcode artifact with:

```sh
/opt/homebrew/opt/llvm/bin/clang --no-default-config --target="$AES_TARGET" \
  -S -x ir "$AES_BITCODE" "$AES_OPTIMIZATION" -o "$AES_ASSEMBLY"
```

GNU/Linux inspection ends at target assembly; it does not claim a configured GNU runtime/sysroot
or cross-target execution. Host native execution and the final LLVM-to-Wasm module are covered
separately by the shared tests. LLVM objdump 22.1.8 crashed on the linked Wasm module on this host,
so the Wasm instruction inspection uses Clang's target assembly output from the same bitcode.

Observed:

- `substitute`, `substituteWord`, `xtime` and byte multiplication use arithmetic/masks and fixed
  public positions, with no secret-indexed lookup table. Release byte multiplication is unrolled.
  Debug byte multiplication has a fixed eight-step loop; its checked counter cannot overflow.
- GHASH multiplication retains a fixed 128-step loop with mask-based reduction. Its only active
  loop branch depends on that counter. The same structure appears in Wasm `i64` operations.
- Key schedule branches depend on the public key width and word position. AES state addresses
  depend on round/row/column counters. CTR and GHASH block loops depend on public input lengths.
  Remaining bounds and overflow traps are unreachable under admitted lengths and byte invariants.
- Authentication computes the tag before any call that writes plaintext. The comparison consumes
  all sixteen byte differences before its result branch. Native optimized output may vectorize
  this reduction; Wasm uses an unrolled XOR/add chain and folds the final add into an OR, without
  an intermediate mismatch branch.

The original OR-of-XOR comparison was replaced with a bounded `u32` sum after the parallel AEAD
inspection found that Clang's Wasm optimizer can rewrite the OR reduction into early-exit byte
comparisons. Each addend is at most 255, so the complete sum is at most 4080 and is zero exactly
when every byte agrees. The sum form was inspected in all modes above. This source form is a
reviewed workaround for the observed optimizer behavior, not a promise about future compilers.

No timing experiment or functional vector establishes constant-time execution. Host/JIT
transformations, physical leakage, formal proofs, guaranteed erasure of stack/spill copies and
production cryptographic assurance remain outside this change's claims. The caller owns nonce
uniqueness and per-key usage policy.

## Delivery checks

Passed during implementation: strict OpenSpec validation, full build, typecheck, format check,
lint, generated documentation policy, the pinned Zig verifier, and the focused tests above.
Independent correctness review by investigator B and independent test-economics review by
investigator C approve implementation commit `8fc69cb6a5c90222513f3ff06540338fd6152258`.
The complete [test-economics review](test-economics.md) records the inventory, distinct failure
claims, simplification and matched baseline measurements. Fresh ordered typecheck, format check
and lint passed at that commit.

The first full `pnpm test` attempt failed after 245.537 seconds in five existing LSP tests across
AutoImportScale, ProjectWorker, Inspection and Server while concurrent full compiler suites
contended for the host. Four failures were timeouts and one received no expected diagnostic.
A subsequent isolated run of all four LSP files passed all eleven tests (343.26 seconds).
The failure log is `/tmp/jul175-full-test-attempt1.log`; isolated evidence is
`/tmp/jul175-lsp-isolated-branch.log`. A matching selection on untouched baseline
`991386ae75fe3037e70da1cde9dc71d91dbc3e67` reproduced the ProjectWorker timeout, both Server
failures (including diagnostics count zero) and Inspection timeout; AutoImportScale passed.
That baseline run took 411.81 seconds with four failures, one pass and six skipped tests; its
log is `/tmp/jul175-lsp-isolated-base.log`. These observations establish four baseline
failures under contention but do not substitute for a successful full gate. The coordinator is scheduling the full test retry;
`pnpm check` and `pnpm release:candidate` have not yet run. Draft PR handoff is coordinator-owned.

Economics review removed two supplemental exact-block fixtures: the NIST exact-block selections already falsify the same boundary claim. The 15- and 17-byte Zig cases remain for both key widths.
