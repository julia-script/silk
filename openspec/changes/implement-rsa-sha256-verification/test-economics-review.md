Verified base: 991386ae75fe3037e70da1cde9dc71d91dbc3e67.
Reviewed initial implementation: 1e78af36688a4911061006047d97660fe0176d06.
Final reviewed implementation: 5a915463bee775b0352f7291ca38f38d263d45f9.
Reviewer: investigator_c, distinct from implementer/root and general reviewer investigator_b. Review remained read-only; root applied requested changes.

test_inventory:

- Added one rsa-verification shared native corpus program in support/rsaAcceptance.ts. Five complete public verification cases survive: PSS2048/2049/4096 and certificate PKCS2048/2049. Four independently recovered NIST encoding cases exercise accepted/rejected PSS and PKCS bytes without changing the admitted exponent profile. Bounded private seams cover parameter DER, padding, digest, key and exact message-domain rejections.
- Added one Driver.test.ts public-import LLVM-to-Wasm2048 PSS verification witness, with no host imports.
- Added one StdlibNamespaceAcceptance.test.ts structured analysis asserting SEM0028 on private key.width with exact span.
- Removed redundant complete PKCS4096 execution from the default matrix. Kept its fixture and independent oracle reproduction opt-in.
- Removed one duplicate leading-zero DER-length case and added one canonical duplicate PSS hashAlgorithm field case to satisfy an explicit acceptance claim.
- No executable documentation fences were added. New generated API declaration fences are signatures, not additional executable examples.

justifications:

- Native complete-verification cases:
  reason_to_exist: public admitted-key/message verification for both algorithms, selected minimum and maximum modulus bounds, partial-word/non-byte-aligned keys, and PSS's shorter emLen at2049 bits.
  distinct_failure: faulty public exponentiation/recovery, wrong modulus width, leading-byte conversion, padding offset or wrong public algorithm dispatch. Private recovered-encoding fixtures cannot prove those composed paths.
  complexity_justification: one source compilation/link/native invocation in the designated shared corpus. Inputs are37-byte messages, at most512-byte signatures and526-byte key DER. The math is bounded public-data work:17 multiplications per complete verification, each bounded by bit length and limb count. The single4096 case is necessary to witness the supported maximum storage/arithmetic boundary.
  optimization_evidence: no per-vector compiler/process, no backend cross-product and no fresh-process determinism test. Removed PKCS4096 because PSS4096 already proves maximum-width recovery while PKCS2048/2049 cover certificate padding at full-byte and partial-word widths. Native source is read from the canonical owner once; generic corpus consumers do not replay the matrix through unrelated engines.
  measured_cost: quiet matched selection: +15.04s process and +42.27s test bodies for the three added test boundaries; see timing caveat below.
  benefit_vs_cost: one native pipeline proves supported cryptographic behavior that parser/analysis tests cannot establish. Removing the redundant largest-width verification preserves the independent dimensions at lower permanent cost.
- Native encoding/admission seams:
  reason_to_exist: strict PSS/PKCS padding and full digest checks, exact parameter/default policy, alignment, DER/key shape and numeric message-domain admission.
  distinct_failure: early PS corruption accepted by the pinned oracle, forbidden high bits, mismatched PSS or PKCS digest, noncanonical or duplicate DER fields, invalid exponent/representative, and emLen overflow at2049 bits.
  complexity_justification: direct calls to private production helpers appended to the same canonical owner source avoid exponentiation for every malformed encoding and avoid a public test-only API. Numeric max-domain probes avoid huge buffers. There is no stress/ordinal sweep; mutation cases distinguish padding, delimiter, digest and trailer positions.
  optimization_evidence: digest arrays and admitted key are reused within the single program. Redundant badDer3 was removed because badDer2 reaches the same leading-zero-length guard. Added duplicateHash has independently checked canonical outer/inner lengths and fields[0],[0],[1],[2], ensuring duplication itself is tested rather than incidental bad framing. Existing final-digest/high-bit checks were preserved.
  measured_cost: included in the same native pipeline; no extra compile or subprocess boundary.
  benefit_vs_cost: required strict-admission evidence is substantially cheaper than manufacturing signatures and rerunning exponentiation for each failure. Standards/NIST bytes supply independent expectations; default tests invoke no live network or host cryptography.
- Wasm witness:
  reason_to_exist: actual LLVM-to-Wasm lowering of bounded RSA recovery and PSS verification through the registered public module.
  distinct_failure: target-specific integer/shift/array lowering or accidental cryptographic host import that native execution cannot detect.
  complexity_justification: one minimum-size2048 PSS verification, not the complete native matrix, in an existing Driver file.
  optimization_evidence: exactly one compiler/module/instance path; no subprocess matrix or per-feature determinism. The nativeSource/source split prevents the full negative matrix from entering this witness.
  measured_cost: included in the matched three-test aggregate below.
  benefit_vs_cost: one intended backend boundary warrants one bounded additional compilation.
- Structured privacy assertion:
  reason_to_exist: preserves the admitted public-key representation boundary so callers cannot manipulate width and bypass validated limits.
  distinct_failure: accidentally exporting width would allow states that valid runtime constructors never create.
  complexity_justification: one Analysis snapshot, one diagnostic-code and exact-span assertion; no lowering/runtime process.
  optimization_evidence: source and snapshot shared across assertions; generic private-field tests do not protect this actor declaration's field visibility.
  measured_cost: included in the matched three-test aggregate below.
  benefit_vs_cost: cheapest semantic tier for a safety-relevant public declaration claim.

findings:

- Resolved: removed redundant PKCS4096 default execution; retained independent oracle fixture.
- Resolved: removed duplicate badDer3 leading-zero-length rejection.
- Resolved: added canonical duplicate PSS hashAlgorithm runtime rejection to cover explicit acceptance; no exponentiation or new pipeline added.
- No remaining material relevance, complexity or measurement finding.

timing:
Both commands use PATH=/opt/homebrew/opt/llvm/bin:$PATH, the same machine and --maxWorkers=1, in isolated worktrees without switching/modifying the implementation to obtain the base.
Quiet command: pnpm --filter @silklang/compiler exec vitest run test/DriverNativeAcceptance.test.ts test/StdlibNamespaceAcceptance.test.ts test/Driver.test.ts -t 'rsa-verification|RSA' --maxWorkers=1
Base /tmp/silk-work-base-991386:355 skipped,0 test bodies;37.33s process,22.23s transform,36.15s imports; /tmp/jul180-economics-quiet-base.log.
Final5a915463 branch /tmp/silk-work-jul180:3 passed,355 skipped;52.37s process,42.27s test bodies,6.01s transform,9.76s imports; /tmp/jul180-economics-quiet-branch.log.
Incremental selected test cost: +15.04s process/+42.27s test bodies. Different transform/import timings show residual startup/cache noise; the42.27s of actual added test bodies is the useful cost measure, not a claim that default pnpm test increases by precisely15.04s. These are three newly added tests, so the matching base selection legitimately has zero test bodies while still loading the same three files. No executable doc examples add runtime. Whole-repository documentation/module-load amplification is not independently isolated here.
Earlier anchor control command added '|bounded certificate decoding|one import namespace' to the same filter: base1 passed53.23s/37.66s bodies; branch4 passed497.66s/433.83s. The branch ran while failed broad pools were still active; its Clang child received roughly8% CPU. This contention-affected comparison was discarded as permanent-cost evidence and prompted the quiet pair above. Logs retained as /tmp/jul180-economics-{base,branch}.log.
Benefit versus measured cost: approximately42.3s of new focused test work buys one shared public RSA verification matrix plus mandatory strict encoding/parameter rejections, one actual wasm32 portability boundary and one opaque-key diagnostic. The suite does not repeat compiler/native work per input, and removal of the redundant largest-width case and duplicate DER guard demonstrates actual simplification. There is no smaller execution tier for the remaining runtime claims; no additional timing run could justify deleting a required width/encoding boundary.
verdict: approve exact5a915463bee775b0352f7291ca38f38d263d45f9
