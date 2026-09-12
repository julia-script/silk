Review scope: JUL-178, verified base 991386ae75fe3037e70da1cde9dc71d91dbc3e67 through final 062b39eb694d5aa27e556d11031bd54070d05792. Dedicated reviewer is distinct from implementation and general correctness reviewers. Read the complete issue acceptance, issue-scoped diff, production actor, tests, nearby controls, fixtures, and OpenSpec contracts. Review was read-only; the implementer applied the consolidation described below.

test_inventory:

- RuntimeSliceOwnership.test.ts: one added structured-analysis test, one source snapshot, three diagnostic code/span assertions for consumed scalar ownership, private scalar access, and missing explicit Random provider.
- DriverNativeAcceptance.test.ts via support/corpus.ts: one added x25519 native entry, one compiled executable/process. Twelve standalone vector cases, scalar/peer width admission cases, and one scripted two-party generation/exchange block share that executable.
- Driver.test.ts: one added LLVM-to-Wasm execution using one RFC 7748 section 5.2 vector through the public API.
- Sixteen committed independent oracle fixtures remain intact. verify.zig is opt-in and adds no permanent runner process. No new test file, fenced documentation example, stress sweep, or benchmark runs by default.

justifications:

- Structured analysis
  reason_to_exist: enforce the public ownership/private representation/provider boundary without executing a curve operation.
  distinct_failure: consuming agreement must reject later scalar reuse (OWN0001); private scalar access must fail (SEM0028); generation without a Random provider must fail (SEM0071). Nearby generic ownership tests cannot detect a changed X25519 signature or exposed field.
  complexity_justification: one shared Analysis snapshot is the lowest tier that can falsify all three claims; assertions compare exact diagnostic codes and selected source spans. No compilation or native process is needed.
  optimization_evidence: inspected the single analyze call and assertions; no repeated source realization. A nearby ordinary box analysis is the common base/branch control, avoiding the unrelated entire forty-case file during measurement.
  measured_cost: base14.620s process/12.26s Vitest/6.17s body; branch16.579/14.79/6.95; observed increment+1.959s process/+2.53s Vitest/+0.78s body.
  benefit_vs_cost: the increment protects three API contracts that runtime known answers cannot prove, with one analysis boundary.
- Shared native acceptance
  reason_to_exist: prove RFC scalar clamping, coordinate masking/reduction, public derivation, consumed-key agreement, all-zero rejection, width errors, and fresh explicit Random acquisition through actual target-neutral runtime behavior.
  distinct_failure: section 5.2 vectors distinguish scalar/u byte order and the ladder; single iteration 9/9 is a bounded clamp boundary; high-bit peers distinguish required masking; p+9 distinguishes noncanonical reduction; twist u=2 distinguishes RFC acceptance from erroneous on-curve-only validation; scalar clamp equivalence protects constructor behavior. Low-order 0, 1, p-1, p, p+1 exercise distinct exceptional/reduction states, including reduction before zero-result rejection. Scripted generation checks two fresh 32-byte requests and independently expected public/shared bytes. Scalar and peer widths 0/31/33 distinguish empty, short and overlong inputs and each entry boundary.
  complexity_justification: one native corpus program shares source realization, analysis, compilation and subprocess startup. Byte inputs and outputs are fixed32; each ladder has fixed255 rounds. The matrix is a bounded list of distinct contract states, not a parameter cross-product or million-iteration stress test. Invalid-peer cases require a new owned scalar because agreement consumes it; bypassing that public API would weaken the claim. Full expected bytes prevent truncated-result checks from missing encoding errors.
  optimization_evidence: the initial runtime matrix redundantly replayed four RFC Alice/Bob exchange vectors already proven in generation. Removed standalone indices2..5 before source emission and main-call construction, eliminating six scalar-multiplication ladders. Generation now checks both shared results against the independent committed RFC shared secret, strengthening the former equality-only assertion. All sixteen oracle fixtures remain available opt-in. The twelve remaining standalone cases have distinct boundaries. No per-feature Driver.compile native test, fresh-process determinism, retry sweep, live expected-value generation, or repeated backend matrix was added. The native corpus entry is not replayed through the global interpreter/module-verification corpus.
  measured_cost: base36.750s process/34.73s Vitest/22.29s body; branch44.627/43.22/35.64; observed increment+7.877s process/+8.49s Vitest/+13.35s body.
  benefit_vs_cost: one compiled program protects mathematically and operationally distinct failures that static diagnostics cannot expose; the identified signal-preserving simplification is applied.
- LLVM-to-Wasm witness
  reason_to_exist: verify the expressly intended wasm32 portability of fixed-width curve arithmetic and slice addressing through public X25519 admission/agreement.
  distinct_failure: unresolved wide-integer runtime helpers or wrong Wasm lowering can fail while native arithmetic and analysis succeed. The existing certificate-decoding Wasm control does not exercise these curve operations.
  complexity_justification: one public RFC vector, one compile/module/instance, complete32-byte output assertion; it does not replay the native matrix. Constructor public derivation plus agreement are the two curve operations inherently required by this public API path.
  optimization_evidence: inspected the bounded standalone Wasm source and single test invocation; no duplicated analysis, backend variants, oracle subprocess, host entropy import, or stress loop. The native component/negative matrix stays native-only.
  measured_cost: base40.380s process/37.72s Vitest/31.89s body; branch48.147/42.16/16.30; observed increment+7.767s process/+4.44s Vitest/-15.59s body. Stage variance makes the body subtraction unsuitable as an estimate of added execution cost; see timing limitations.
  benefit_vs_cost: one intended-backend witness is the minimum execution that can expose target-specific arithmetic/helper regressions.

findings:

- Resolved material test redundancy in packages/compiler/test/support/x25519Acceptance.ts. The initial standalone Alice/Bob public/agreement cases duplicated the scripted generation exchange. Final062b39eb removes those source functions/calls and compares both generated shared results with the committed RFC answer. Six duplicate ladders are eliminated without deleting independent oracle fixtures or any distinct acceptance claim.
- No remaining material economics finding. All six final paired measurements pass, including execution of the consolidated native program at062b39eb.

timing:

- Exact commands from each isolated worktree's packages/compiler directory:
  - pnpm exec vitest run test/RuntimeSliceOwnership.test.ts -t 'constructs ordinary boxes with externally borrowed elements|enforces X25519' --maxWorkers=1
  - pnpm exec vitest run test/Driver.test.ts -t 'bounded certificate decoding through LLVM-to-Wasm|X25519' --maxWorkers=1
  - pnpm exec vitest run test/DriverNativeAcceptance.test.ts -t 'hmac-hkdf|x25519' --maxWorkers=1
- Base /tmp/silk-work-base-991386 at991386; branch /tmp/silk-work-jul178 at062b39eb. Same machine, PATH=/opt/homebrew/opt/llvm/bin:$PATH, SILK_NATIVE_FIXED_TESTS=false, no SILK_NATIVE_CORPUS_CASES, maxWorkers1. Each pair uses the same command and existing common control; only the branch adds the issue test. Every final command exits0. Analysis/native/Wasm tests respectively grow1→2,1→2,1→2.
- Summed process time91.750→109.353s: observed incremental selected default-suite work+17.603s. Summed Vitest duration84.71→100.17s: +15.46s. These sequential focused sums are not a prediction of the parallel full-suite critical path.
- Timing limitation: up to three focused reviewer workers ran while broad pools were stopped. Compiler/import versus test-body stage variance is substantial: the Wasm base body31.89s is slower than branch16.30s, while branch import25.55s exceeds base5.48s. Therefore the summed body subtraction60.35→58.89s (-1.46s) is not evidence of a speedup, and process+17.603s is an observed loaded-machine increment, not a precise isolated feature budget. The shape remains one analysis, one native program and one Wasm witness; the verdict does not depend on small timing precision or this accidental negative body difference. Repeating under a quieter condition would characterize performance, not alter the decision about remaining distinct acceptance claims.
- Earlier highly contended attempt is preserved separately: ownership pair105.049→107.039s passed, then the untouched base certificate Wasm control timed out60000ms (164.133s process). No branch Wasm/native was attempted then. Coordinator canceled the failing broad pools, and all six comparisons were rerun successfully above. The failed baseline attempt is excluded from final cost totals and never reported as a JUL178 failure.
- Raw final results /tmp/jul178-economics.json; logs /tmp/jul178-economics-{analysis,wasm,native}-{base,branch}.log. Historical failed attempt uses -contended suffix. No timings or compiler limits were changed in permanent tests.
- Benefit versus permanent cost: approximately18s of observed added selected-process work protects three public static contracts, bounded independent curve/admission/acquisition states, and the explicitly intended Wasm backend. The six duplicate ladder operations identified by review are removed. No remaining cheaper shared, narrower or lower-tier form preserves those distinct signals. No added doctest or opt-in oracle time is omitted from the default inventory.

verdict: approve exact062b39eb694d5aa27e556d11031bd54070d05792
