Review scope: 991386ae75fe3037e70da1cde9dc71d91dbc3e67..13e7b911; current Linear acceptance read in full. Read-only review.

test_inventory:

- StdlibNamespaceAcceptance.test.ts: one new ownership test, one shared analysis snapshot, two code/span assertions.
- DriverNativeAcceptance via support/corpus.ts: one native entry, one compilation/process; seven AEAD known answers (seal/open), eight Poly1305 cases, one counter-zero block; runtime tamper/validation/preflight boundaries consolidated inside that executable.
- Driver.test.ts: one LLVM-to-Wasm portability execution using one 65-byte AEAD fixture.
- verify-chacha20-poly1305.py is opt-in provenance/oracle tooling and adds no default-suite test.

justifications:

- Analysis: separate later-exclusive-borrow conflicts protect seal output/tag overlap and open ciphertext/output overlap. Static ownership is falsified without running encryption; one snapshot shares parsing/resolution/typing across both assertions. Existing namespace tests do not call this public API, so cannot detect signature regressions. No redundant runtime alias test.
- Native: composed RFC bytes protect stream/MAC framing; boundaries combine MAC15/16/17 and stream63/64/65 in three fixtures rather than a Cartesian product; both-empty and each-independent-empty distinguish missing length/padding contributions. RFC component vectors exercise clamp, carry, pad overflow, reduction equal/above modulus and 130/131-bit cases not reliably reached by the composed random MAC keys. Separate tag/AAD/nonce/ciphertext mutations protect each authenticated input and complete output preservation. Ordered malformed widths/short capacity test each seal/open preflight guard. Exact scalar-domain max/max+1 calls the private predicate without huge or invented slices. Every operation shares a single native compiled program/process; assertions compare all bytes and suffix sentinels. The full source inclusion exposes private component seams only within the test source, not public API. No permanent oracle subprocess or stress sweep.
- Wasm: one public-import fixture crosses a stream block and covers i64 MAC arithmetic and wasm32 slice addressing. It has one compile/module/instance and compares the complete ciphertext/tag/open result; the native matrix is not replayed. Although primitive semantics are target-neutral, unsupported helper lowering and integer-width errors require this intended backend witness. Existing unrelated Wasm certificate decoding cannot falsify these arithmetic failures.

optimization_evidence:

- Inspected support generation, fixture loops and corpus consumers; nativeCorpus is consumed only by DriverNativeAcceptance and named BulkMemory selections. No global interpreter/direct-Wasm replay.
- No repeated Analysis snapshot for one source, per-feature native subprocess, fresh-process determinism, stress loop, timing assertion, live network or host-generated expected bytes.
- Seven AEAD selections cover distinct framing/length states; no duplicate exact boundary fixture identified. Eight Poly1305 selections carry individually named reduction/carry failures; collapsing to ordinary AEAD would lose those deliberate key/message states.
- Source/nativeSource split is representation only; nativeSource compiles once. Component and preflight calls avoid additional compiler processes.

findings: none material.
timing: completed matched controls/branch selection below.
verdict: approve (orphan example removed in final9f835ff1; see final addendum).

Completed timing (all PASS, same machine, maxWorkers1, Homebrew LLVM PATH, SILK_NATIVE_FIXED_TESTS=false, no SILK_NATIVE_CORPUS_CASES):

- Namespace fullfile: base9.621s process/5.86s body; branch12.193/7.83; delta+2.572s process/+1.97s body.
- Driver certificate control plus ChaCha20-Poly1305 selector: base13.404/8.98; branch16.614/12.01; delta+3.210/+3.03.
- Native hmac-hkdf control plus chacha20-poly1305 selector: base21.105/16.62; branch30.211/25.52; delta+9.106/+8.90.
- Sum process delta+14.888s; test-body delta+13.90s. This is measured incremental work, not a prediction of parallel full-suite wall time.
  Commands are pnpm exec vitest run FILE [ -t SELECTOR ] --maxWorkers=1 from packages/compiler. Exact selectors/files appear in /tmp/jul176-economics.json and /tmp/jul176-economics-base.json; logs are /tmp/jul176-economics-{analysis,wasm,native}-{base,branch}.log.
  The earlier borrowed181 controls were discarded because external machine load dropped: their51-81s timings were plainly incomparable. One bounded freshbase trio produced the comparable9-21s controls above. Other agents and the existing176 fullnative run remained active, so these are approximate loaded-machine costs; the decision is not sensitive to small noise.
  Benefit versus cost: approximately14s added test work buys one complete composed/component cryptographic runtime boundary and one specifically intended Wasm lowering witness, plus1.97s static API ownership protection. The fixed, small case matrix has no remaining signal-preserving cheaper execution shape; removing another tier or component boundary would lose an explicit acceptance claim.
  Initial Vitest-only verdict was APPROVE exact13e7b911. Superseded by the post-review inventory correction below.

Post-review inventory correction: REVISE pending resolution.
The module source contains a separate fenced-example //! block after a physical blank line. It is absent from generated chacha20-poly1305.md, so existing doctests did not collect/compile it. Extracting the exact fence and invoking the same Doctest.compile analysis boundary returns PAR0001/SEM diagnostics around the inline-if Success match arm. The 3.35s attempted measurement is not a passing doctest cost and is not added to default runtime. Either remove the orphan block, or attach it to module docs, repair its syntax, regenerate and prove the example. Author was notified; prior Vitest measurements remain valid but approval is withheld until this concrete finding is resolved.

Resolution and final approval: APPROVE exact9f835ff11135e73ee5da30305d81a9d4442f15a0.
Inspected13e7b911→9f835ff1: the orphan28-line example block is deleted, generated source/digest updated, and OpenSpec evidence corrected. All text from the first actual import onward is byte-identical; every runtime test is unchanged. The finalmodule has no new fenced example, so no added doctest program or cost is omitted. Existing collected API documentation remains complete and unchanged. The attempted orphan example analysis was diagnosis only and adds zero default runtime. The final added test-work measurement remains13.90s (+14.888s process across selected controls). No remaining economics finding.
