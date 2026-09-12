Review scope: b7c5bb41e16783e951abc6df24bfb315f4622ab5..fef7ea56113ec1393bd539a50af9d6f0d15724b5, rebased final P-256 parent. The final head differs from inspected implementation8f3f3f2b287be3e69ea945712cf55b9857225d72 only by general-review.md. Reviewer investigator_a is distinct from implementer investigator_c and general reviewer coordinator. Current Linear acceptance read, exact source/test/OpenSpec/provenance/reference diff inspected. Read-only reviewer.

test_inventory:

- StdlibNamespaceAcceptance.test.ts: one new analysis program/snapshot proving borrowed inputs remain independently writable/reusable while result exists and verification needs no service provision.
- DriverNativeAcceptance via nativeCorpus: one new ECDSA executable, shared owner source for private numeric-length and synthetic-equation seams; runtime valid/invalid signature, strict DER, point admission and retained certificate metadata classes consolidated inside it.
- Driver.test.ts: one public-import ECDSA LLVM-to-Wasm KAT with no host imports.
- Existing177 runtime/analysis tests remain as the already-approved final parent (COUNT24 removed, ownership/Random snapshots merged).
- No new public documentation fence; oracle/inspection tooling is opt-in, not a default test matrix.

justifications:

- Analysis: mutation of all three borrowed byte arrays before dropping the result detects retained loans; a second call detects accidental input consumption, and the ordinary fn entry detects hidden provider requirements. Runtime signature-byte success cannot prove those ownership/requirement contracts. One shared snapshot is the cheapest tier; invalid one-byte data is intentional because this test asserts typing, not cryptographic outcomes.
- Native: the positive NIST KAT passes through the metadata adapter rather than adding another complete verification, while its alternate high/low-s version passes through standalone verify. The invalid NIST case and direct message mutation distinguish general equation failure and message binding. One synthetic valid prehash equation combines z=0/zero multiplication with x(R)>=n reduction; another rejects final infinity. These explicit numerical seams avoid impossible SHA preimage search or huge fake slices. DER cases cover sign, minimal positive encoding, indefinite/overlong length, truncation/trailing/extra integer, empty INTEGER and r/s=0/n domains. Metadata cases individually reject wrong OIDs, absent/NULL/explicit/wrong curve parameters and nonzero retained key/signature unused bits; an empty signature ensures early metadata failures do not pay scalar verification. Existing parent tests own the broad point-arithmetic matrix; new verification key errors only exercise length/prefix/on-curve mapping. Each claim runs in one native compile/process and uses fixed small inputs, with no error-ordinal stress or new public seam API.
- Wasm: one full NIST message/signature KAT exercises the new scalar-order arithmetic, SHA composition and byte framing through the intended backend. Parent P-256 Wasm agreement does not exercise scalar inversion modulo n or DER integration. This is one compiler/module/instance, without replaying the native rejection matrix.

optimization_evidence:

- Confirmed final parent reductions are retained after rebase; no reintroduced redundant COUNT24 runtime or duplicate177 analysis.
- Metadata success reuses the main positive KAT, z=0 and x>=n share one synthetic case, all runtime failures share the same native executable, and one static snapshot proves all borrow/service assertions.
- Private owner source inclusion accesses exact implementation seams without a duplicate handwritten algorithm or public API for tests. Fixed-size scalar boundaries are direct numeric calls. No live-network oracle, new worker file, timing assertion, fresh-process determinism or large parameter cross-product.
- Existing P-256 helper changes factor point admission into the same owner; paired cost controls will include P-256 to capture changes in the existing affected path.

findings: no material test-economics finding. Existing consolidations retain each distinct claim at its cheapest adequate tier.

timing:

- Same host, Homebrew LLVM PATH, maxWorkers=1, SILK_NATIVE_FIXED_TESTS=false, SILK_NATIVE_CORPUS_CASES unset. Each base immediately precedes its branch using the identical command. Exact roots, commands, statuses and process timings: /tmp/jul179-economics.json; reproduction: /tmp/jul179-economics.py. Baseline /tmp/silk-work-base-p256 is exact final177b7c5bb41; branch /tmp/silk-work-jul179 is exactfef7ea56. All six invocations passed.
- Full StdlibNamespaceAcceptance.test.ts: base4tests,20.831s process/11.27s bodies; branch5tests,59.102/52.24; added38.271s process/40.97s bodies.
- Driver.test.ts -t 'P-256 agreement through LLVM-to-Wasm|ECDSA P-256 verification': base1passed17skipped,21.172s process/4.88s bodies; branch2passed17skipped,28.368/20.63; added7.196s process/15.75s bodies.
- DriverNativeAcceptance.test.ts -t 'p256-key-agreement|ecdsa-p256-verification': base1passed335skipped,14.074s process/9.07s bodies; branch2passed335skipped,42.106/33.07; added28.032s process/24.00s bodies.
- Summed added measured work:73.499s process/80.72s test bodies. This measures default selected work, not predicted parallel full-suite latency. Logs: /tmp/jul179-economics-{analysis,wasm,native}-{base,branch}.log.
- The team's broad compiler pools were stopped before these pairs. Two other independent economics reviewers ran at most one focused worker each, and an unrelated user-owned t3 Vitest process remained active. This is a shared-machine comparison, not an isolated performance benchmark; startup/import variation makes sub-second delta precision inappropriate. No timing assertion or performance claim enters the correctness suite.

benefit_vs_cost:

- Analysis is the largest measured addition under this load. Its single source/snapshot proves the complete public borrow and provider contract using structured diagnostics; it has no backend or runtime pipeline to remove. Splitting it or replaying it per assertion would increase cost, while deleting it leaves ownership and service requirements untested. The two calls distinguish reuse from merely borrowing once.
- Native adds one compiler/process boundary and fixed small cases. Its required runtime outcomes cover strict DER, certificate metadata, equation edges and SHA message binding that analysis cannot falsify. Metadata success shares a KAT; zero digest and x reduction share one constructed equation. Parent tests already own general field arithmetic. No further case is interchangeable with a neighbor's rejection or arithmetic boundary.
- The single Wasm KAT pays for an intended backend that native cannot validate. Replaying the runtime matrix would be wasteful; removing this witness would leave newly introduced scalar-order arithmetic and SHA/DER composition unexecuted on wasm32.
- Documentation introduces no fenced program, and independent oracle/codegen checks are opt-in evidence. No default suite cost is hidden in a new feature worker or documentation compile. The added measured cost is proportionate to these three distinct compiler/runtime boundaries; remaining variation does not expose an avoidable test pipeline.

verdict: APPROVE exact fef7ea56113ec1393bd539a50af9d6f0d15724b5 against b7c5bb41e16783e951abc6df24bfb315f4622ab5. No remaining economics blocker. Full repository gates remain a separate delivery requirement.
