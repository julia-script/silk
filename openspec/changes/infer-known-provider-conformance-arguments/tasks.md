## 1. Known-provider constraint inference

- [x] 1.1 Extend generic call inference with a bounded worklist over direct interface constraints whose provider is already ground, and verify a known nominal provider binds unresolved value, failure-row, and requirement-row call arguments in structured analysis.
- [x] 1.2 Integrate the worklist with the existing visible coherent conformance proof and kind-aware unifier so it fills holes without replacing explicit or operand evidence, and verify agreement, conflict, missing-proof, ambiguous-proof, and genuinely reversed declaration/constraint-order cases report the expected structured facts or diagnostics.
- [x] 1.3 Preserve the selected conformance as the call's ordinary static constraint evidence while leaving executable representation unchanged, and verify realization reaches one finite specialization with no service requirement, runtime witness operand, or new HIR/MIR/backend form.

## 2. Semantic and dependent acceptance

- [x] 2.1 Consolidate positive and negative inference boundaries in an existing structured compiler acceptance source, including refusal to infer an unknown provider, use expected-result evidence, solve row constraints backwards, or erase a higher-ranked callable; verify one shared analysis snapshot distinguishes every case by semantic fact or exact diagnostic code and span.
- [x] 2.2 Re-enable the JUL-192 interface-only buffered context realization with a concrete nominal context conformance, and verify omitted provider/result/failure/requirement arguments infer at the public call while affine context consumption and temporary-session escape rejection remain statically enforced.

## 3. Language reference

- [x] 3.1 Update the prescriptive generic-call and interface documentation with the known-provider conformance rule, evidence priority, ambiguity boundary, and compile-time-only representation, and verify the documented buffered context example analyzes without explicit generic-row arguments or an erased callable fallback.
