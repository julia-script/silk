## 1. Capture Real Runner Shapes

- [x] 1.1 Extend the cost harness with deterministic facts for every runner reached by `RunStaticEffect`.
- [x] 1.2 Record regions, outcomes, nested matches, calls, effect operations, loans, releases, cleanup, recursion, and estimated cloned size.
- [x] 1.3 Assert fresh-process runner classifications and relate them to entry-local call evidence.

## 2. Prototype Exit Remapping

- [x] 2.1 Add a test-only immutable remapper for a closed synthetic operation-region graph.
- [x] 2.2 Prove deterministic local/region substitution and return-to-continuation remapping.
- [x] 2.3 Add rejection controls for unknown locals, conditional/loop exits, cleanup, cycles, and multiple returns.

## 3. Decide the Production Boundary

- [x] 3.1 Compare the prototype vocabulary with every real runner classification.
- [x] 3.2 Select exactly one disposition: narrow production proposal, named prerequisite, or backend-only closure.
- [x] 3.3 Record the decision, evidence, limitations, and reproduction commands in a Wayfinder research note and roadmap.

## 4. Verify and Archive

- [x] 4.1 Run focused classifier/remapper and synchronous cost tests.
- [x] 4.2 Run repository checks and strict OpenSpec validation.
