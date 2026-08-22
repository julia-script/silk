# Finding ledger: SLP-0002

| Id | Claim | Severity | State | Raised | Last touched | Evidence / resolution |
| --- | --- | --- | --- | --- | --- | --- |
| SLP2-R001-1 | Count state and access state need an explicit independence rule. | Low | CLOSED | r001 | revision 6 | Revised in `Worked language experience` and semantic step 3: cloning or dropping a non-last handle changes only the strong count and neither accesses `T` nor changes active access. |
| SLP2-R001-2 | Typed failure after successful construction needs a multi-frame cleanup trace. | Low | CLOSED | r001 | revision 6 | Revised in `Construction failure and last-handle cleanup`: the deeper clone drops `2 -> 1`, then the caller's final handle drops `1 -> 0`, cleans `T`, and releases storage. |
| SLP2-R001-3 | Conflict observation must state whether it mutates the already-active access. | Low | CLOSED | r001 | revision 6 | Revised in semantic step 6: `onConflict` observes without altering or releasing the active access. |
| SLP2-R001-4 | Exact target layout, alignment, and strong-count overflow mechanics need realization detail. | Medium | CLOSED | r001 | revision 6 | Delegated to OpenSpec because representation mechanics cannot reverse explicit caller-funded allocation or pre-mutation fatal overflow. |
| SLP2-R001-5 | Allocation-release authority and clean-then-release ordering need executable realization scenarios. | Medium | CLOSED | r001 | revision 6 | Delegated to OpenSpec because the observable last-handle order is fixed; only control-block metadata and lowering remain open. |
| SLP2-R001-6 | Count transitions, access behavior, and cleanup require evaluator/native/Wasm parity evidence. | Medium | CLOSED | r001 | revision 6 | Delegated to OpenSpec because cross-engine conformance verifies rather than chooses the accepted ownership model. |
| SLP2-R001-7 | Callback-loan escape needs diagnostics for direct, generic, Effect, and callable escape paths. | Medium | CLOSED | r001 | revision 6 | Delegated to OpenSpec because callback-scoped non-escape is fixed and only diagnostic and semantic-fact realization remain open. |
| SLP2-R001-8 | Local non-transferability needs an internal semantic representation before parallel execution exists. | Medium | CLOSED | r001 | revision 6 | Delegated to OpenSpec because local affinity is fixed; the compiler fact used to enforce it cannot reverse that direction. |
