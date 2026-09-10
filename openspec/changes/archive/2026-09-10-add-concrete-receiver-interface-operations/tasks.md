## 1. Candidate authority

- [x] 1.1 Promote `ConformanceProof.implementedContracts` to the shared candidate authority for concrete receivers, recording in its doc comment that resolution and tooling both depend on its visibility, proof, and coherence filter, and verify existing hover contract tests still pass.
- [x] 1.2 Add the concrete-receiver candidate query selecting receiver-bearing operations of one name across the visible implemented applications, excluding own-binder operations and operations with no provider operand, and verify it returns nothing for a service, a `Copy`/`Drop` capability, and an invisible interface.

## 2. Receiver call resolution

- [x] 2.1 Add a `Conformance` method candidate carrying the finished interface-operation reference with a concrete provider, and route it to the existing interface-operation call completion so it shares the witness, specialization, failure channel, and requirement row.
- [x] 2.2 Consult the candidate query only where the nominal branch falls through to `Missing`, and verify inherent, inaccessible, duplicate, receiver-less, and callable-field outcomes are unchanged.
- [x] 2.3 Report two or more supplying applications at the member token before argument analysis, naming the receiver type and each qualified alternative, and verify arguments never select among candidates.

## 3. Diagnostics

- [x] 3.1 Add the ambiguous-supplied-operation diagnostic naming the receiver type and supplying applications, regenerate the diagnostic catalog, and verify the bound-receiver message is unchanged.
- [x] 3.2 Report a supplied operation named outside callee position as a member that must be called, rather than as a missing field, and verify the bound-method-value path keeps its own outcome.

## 4. Tooling

- [x] 4.1 Offer a concrete receiver's uniquely supplied interface operations in completion under the resolver's filter, excluding inherent-shadowed and ambiguous names, and verify a focused completion case.
- [x] 4.2 Resolve semantic identity and navigation to the selected operation declaration, and verify agreement with the bounded spelling.

## 5. Verification and reference

- [x] 5.1 Extend the existing method-call coverage with ordinary and effectful concrete-receiver calls, an exclusive receiver, ambiguity with qualified recovery, and an unrelated invisible interface, reusing the existing shared fixture rather than adding a test file.
- [x] 5.2 Add no native or differential-corpus leg: `Conformance` and the shipped `Bound` candidate converge on one `finishInterfaceOperationCall` reference and one unmodified receiver adaptation, so no production line below resolution changed and the existing `applied-interface-operation-calls` entry already exercises an effectful interface operation natively.
- [x] 5.3 Reverse the prescriptive reference boundary that a concrete receiver never reaches an interface operation, describe the precedence and ambiguity rules, and verify documentation checks and links pass.
- [ ] 5.4 Run `pnpm typecheck`, `pnpm format:check`, `pnpm lint`, `pnpm test`, then `pnpm check`.
