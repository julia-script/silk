# Resolution audit a001: SLP-0004 revision 34

Proposal: `proposals/0004-silk-native-testing/proposal.md`
Proposal revision: 34
Proposal digest: `525a2f9968eeaa15dd5f38ffd3779faae142c9edf5a71b479a6c5a7122bcd3aa`
Date: 2026-08-23
Kind: Targeted post-resolution audit, not SLP-2 Round 4
Result: Passed

## Author decisions under audit

- **E13:** PassedCase.function, FailedCase.function/path, and Event.value are public so an external
  Reporter provider can inspect the structured case event.
- **E14:** Standard filters use ASCII case-insensitive byte-substring matching: `A`–`Z` fold to
  lowercase for comparison and every other byte remains exact.

## Audit boundary

Check that the two changes close E13 and E14 without contradicting current Silk visibility,
ownership, raw HostInput behavior, filter examples, standard-runner policy, compiler privilege, or
the revision-33 architecture. Re-raising unrelated ledger items does not fail this targeted audit.

## Findings

| Id | Lens | Severity | Evidence | State |
| --- | --- | --- | --- | --- |
| AS1 | Scope/coherence | High | Public case fields make the external match legal while Outcome-to-Event still moves the owned StackPath exactly once. | PASS |
| AS2 | Scope/coherence | High | ASCII folding makes `fillBytes` match `FillBytes`; punctuation remains exact, bytes stay raw, and order/policy remain runner-owned. | PASS |
| AE1 | Examples/model | High | Current Silk supports field-level `pub`; CountingReporter can access every field it destructures. | PASS |
| AE2 | Examples/model | High | `random`, `fillBytes`, and `seededZero` now match their IDs, while `fill-bytes` remains an exact no-match boundary. | PASS |
| AE3 | Examples/model | Medium | Invalid UTF-8 needs no decoding: only ASCII uppercase bytes fold and all other bytes compare exactly. | PASS |
| AP1 | Compiler privilege | High | Public visibility changes access only; Event consumption and StackPath ownership remain ordinary source behavior. | PASS |
| AP2 | Compiler privilege | High | Slice iteration and byte comparison can implement ASCII folding in ordinary Silk; the compiler still only seeds raw host data. | PASS |

## Result rationale

Revision 34 closes E13 and E14 without a new blocker in the targeted boundary. All three lenses
confirmed current visibility syntax, ownership transfer, focused and no-match examples, raw invalid
UTF-8 behavior, and the absence of new compiler privilege. This audit does not reopen or resolve
unrelated nonblocking ledger items.
