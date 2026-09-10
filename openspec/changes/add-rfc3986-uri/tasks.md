## 1. Borrowed parsing and storage

- [x] 1.1 Add the minimal checked shared-slice view and exact reservation/ownership-transfer operations; verify bounds, lifetimes, and allocation behavior.
- [x] 1.2 Replace owned-only parsing with borrowed Uri/UriReference and explicit owned variants; verify unchanged grammar, input lifetimes, and copy/adoption behavior.

## 2. Reusable operations and components

- [x] 2.1 Implement reusable-buffer/writer percent coding, strict in-place decoding, unchanged-input borrowing, and whole-path/unreserved contexts; verify malformed-input atomicity and allocation contracts.
- [x] 2.2 Resolve directly in reusable output storage and adopt owned results; verify all RFC vectors, recomposition boundaries, and no intermediate backing allocations.
- [x] 2.3 Add raw/encoded components, construction/modification, and selective serialization; verify delimiters, empty components, and authentication omission.

## 3. Integration and handoff

- [ ] 3.1 Update callers, the consolidated native corpus, lifetime assertions, generated surfaces, docs, and PR usage examples; verify focused checks and public documentation.
- [ ] 3.2 Run typecheck, format:check, lint, test, check, and release:candidate in order; record current results.
- [ ] 3.3 Obtain independent code review and dedicated test-economics approval for the final committed diff; address the three review threads and confirm the updated PR and Linear baseline.

The prior owned-only implementation passed its checks at 768baa6c. Those results do not certify
this allocation/API revision. PR: https://github.com/julia-script/silk/pull/401.

Julia requested publishing the revision before further local tests. Focused URI/slice and
ownership/MIR checks, typecheck, formatting, lint, documentation policy, and all 57 doctests passed.
The full workspace run remains incomplete; check and release:candidate are pending.
