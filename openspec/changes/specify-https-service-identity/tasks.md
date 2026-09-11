This checklist tracks the **design-only JUL-169 deliverable**. Runtime implementation is separately
estimated in [implementation.md](implementation.md); none of these tasks ships a matcher.

## 1. Specify the identity contract

- [x] 1.1 Revalidate repository and related ownership against the triage baseline; record the inspected commit and changed input APIs in design.md.
- [x] 1.2 Specify exact Silk data/signatures, ownership and origin provenance; verify the design and delta cover every input boundary in JUL-169.
- [x] 1.3 Specify DNS/IP matching, malformed SAN precedence and budgets; cross-check against the pinned RFCs and webpki source and retain differences in design.md.
- [x] 1.4 Publish the versioned fixture matrix and separately scoped five-point implementation decomposition; verify every acceptance category maps to fixture rows or an explicit integration obligation.

## 2. Validate and hand off the design

- [x] 2.1 Create and link the estimated implementation follow-up in the canonical Linear project; read it back and record its URL in implementation.md.
- [x] 2.2 Strictly validate this OpenSpec change and obtain independent design review; resolve concrete findings and record commands/verdicts in validation.md.
- [x] 2.3 Commit and push the scoped change to a draft PR before final verification; run pnpm typecheck, pnpm format:check, pnpm lint, pnpm test and pnpm check in order and record exact outcomes.
- [x] 2.4 Obtain dedicated test-economics approval of the committed diff, confirm draft PR head/state and update JUL-169 with the same verification evidence; report no runtime support.
