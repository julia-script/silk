## 1. Decision and regression gate

- [ ] 1.1 Record explicit approval of the finite-intersection environment contract in this change before implementing it; verify the approval covers the syntax, anonymous elaboration, and bracket contract described in `design.md`.
- [ ] 1.2 Convert the captured reproduction from current-signature rejection to intended intersection-signature acceptance and retain the direct-forwarding and escaped-resource controls; verify the new acceptance assertion fails at `SEM0089` before the implementation.

## 2. Finite environment semantics

- [ ] 2.1 Add canonical finite intersections to lifetime representation, substitution, free-variable traversal, outlives proofs, and runtime erasure; verify canonical alpha/order identity, shortening, non-promotion, invariant payloads, and placeholder rejection in the existing lifetime/type tests.
- [ ] 2.2 Add intersection parsing, elaboration, display, and formatting in Effect environment positions, including explicit effect-function environments; verify supported syntax round-trips and unsupported quantifier structures remain rejected in existing parser/declaration tests.
- [ ] 2.3 Derive anonymous returned Effect environments from capture validity and newly retained inputs while preserving generic content obligations; verify captured config/once-callback cases, capture-free reduction, consume-once behavior, and short-capture escape rejection in existing anonymous-callable tests.
- [ ] 2.4 Carry intersections through quantified compatibility/inference and semantic surface encoding, keeping exact row and representation identities; verify generic symbolic-row forwarding, module-surface round trips, scoped placeholders inside intersections, and no lifetime-dependent runtime instance identity.
- [ ] 2.5 Attribute a failed environment obligation before fallback row diagnostics; verify the lifetime mismatch is reported precisely while genuinely ambiguous/non-finite rows still receive their existing diagnostic families.

## 3. Bracket integration

- [ ] 3.1 Replace both callback result environments in the source helper and sealed intrinsic with the intersection contract, preserving release representation and NonParking constraints; regenerate embedding and docs and verify their check commands plus the tiny reproduction's analysis and MIR verification.
- [ ] 3.2 Verify that the bracket retains both callbacks/resource, ends the use Effect and loan before fresh release access, and rejects escaping outcomes and parking release; extend the existing Suspendability assertions only for distinguishing obligations.
- [ ] 3.3 Add a representative captured-use cancellation boundary to the shared acceptance corpus only if existing cancellation fixtures do not prove it; verify success, typed failure, cancellation, nested release order, and original-outcome preservation using the designated existing runtime acceptance paths in CI.

## 4. Preserved caller and delivery

- [ ] 4.1 After the tiny program both analyzes and lowers, copy the exact five-file JUL-188 correction set into an isolated integration checkout and apply the focused change; verify the actual preserved caller's analysis and lowering once, without changing the original worktree/stashes or rebuilding the broad TLS implementation.
- [ ] 4.2 If a distinct provider/generic ownership obligation remains, record its minimal source and exact failing proof and stop for scope/design review; verify no TLS-specific rule, ambient service fallback, signature weakening, or manual cleanup was introduced.
- [ ] 4.3 Publish the coherent focused commit range only with publication authorization and run broad delivery checks in CI, including `pnpm check` and package-content validation when applicable; record exact revisions and outcomes and hand integration instructions to the paused implementer without merging or completing JUL-188.
