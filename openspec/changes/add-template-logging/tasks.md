## 1. Public logging contract

- [x] 1.1 Change `Logger.log` and all `Effect` logging helpers to the generic static-template and
      borrowed-argument-pack signatures, then verify focused structured analysis accepts runtime
      `LogLevel`, literal/positional/named templates, borrowed temporaries, and reusable packs with only
      `LogError ? &mut Logger` channels.
- [x] 1.2 Add focused negative analysis cases for malformed templates, mismatched packs, missing
      fields, and missing Display evidence, and verify they fail during specialization through the same
      diagnostics as `Format.format`.

## 2. Provider implementation

- [x] 2.1 Implement transactional fixed-capacity formatting for `InMemoryLogger`, and verify success
      commits exactly one complete event while formatting, configured-attempt, and capacity failures
      increment attempts but commit no event metadata or message prefix.
- [x] 2.2 Implement direct `Format.format` rendering in `StdoutLogger` with WriterError-to-LogError
      translation, and verify native logging produces the expected bytes without exposing Writer or
      Allocator channels to callers.

## 3. Migration and generated surfaces

- [x] 3.1 Migrate every repository Silk call site and TypeScript source fixture to pass an argument
      pack, and verify no one-argument Effect logging call or message-only Logger implementation remains.
- [x] 3.2 Update public source documentation examples and regenerate `Stdlib.generated.ts` plus the
      standard-library reference pages, then verify generator check commands report exact output.

## 4. Runtime and repository verification

- [x] 4.1 Extend the existing shared native acceptance corpus with the minimum formatted-logging case
      that proves helper severity/order/message behavior and cross-engine agreement without a new test
      worker or redundant native compilation.
- [x] 4.2 Preserve static specialization identity through finite Effect joins and composite runner
      selection, and verify a stored choice between two static specializations realizes, verifies, and
      executes the selected specialization.
- [ ] 4.3 Run focused logging/formatting checks, OpenSpec strict validation, then the repository-required
      `pnpm typecheck`, `pnpm format:check`, `pnpm lint`, `pnpm test`, `pnpm check`, and
      `pnpm release:candidate` commands in order and record exact results.
