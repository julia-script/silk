## 1. Ordinary system-clock provider

- [x] 1.1 Declare the C-layout timespec record and libc clock functions in `OsSystemClock`, validate
      returned values in Silk, and verify Darwin/Linux analysis retains only the exact foreign imports.
- [x] 1.2 Delete the two system-clock intrinsic operations, generated runtime functions, symbol
      reservations, and inventories; verify focused intrinsic and runtime-source tests prove absence.

## 2. Non-native behavior and obsolete host machinery

- [x] 2.1 Define evaluator system-clock absence through `ForeignHostUnavailable` and direct-Wasm
      behavior through versioned foreign imports; verify both without ambient host reads.
- [x] 2.2 Delete the bespoke TypeScript system-clock provider, evaluation option, blocked reason, and
      inspector branches; keep monotonic host validation self-contained and its tests intact.

## 3. Acceptance, follow-ups, and documentation

- [x] 3.1 Reuse the serialized native clock acceptance to compile and run the extern-backed system
      provider with the intrinsic-backed monotonic provider on the host target.
- [x] 3.2 File focused Linear follow-ups for every remaining compiler-owned OS provider subsystem.
- [x] 3.3 Update reference docs and the four affected main specs, sync the delta specs, and archive
      the completed change.

## 4. Repository verification and handoff

- [x] 4.1 Run `pnpm typecheck`, `pnpm format:check`, `pnpm lint`, `pnpm test`, `pnpm check`, and
      `pnpm release:candidate` in repository order and record exact outcomes.
- [x] 4.2 Prepare the exact committed issue diff for independent correctness and mandatory
      test-economics approval, then create and read back the stacked draft PR and Linear handoff.
