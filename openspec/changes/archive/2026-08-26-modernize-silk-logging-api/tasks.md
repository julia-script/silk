## 1. Enum-Based Logger Model

- [x] 1.1 Replace the `LogLevel` struct and numeric helpers in `logger.silk` with the five-member
  scalar enum, update Logger documentation and examples to use qualified members, and verify the
  shipped module analyzes through the compiler's standard-library check.
- [x] 1.2 Store `LogLevel` values directly in `InMemoryLogger`, return them directly from `levelAt`,
  preserve the unused-slot `Trace` behavior, and verify focused logging tests observe nominal enum
  equality without integer conversion.

## 2. Effect Logging Surface

- [x] 2.1 Keep `logAt` as the single Logger dispatch path, route `log` through the `Info` member, add
  `logTrace`, `logDebug`, `logInfo`, `logWarning`, and `logError`, and verify every function has the
  `() ! LogError ? &mut Logger` contract in realized analysis.
- [x] 2.2 Remove every repository use of the superseded Logger level constructors and `levelCode`,
  migrate source fixtures and examples to qualified enum members or Effect aliases, and verify a
  repository-wide literal search finds no stale API calls.

## 3. Tests, Tooling, and Documentation

- [x] 3.1 Update executable call discovery to traverse both operands of `EnumEquality`, retain the
  minimized imported-accessor regression that fails without this traversal, and verify the direct
  comparison reaches evaluator completion without a staging local.
- [x] 3.2 Extend the existing `Logging.test.ts` source program to exercise all level-specific helpers
  in order, retain dynamic `logAt` coverage, update the committed MIR golden, and verify the focused
  logging test passes without adding a redundant test file or native execution leg.
- [x] 3.3 Update the existing editor-intelligence logging story to complete and navigate the
  `LogLevel` enum members and new Effect helpers, and verify its focused test passes.
- [x] 3.4 Update standard-library README prose and all logger/effect doc examples for the enum and
  helper family, regenerate any derived standard-library artifacts, and verify the compiler's
  documentation policy, generated-document, and documentation-example checks pass.

## 4. Repository Verification

- [x] 4.1 Run `pnpm typecheck`, then `pnpm exec biome check .`, then `pnpm test`, correcting any
  failures caused by this change and recording any confirmed pre-existing failure.
- [x] 4.2 Run `pnpm check` and `pnpm release:candidate`, record any confirmed pre-existing gate
  failure, and inspect the final diff to confirm the old logging path and compatibility aliases are
  absent.
