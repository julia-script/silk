## Why

Silk logging still models a closed five-value severity set as an open struct with constructor and
numeric accessor functions, even though scalar enums can now express that domain directly. The
Effect logging surface also makes callers select common severities through `logAt`, rather than
offering the familiar level-specific operations used for routine logging.

## What Changes

- **BREAKING** Replace the `LogLevel` struct and its `trace`, `debug`, `info`, `warning`, `error`,
  and `levelCode` helpers with a closed scalar enum whose members are `Trace`, `Debug`, `Info`,
  `Warning`, and `Error`.
- Keep `Effect.log(message)` as the info-level shorthand and `Effect.logAt(level, message)` as the
  general level-selecting operation.
- Add `Effect.logTrace`, `Effect.logDebug`, `Effect.logInfo`, `Effect.logWarning`, and
  `Effect.logError` aliases, each preserving the Logger requirement and typed `LogError` failure.
- Update the in-memory Logger, examples, documentation, fixtures, and tests to use enum members and
  compare levels nominally rather than through integer codes.
- Correct executable call discovery so ordinary calls nested directly in scalar-enum equality
  operands remain reachable and lowerable.
- Do not add a new severity such as `Fatal`; the aliases cover the existing five-level contract.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-logging`: Define logging severity as a closed scalar enum and expose one Effect helper
  for every existing severity alongside `log` and `logAt`.
- `bootstrap-silk-stdlib`: Ship and surface the enum-based Logger API and all level-specific Effect
  logging operations as canonical ordinary Silk source.

## Impact

The public `silk.logger` and `silk.effect` APIs change, so all repository call sites using
`Logger.trace()`, `Logger.debug()`, `Logger.info()`, `Logger.warning()`, `Logger.error()`, or
`Logger.levelCode(...)` must migrate in the same change. The work affects the shipped standard
library source and manifest-derived editor surface, compiler logging tests, standard-library
documentation and doctests, and executable call discovery for enum-equality operands. It introduces
no Logger-specific compiler privilege, backend feature, host dependency, or data migration.
