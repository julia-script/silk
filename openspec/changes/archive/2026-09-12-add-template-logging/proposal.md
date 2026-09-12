## Why

Silk's logging helpers accept only pre-rendered strings even though the standard library already
supports statically validated, allocation-free template formatting over borrowed argument packs.
Callers need one API that combines those facilities without fragmenting a semantic event or
acquiring Writer and allocation capabilities at the call site.

## What Changes

- **BREAKING** Replace the message-only `Logger.log` operation and all `Effect` logging helpers with
  generic operations that accept a static template and one borrowed tuple or record argument pack.
- Keep `Effect.logAt` severity runtime-selectable while the named helpers continue to select their
  fixed levels.
- Reuse `Format.format` and `Display` semantics inside Logger providers, preserving one semantic
  event and the public `LogError`/`&mut Logger` channels.
- Make retained-provider failure atomic while permitting stream providers to have physically
  written a prefix before a failure.
- Migrate all callers, tests, generated standard-library artifacts, and documentation; remove the
  superseded message-only path.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-logging`: Change Logger and Effect logging invocations from complete-message inputs to
  statically validated template-and-argument-pack inputs while preserving event, provider, order,
  and failure semantics.
- `template-formatting`: Extend the existing template formatting contract to Logger-backed
  formatting with the same grammar, validation, Display selection, and residual behavior.

## Impact

- Public Silk standard-library API: `Logger.log`, `Effect.log`, `Effect.logAt`, and all named
  severity helpers.
- Portable `InMemoryLogger` and native `StdoutLogger` implementations.
- Every repository logging call site, logging analysis tests, shared execution corpus, generated
  stdlib embedding, generated standard-library documentation, and project initializer snapshots.
- No new dependency, grammar, intrinsic, compiler-known standard-library actor, or compatibility
  overload.
