## Context

See `proposal.md` for motivation and the two delta specs for the required public behavior.
`silk/logger.silk` currently encodes `LogLevel` as a private-field `i32` struct, exposes five
constructor functions plus `levelCode`, and stores integer codes inside `InMemoryLogger`.
`silk/effect.silk` exposes only the info shorthand `log` and the general `logAt`. Scalar enums are
now ordinary source-defined, `Copy`, layout-bearing values with nominal equality and qualified
members, so neither the Logger service nor its providers need compiler recognition to accept one.

Applying the enum API exposed one general executable-discovery defect. A direct expression such as
`Logger.levelAt(&logger, 0) == LogLevel.Trace` analyzes without diagnostics but reaches MIR with the
`levelAt` callee absent. Binding the same call to a local before equality succeeds. The cause is that
call discovery recursively visits `StringEquality` and `ShortCircuit` operands but omits the
structurally identical `EnumEquality` node.

This repository is green-field: the old constructors and numeric observation function are not a
compatibility contract. The change therefore migrates every caller and removes the superseded
surface in one atomic update.

## Goals / Non-Goals

**Goals:**

- Make invalid logging severities unrepresentable in safe source.
- Keep Logger provider dispatch, requirements, typed failures, message ownership, and execution
  order unchanged.
- Give each existing severity an obvious Effect operation without duplicating provider logic.
- Store and inspect in-memory levels as nominal enum values end to end.
- Preserve reachable ordinary calls when they appear directly inside enum-equality operands.

**Non-Goals:**

- Adding `Fatal`, changing the five-level ordering, or standardizing provider rendering.
- Adding filtering, annotations, timestamps, spans, default providers, or telemetry transport.
- Adding compiler recognition for Logger, LogLevel, or any Effect logging helper.
- Expanding the compiler correction beyond the missing `EnumEquality` operand traversal.
- Preserving the struct constructors or `levelCode` as compatibility aliases.

## Decisions

### `LogLevel` is a default-representation scalar enum

Declare `pub enum LogLevel { Trace, Debug, Info, Warning, Error }` and use qualified members at
call sites. The enum's implicit source order retains the existing conceptual ordering while the
default fixed-width representation is sufficient for five members. No public contract needs an
`i32` representation: integer codes were an artifact of the former struct implementation, not a
logging behavior.

An explicitly represented `enum(i32)` would reduce changes to existing storage and goldens, but it
would preserve representation policy solely because the removed struct happened to contain an
`i32`. The default representation is the clean declaration of this domain and allows the compiler's
ordinary enum rules to choose its physical shape.

The old `trace`, `debug`, `info`, `warning`, `error`, and `levelCode` functions are deleted. Keeping
them would create two public construction/observation styles and contradict the green-field rule.
Code that needs to branch on a level uses enum equality or an exhaustive match; implementation code
that genuinely needs the discriminant can use the declaration-generated `LogLevel.value` wrapper.

### In-memory storage retains `LogLevel`, not its representation

Change `InMemoryLogger.levels` from `[i32; 8]` to `[LogLevel; 8]`, initialize unused slots with
`LogLevel.Trace`, store the incoming enum directly, and return the stored enum directly from
`levelAt`. This maintains the nominal type through the whole provider and removes both directions
of manual integer conversion.

The bounded provider's documented unused-slot behavior remains Trace. Its capacity, message
storage, attempt accounting, and failure behavior do not change. Keeping an integer array and
reconstructing an enum would require either an unsafe conversion or an exhaustive integer decoder;
both manufacture a representation boundary that the provider does not need.

### Level-specific Effect operations delegate to `logAt`

Implement `logTrace`, `logDebug`, `logInfo`, `logWarning`, and `logError` as small public effect
functions that call `logAt` with the corresponding enum member. Keep `log(message)` as the
established info spelling and have it share the same path. This leaves exactly one operation that
calls `Logger.log`, so every helper necessarily preserves `() ! LogError ? &mut Logger` and message
forwarding.

Duplicating direct `Logger.log` calls in every helper would behave correctly but create six places
whose channels and dispatch could drift. Removing `logAt` would make dynamic level selection
awkward and would narrow existing useful behavior, so it remains the primitive public helper.

### Executable discovery traverses enum-equality operands

Add `EnumEquality` to the same recursive operand branch as `StringEquality` and `ShortCircuit` in
`ExecutableOrigin.callTargets`. This is the smallest correction: both equality operands are already
ordinary HIR expressions, and any calls nested within them must contribute their targets to the
executable instance closure before MIR calling shapes are planned.

The minimized regression imports `Logger.levelAt`, calls it directly as one equality operand, and
requires evaluator completion. Before the correction it deterministically produces
`InvalidCallShape` because no `levelAt` instance is discovered; binding the result to a local moves
the call outside `EnumEquality` and proves that the Logger declaration and enum layout themselves
are valid.

Requiring callers to introduce locals was rejected because it would leave a well-typed expression
shape unable to lower. Special-casing Logger or `levelAt` was also rejected: logging declarations
remain ordinary source, and the missing traversal applies to every scalar enum.

### Verification extends the existing logging stories

Update `Logging.test.ts` rather than adding a new worker file. One shared in-memory source program
will invoke all aliases, assert nominal enum levels and order with `Analysis.evaluate`, and continue
to pin the relevant MIR golden. Existing provider-failure, missing-provider, user-provider, stdout,
and backend coverage remains responsible for behavior that the aliases merely delegate to.

Keep the minimized direct-operand program in the same file as a regression for the executable
discovery correction. It proves a failure mode distinct from alias dispatch: the direct enum
comparison must discover and lower its imported accessor without a staging local.

Update the existing editor-intelligence logging story to navigate an enum member and the added
Effect operations. Documentation examples and stdlib prose use qualified enum members and describe
the full helper family. This proves the public source surface without adding redundant native runs.

## Risks / Trade-offs

- **[Risk] The enum's default representation changes the Logger parameter and in-memory array
  layout from `i32` to the compiler-selected enum representation.** → Regenerate the committed MIR
  golden and rely on the existing evaluator/backend agreement coverage plus the full repository
  checks; no external ABI is stable in this green-field repository.
- **[Risk] A stale constructor or `levelCode` reference survives in a fixture, documentation block,
  or generated expectation.** → Search the complete repository for each removed spelling and update
  every caller in the same change, then run documentation and release-candidate validation.
- **[Risk] Several thin aliases add surface area.** → Keep `logAt` as the sole dispatching helper and
  limit aliases to the existing five severities; no parallel implementation or new severity is
  introduced.
- **[Risk] A narrowly observed discovery omission could invite a broad traversal refactor.** → Change
  only the existing equality operand branch, retain a minimized red-before/green-after regression,
  and rely on the full instance, MIR, evaluator, and backend suites for traversal consistency.

## Migration Plan

1. Replace `LogLevel` and migrate the in-memory provider in `logger.silk`.
2. Correct executable discovery for direct calls nested in `EnumEquality` operands.
3. Add the Effect aliases and route all shorthand operations through `logAt`.
4. Update every repository caller, test, golden, example, and editor assertion atomically; leave no
   compatibility declarations behind.
5. Run the focused logging and documentation tests, then the required typecheck, Biome, test,
   `pnpm check`, and release-candidate gates.

Rollback is a source revert of the complete change. There is no staged dual API or data migration.
