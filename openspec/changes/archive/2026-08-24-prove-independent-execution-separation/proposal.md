## Why

Backend parity does not by itself prove SLP-0001's pay-for-use and minimal-privilege thesis. The
handoff needs connected ordinary-Silk owners plus artifact evidence that direct and nested programs
stay cheap and that another owner can reuse the same substrate without compiler policy changes.

Source: [SLP-0001, revision 31](../../../proposals/0001-independently-resumable-effect-executions/proposal.md),
SHA-256 `963a7420f16bce3bd0ec50acd906b4a4ee43319d132c5452d3686cde643c5635`,
realization slice 5 of 5. Depends on `add-independent-execution-engine-parity` and the SLP-0002
changes culminating in `prove-local-shared-slp1-sufficiency`.

## What Changes

- Add connected ordinary-Silk pressure programs for deferred first activation, source-owned result
  waiting, same-thread timer wake, cancellation, and a future Coroutine-shaped alternate owner.
- Prove construction is all-or-nothing before publication, readiness publication is task-specific,
  no callback runs under source Shared access, and no wake-time allocation is hidden.
- Inspect ordinary direct, ordinary nested-only, explicit direct, explicit nested-only, and
  park-capable artifacts to prove the selected static pay-for-use matrix and explicit delimiter.
- Audit compiler phases and artifacts to prove no Scheduler, Fiber, Deferred, Timer, Coroutine,
  Allocator, or safe Execution wrapper gains privilege by spelling.
- Record the accepted smaller eager-start/owner-sweep/fatal-package alternative and why its loss of
  first-activation control, push readiness, and recoverable admission is intentional rather than an
  implementation fallback.

## Capabilities

### New Capabilities

- `bootstrap-independent-execution-pressure`: define the connected source sufficiency,
  alternate-owner reuse, publication, allocation, and privilege evidence gate.

### Modified Capabilities

- `bootstrap-language-pressure-programs`: add explicit Scheduler-shaped, timer-shaped, and
  Coroutine-shaped ordinary-source owners with cross-engine observable evidence.
- `bootstrap-backend`: require static absence/presence evidence for direct, nested-only,
  non-parking explicit, and external-park explicit execution tiers.

## Impact

This affects pressure-program sources, compiler-artifact inventories, differential corpus entries,
findings documentation, pay-for-use assertions, and package verification. It does not select
canonical concurrency or Coroutine APIs, implicit root ownership, parallel transfer, or a new
compiler-recognized library actor.
