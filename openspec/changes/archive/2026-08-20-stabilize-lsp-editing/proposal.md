## Why

Ordinary incomplete text can make compiler analysis defect, after which the language-server project
scheduler remains permanently marked active: later edits are never analyzed, semantic requests wait
forever, old diagnostics remain visible on correct code, and even `Silk: Restart Language Server`
times out. Independent of that liveness failure, sustained typing also accumulates uncancelled
frontend work on the protocol thread, so latency rises quickly with realistic source files.

## What Changes

- Require frontend snapshot construction to remain available for every synchronized source byte
  sequence, including parser-recovered match patterns and other incomplete constructs. Damaged facts
  remain explicitly unavailable instead of escaping as internal exceptions.
- Make each project scheduler recover after successful, failed, defective, or interrupted analysis.
  Every accepted edit either reaches an exact-version commit, is superseded, or settles as
  unavailable; it cannot leave requests, diagnostics, or shutdown waiting indefinitely.
- Replace the fixed sleep-and-drain loop with project-scoped trailing-edge coalescing and
  latest-revision preemption so stale work does not consume the full latency budget before the newest
  edit can begin.
- Keep protocol handling responsive while frontend work runs, and synchronize semantic requests
  only with the requested project and document revision rather than unrelated document updates.
- Define failure-time diagnostic behavior: a failed current revision cannot leave an older
  version's diagnostics presented as current, and a later valid revision republishes normally
  without requiring a server restart.
- Make language-server shutdown and the extension restart command bounded and self-healing even if
  analysis is defective or unresponsive.
- Add deterministic defect-recovery, rapid-edit, cross-project isolation, shutdown, and recovered
  syntax regression coverage, with performance observations kept in opt-in benchmarks rather than
  timing assertions in the correctness suite.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-analysis-facade`: require frontend snapshot construction and editor-facing queries to
  represent arbitrary recovered source as analysis data without leaking an internal defect.
- `language-server-synchronization`: strengthen latest-wins scheduling with failure recovery,
  bounded request settlement, project isolation, responsive preemption, diagnostic freshness, and
  bounded shutdown.
- `cursor-extension`: require `Silk: Restart Language Server` to recover from a wedged server instead
  of surfacing a terminal stop-timeout failure.

## Impact

- Compiler recovery and elaboration paths in `packages/compiler`, especially recovered pattern and
  expression facts exposed through `Analysis` and `ProjectAnalysis`.
- Project scheduling, request acquisition, diagnostic publication, lifecycle management, and test
  infrastructure in `packages/lsp`.
- Language-client lifecycle handling and restart tests in `packages/vscode`.
- No Silk language syntax, public standard-library API, diagnostic code catalog, or backend contract
  changes are intended.
