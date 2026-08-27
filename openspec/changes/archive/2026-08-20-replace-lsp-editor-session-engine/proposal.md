## Why

The language server can permanently retain stale diagnostics and leave hover or quick-fix requests
pending after ordinary edit transitions because document versions, project revisions, query
waiters, diagnostics, and client generations are owned by separate modules with incompatible
liveness assumptions. The current push-diagnostic path also relies on version filtering that the
production VS Code language client explicitly does not support, so another scheduler guard cannot
make the end-to-end editor session coherent.

## What Changes

- **BREAKING** Replace `DocumentUpdates` and the current `ProjectSession` orchestration with one
  workspace revision engine that owns synchronized text, project identity, explicit revision
  states, bounded latest-state scheduling, query settlement, diagnostic freshness, and worker
  health.
- Accept editor changes into a monotonic per-document source ledger before project discovery and
  retain at most one active and one latest pending project revision rather than a FIFO of obsolete
  synchronization operations.
- Move project analysis and semantic editor queries into isolated project workers so compiler work
  cannot monopolize the protocol event loop; recreate an unresponsive worker from the latest
  source ledger through a bounded watchdog path.
- Give every query an exact revision and worker generation plus an explicit terminal outcome;
  propagate LSP cancellation and deadlines so no hover, completion, code action, inspection, or
  shutdown operation can wait forever.
- Commit immutable snapshots before and independently of diagnostics or inspector delivery, and
  replace push diagnostics with LSP pull diagnostics plus standard refresh notifications for
  dependency-only changes.
- Replace the extension's exposed replaceable-client reference with one stable editor-session
  owner that binds diagnostics, inspector subscriptions, restart, built-in document
  synchronization, an acceptance barrier, process termination, and health to an acknowledged
  client generation.
- Delete the superseded scheduling and lifecycle paths rather than retaining adapters or fallback
  modes.

## Capabilities

### New Capabilities

<!-- None. The new architecture fulfills and strengthens existing language-server and editor
     capabilities rather than creating a separate user-visible capability. -->

### Modified Capabilities

- `language-server-synchronization`: Make source acceptance coalescing, explicit terminal revision
  states, worker isolation, cancellable exact-revision queries, commit-before-publication, and pull
  diagnostics part of the synchronization contract.
- `cursor-extension`: Make one stable editor session own client generations, acknowledged process
  replacement, built-in open-document synchronization and acceptance acknowledgement,
  pull-diagnostic integration, and durable inspector subscriptions.

## Impact

- Replaces the orchestration centered in `packages/lsp/src/Server.ts`, `DocumentUpdates.ts`, and
  `ProjectSession.ts` with a deep workspace engine and project-worker adapter.
- Introduces a worker protocol carrying source revisions, query DTOs, diagnostics, editor edits,
  symbols, and health events while keeping compiler semantic graphs inside the worker.
- Changes `apps/vscode` activation, restart, diagnostics, and inspector subscription ownership.
- Reworks LSP integration, pressure, worker-failure, cancellation, restart, and Extension
  Development Host tests around the production adapter behavior.
- Preserves the compiler-owned immutable `ProjectAnalysis`/analysis-facade semantics initially;
  dependency-keyed compiler incrementality beyond the worker seam is a separate optimization unless
  measurements show it is required for acceptance.
