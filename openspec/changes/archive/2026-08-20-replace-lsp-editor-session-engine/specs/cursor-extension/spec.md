## ADDED Requirements

### Requirement: One stable editor session owns replaceable client generations

The extension SHALL expose one stable editor-session lifetime to its commands and feature
consumers while keeping replaceable language-client and server-process generations private. The
session SHALL bind built-in open-document synchronization and its server-authoritative acceptance
barrier, diagnostic
pulls, inspector invalidation subscriptions, request cancellation, generation health, and disposal
to the authoritative generation. Consumers MUST NOT retain a concrete replaceable client reference
or register a subscription only against the generation present during activation. The language
client SHALL be the sole sender of document synchronization notifications. The session SHALL own
the sole visible diagnostic collection and issue standard pull-diagnostic requests through the
language client; a library-owned competing collection MUST NOT remain registered. On an accepted
Silk document change, the session SHALL immediately retire diagnostics already displayed for that
URI and SHALL apply a later diagnostic pull result only if its captured document version and client
generation are still current.

#### Scenario: Initial activation subscribes the inspector

- **WHEN** extension activation completes and the first language-client generation becomes ready
- **THEN** the inspector receives invalidation notifications without requiring the panel to be reopened or the server to be restarted

#### Scenario: Restart preserves stable consumers

- **WHEN** the language-client generation is replaced while the inspector is open
- **THEN** the stable session rebinds protocol subscriptions and subsequent commits refresh the inspector

#### Scenario: Restart synchronizes open documents once

- **WHEN** a replacement server generation becomes ready while Silk documents are open
- **THEN** the language client's built-in synchronization sends each open document once and the session acknowledges their latest accepted versions before declaring the replacement healthy for semantic requests

#### Scenario: Local synchronization observation misses an already-open document

- **WHEN** the language client delivered an already-open document and the replacement server acknowledges its current version but the session's notification-sent observer did not see that delivery
- **THEN** the server acknowledgement remains authoritative and the session becomes ready rather than timing out on the missing local observation

#### Scenario: Diagnostic result belongs to a retired generation

- **WHEN** a diagnostic response from a retired generation arrives after replacement
- **THEN** the session discards it without changing the editor's current diagnostic collection

#### Scenario: Existing diagnostics are retired on edit

- **WHEN** the editor displays diagnostics for `effec` and the document changes to `Effect`
- **THEN** the session removes the `effec` findings immediately and only a pull captured for the current `Effect` version may populate the collection again

#### Scenario: Document closes during a diagnostic pull

- **WHEN** a Silk document closes while its current or older diagnostic pull is running
- **THEN** the session removes its diagnostic collection and no late response repopulates the closed URI

## MODIFIED Requirements

### Requirement: Language-server restart is bounded and self-healing

`Silk: Restart Language Server` SHALL serialize replacement through the stable editor session,
attempt graceful shutdown within a bounded interval, forcibly retire a server that does not exit,
and acknowledge the previous process and protocol generation as unable to publish before making a
replacement authoritative. The replacement SHALL initialize, bind stable consumers, synchronize
the latest open Silk documents through the language client's built-in mechanism, and acknowledge
their acceptance before the command succeeds. Failure SHALL be reported only when the
previous generation cannot be safely retired or the replacement cannot become ready; no hidden
old/new overlap or stale running state is permitted.

#### Scenario: Restart a healthy server after rebuild

- **WHEN** a contributor rebuilds the language-server binary and runs the restart command
- **THEN** the old server exits, the replacement starts from the rebuilt binary, open documents are synchronized once and acknowledged, and the command completes after readiness

#### Scenario: Restart a server with a wedged project

- **WHEN** the current server cannot complete graceful shutdown because project analysis failed or stopped making progress
- **THEN** the extension forcibly retires and acknowledges that generation, starts and hydrates a replacement, and succeeds without requiring a window reload

#### Scenario: Replacement startup fails

- **WHEN** the old server has been retired but the replacement process cannot start, initialize, bind consumers, synchronize documents, or acknowledge their acceptance
- **THEN** the command reports the replacement failure and leaves the stable session explicitly unavailable rather than claiming language features are running

#### Scenario: Previous process cannot be retired

- **WHEN** the extension cannot prove that a timed-out previous generation can no longer publish or own editor resources
- **THEN** restart fails explicitly and does not start an overlapping authoritative generation
