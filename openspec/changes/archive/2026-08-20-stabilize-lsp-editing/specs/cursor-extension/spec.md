## ADDED Requirements

### Requirement: Language-server restart is bounded and self-healing

`Silk: Restart Language Server` SHALL replace the current server with a newly started server even
when the current server does not complete graceful shutdown. The command SHALL retire the old
server within a bounded lifecycle, SHALL avoid leaving the language client permanently stopped, and
SHALL report failure only when a replacement server cannot be started or initialized.

#### Scenario: Restart a healthy server after rebuild

- **WHEN** a contributor rebuilds the language-server binary and runs the restart command
- **THEN** the old server shuts down and a new server starts from the rebuilt binary

#### Scenario: Restart a server with a wedged project

- **WHEN** the current server cannot complete graceful shutdown because project analysis failed or stopped making progress
- **THEN** the extension retires that server and starts a replacement without requiring a window reload or a second restart command

#### Scenario: Replacement startup fails

- **WHEN** the old server has been retired but the replacement process cannot start or initialize
- **THEN** the command reports the replacement failure and leaves no stale client state claiming that language features are running
