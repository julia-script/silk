## ADDED Requirements

### Requirement: Failed child startup releases only acquired resources

Failure before child creation SHALL attempt release of every successfully acquired pipe endpoint
exactly once, SHALL release no unacquired endpoint, and SHALL start no child. The failure SHALL
preserve the original operation's native error even if cleanup changes native error state.

#### Scenario: Startup channel configuration fails

- **WHEN** the output, error, and startup-reporting pipes are acquired but close-on-exec configuration fails
- **THEN** all six acquired endpoints receive exactly one close attempt, no child is created, and the original configuration error is reported

#### Scenario: Startup channel acquisition fails

- **WHEN** the output and error pipes are acquired but startup-reporting pipe acquisition fails
- **THEN** only the four acquired endpoints receive a close attempt, no child is created, and the original acquisition error is reported
