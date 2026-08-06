## Purpose

Converts WebContainer callback subscriptions into typed Effect streams with deterministic subscription cleanup and independent consumers.

## ADDED Requirements

### Requirement: Typed runtime event streams
The runtime service SHALL expose typed streams for port changes, server readiness, internal runtime errors, and preview messages, preserving all event fields supplied by WebContainer.

#### Scenario: Port opens or closes
- **WHEN** WebContainer reports a port event
- **THEN** the port stream emits the port number, open-or-close state, and URL

#### Scenario: Server becomes ready
- **WHEN** WebContainer reports a ready server
- **THEN** the server-ready stream emits the port number and URL

#### Scenario: Runtime reports an internal error
- **WHEN** WebContainer emits an internal error event
- **THEN** the internal-error stream emits a typed event value containing its message

#### Scenario: Preview reports a message
- **WHEN** WebContainer emits a forwarded preview exception, rejection, or console error
- **THEN** the preview-message stream emits a typed value preserving its preview location and variant-specific details

### Requirement: Scoped event subscriptions
Each event-stream subscription SHALL register its WebContainer listener when consumed and unregister that listener exactly once when consumption completes, fails, or is interrupted.

#### Scenario: Stream consumer is interrupted
- **WHEN** a fiber consuming an event stream is interrupted
- **THEN** its registered WebContainer listener is removed exactly once

#### Scenario: Runtime scope closes
- **WHEN** the runtime scope closes while event streams are subscribed
- **THEN** all subscriptions stop and their listeners are removed before the runtime becomes unusable

### Requirement: Independent subscribers
Multiple consumers of the same event kind SHALL have independent subscriptions so that stopping one consumer does not stop or drain another consumer.

#### Scenario: One of two consumers stops
- **WHEN** two consumers subscribe to port events and one subscription ends
- **THEN** the remaining consumer continues receiving subsequent port events

### Requirement: Preserve event ordering
Each event stream SHALL emit events in the order its corresponding WebContainer listener observes them and SHALL not merge unrelated event kinds into an ambiguous union stream by default.

#### Scenario: Consecutive events arrive
- **WHEN** WebContainer invokes a listener with multiple events in sequence
- **THEN** the corresponding stream emits those events in the same sequence
