## Context

This change follows static text and uses the existing explicit service requirement/failure-row model. The output boundary must work in evaluation, native executables, and hosted Wasm without equating process streams with structured logging.

## Goals / Non-Goals

**Goals:** explicit stdout/stderr byte writes, typed failures, replaceable tests, and host parity.

**Non-Goals:** `Effect.log`, Logger policy, OpenTelemetry, buffering/formatting, Stream/Sink, stdin, or default providers.

## Decisions

### StandardStreams is an explicit service

`writeAll(destination, bytes)` remains in the requirement row until provided. Tests can provide memory; native entry provides process streams; Wasm hosts provide the versioned import.

_Alternative considered:_ ambient stdout. Rejected because it breaks capability transparency and provider replacement.

### MIR carries target-neutral writes

The operation contains destination, immutable bytes, order, provenance, and typed outcome—not file descriptors, console calls, or log fields.

### Logger and defaults remain general future features

A future Logger may route to many sinks and initially remains explicit. Later default-overridable provisioning applies to every service at the outer boundary, never Logger alone.

## Risks / Trade-offs

- [Host partial writes] → provider implements all-or-failure looping and exposes one typed outcome.
- [Wasm ABI ossifies] → keep the import private and versioned; source semantics do not name it.
- [Logging pressure leaks downward] → accept only bytes/destination and prohibit log metadata.

## Migration Plan

Add service contracts and requirement propagation, MIR/evaluator memory provider, native adapter, Wasm host import, then differential output/failure tests.

## Open Questions

None.
