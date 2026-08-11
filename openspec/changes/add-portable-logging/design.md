## Context

See `proposal.md` for motivation. Silk now has source-defined Effect combinators, arbitrary `service`
contracts, static `interface` conformances, general lexical provision, source-defined
`StandardStreams`, static UTF-8 views, and evaluator/native/direct-Wasm provider plumbing. All
compiler-callable operations live under the sealed `Intrinsic` namespace. This design has been
reconciled with those implemented contracts; implementation remains gated only on archival of
`establish-minimal-intrinsic-boundary`.

Logging still needs a useful complete-event boundary without introducing a String decision or a
stream-shaped ABI.

## Goals / Non-Goals

**Goals:**

- Establish Logger as the first portable application-level service.
- Keep the log call, event, service, and initial providers visible as ordinary Silk source.
- Preserve complete-message semantics under stdout, memory, native, and hosted-Wasm execution.
- Make provider failure and requirement propagation honest in the existing Effect channels.

**Non-Goals:**

- Tracing spans, metrics, filters, asynchronous exporters, batching, sampling, or OpenTelemetry wire
  formats.
- An ambient/default Logger, compiler debugging intrinsic, or automatic logging of Effects.
- An owning String, general formatting framework, interpolation, or arbitrary structured values.
- Browser console and OpenTelemetry implementations in the first change.

## Decisions

### 1. The first LogEvent is severity plus a borrowed immutable UTF-8 view

`LogEvent` carries a closed `LogLevel` and one complete immutable UTF-8 message view. The Logger
operation consumes the view only for the dynamic extent of the call; implementations that retain
events copy into provider-owned storage. `Effect.log(message)` constructs Info, while a sibling
accepts an explicit level.

This admits static literals immediately, preserves multiline messages as one event, avoids choosing
an owning String, and allows stdout, browser, telemetry, and test providers to share one contract.
The event actor can later gain annotations or span context through a deliberate breaking alpha
change or a new richer operation.

Alternatives considered:

- **Accept raw bytes.** Portable, but makes invalid UTF-8 and rendered bytes the semantic logging
  model.
- **Own a Vector of bytes.** Lets providers retain cheaply but forces allocation and ownership
  transfer for every static log call.
- **Wait for String.** Couples basic observability to a separate unresolved storage decision.

### 2. Logger is an ordinary nominal capability and Effect.log is library code

The canonical logging module defines LogLevel, LogEvent, LogError, Logger, and provider types.
`silk.effects` imports the contract and defines `Effect.log` using the same requirement-binding and
Effect construction mechanisms as other source-defined combinators. No HIR, MIR, evaluator, or
backend branch matches the names `Logger` or `Effect.log`.

This keeps dependency replacement, failure rows, callable composition, and tooling navigation
uniform. It also makes a user-authored browser or telemetry Logger semantically ordinary.

### 3. StdoutLogger renders once and writes once

The stdout provider renders one deterministic UTF-8 record with a canonical lowercase severity
prefix, one separating space, the exact message, and one final LF. It invokes
`StandardStreams.writeAll(stdout, rendered)` once. Embedded newlines remain inside the event and do
not split provider calls. Stream failure is wrapped as LogError with the original provider detail.

Rendering lives in the provider, not LogEvent or Effect.log. That preserves the possibility of a
browser console provider using structured arguments and an OpenTelemetry provider mapping severity
without parsing a pre-rendered line.

### 4. InMemoryLogger owns observations

The test provider copies severity and message bytes into its own ordered storage before returning.
Construction receives any allocator it needs; the Logger operation does not invent an ambient
allocator requirement. Inspection borrows the recorded event sequence. A configurable failure
ordinal exercises typed failure and ordering without host access.

This provider supplies semantic acceptance. StandardStreams' existing memory provider remains a
raw byte-write test double and is not reused as the Logger oracle.

### 5. Backends use existing service and stream seams

The evaluator dispatches through the provided Logger witness. Native and direct-Wasm programs
compile the same Logger implementation as ordinary Silk. StdoutLogger eventually reaches the
existing StandardStreams native/private-Wasm boundary, so logging adds no Logger-specific host
import. A browser can instead provide another Logger or a StandardStreams implementation without a
Unix file descriptor model.

### 6. Tooling treats logging declarations as standard-library source

The canonical files enter the standard-library manifest and embedded source table. Existing module
closure, occurrence, presentation, completion, hover, and definition machinery must expose them.
Labs add one preset showing the Logger requirement, provider implementation, HIR/MIR composition,
events, and backend artifacts through the facade.

## Risks / Trade-offs

- **[Borrowed UTF-8 limits dynamic message construction]** → Accept this bootstrap boundary and add
  owning String/formatting separately when real programs demonstrate it.
- **[Stdout rendering accidentally becomes canonical logging data]** → Test semantic events with
  InMemoryLogger and keep rendering assertions scoped to StdoutLogger.
- **[Provider storage introduces a hidden allocator dependency]** → Require allocation when
  constructing a retaining provider; do not add it to Logger's capability contract.
- **[Logging receives compiler privilege for convenience]** → Gate copied user implementations and
  inspect HIR/MIR for ordinary service operations only.
- **[Direct-Wasm tests accidentally rely on process stdout]** → Run semantic parity with the
  in-memory provider and test stdout adaptation separately through the existing host boundary.

## Migration Plan

1. Archive `establish-minimal-intrinsic-boundary` and verify its archived contracts match this
   already-reconciled design. Logger remains a `service`, never a static `interface` or intrinsic.
2. Add logging values, contracts, and source modules plus generated-manifest verification.
3. Add `Effect.log` and conformance/row/tooling tests before provider execution.
4. Implement InMemoryLogger and make it the cross-engine semantic oracle.
5. Implement StdoutLogger over one complete StandardStreams write and add failure translation.
6. Add labs, pressure-program usage, deterministic artifacts, and full repository checks.

Rollback removes the new standard-library modules and fixtures. No persisted format or compatibility
adapter exists in this unreleased language.
