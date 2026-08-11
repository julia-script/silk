## Context

See `proposal.md` for motivation. Silk now has source-defined Effect combinators, arbitrary `service`
contracts, static `interface` conformances, general lexical provision, source-defined
`StandardStreams`, static UTF-8 views, and evaluator/native/direct-Wasm provider plumbing. All
compiler-callable operations live under the sealed `Intrinsic` namespace. The prerequisite change
`establish-minimal-intrinsic-boundary` is implemented and archived, and this design is reconciled
with its contracts.

Logging still needs a useful complete-event boundary without introducing a String decision or a
stream-shaped ABI.

## Goals / Non-Goals

**Goals:**

- Establish Logger as the first portable application-level service.
- Keep the log call, event, service, and initial providers visible as ordinary Silk source.
- Preserve one complete-message invocation under stdout, memory, native, and hosted-Wasm execution
  without prescribing how a provider renders or physically emits that invocation.
- Make provider failure and requirement propagation honest in the existing Effect channels.

**Non-Goals:**

- Tracing spans, metrics, filters, asynchronous exporters, batching, sampling, or OpenTelemetry wire
  formats.
- An ambient/default Logger, compiler debugging intrinsic, or automatic logging of Effects.
- An owning String, general formatting framework, interpolation, or arbitrary structured values.
- Browser console and OpenTelemetry implementations in the first change.

## Decisions

### 1. Logger receives severity and a borrowed UTF-8 view as separate arguments

The Logger operation accepts a closed `LogLevel` and one complete immutable UTF-8 message view as
separate parameters. The complete message must exist before the call; there is no begin, append,
flush, or end-event API. The provider consumes the view only for the dynamic extent of the call,
and implementations that retain observations copy into provider-owned storage. `Effect.log(message)`
selects Info, while a sibling accepts an explicit level.

This admits static literals immediately, preserves a multiline message as one semantic invocation,
avoids storing a borrowed slice inside an owned event value, and postpones the separate owning String
and formatting design. A future String can expose the same borrowed view without changing Logger.
Annotations or span context can arrive through a deliberate breaking alpha change or a richer
operation.

Alternatives considered:

- **Accept raw bytes.** Portable, but makes invalid UTF-8 and rendered bytes the semantic logging
  model.
- **Accept an owned event.** Forces allocation or ownership transfer for static messages and
  conflicts with the current rule that borrowed slices cannot be stored in owned values.
- **Wait for String.** Couples basic observability to a separate unresolved storage decision.

### 2. Logger is an ordinary nominal service and Effect.log is library code

The canonical logging module defines LogLevel, LogError, Logger, and provider types.
`silk.effects` imports the contract and defines `Effect.log` using the same requirement-binding and
Effect construction mechanisms as other source-defined combinators. No HIR, MIR, evaluator, or
backend branch matches the names `Logger` or `Effect.log`.

This keeps dependency replacement, failure rows, callable composition, and tooling navigation
uniform. It also makes a user-authored browser or telemetry Logger semantically ordinary.

### 3. Provider invocation boundaries do not prescribe physical output

The bootstrap stdout provider may forward the complete borrowed message directly through
`StandardStreams.writeAll` without allocating. It translates stream failure into LogError with the
original provider detail. Another provider may add a severity prefix, timestamp, newline, color,
structured fields, batching, or multiple physical writes. Those are provider decisions rather than
observable requirements of `Effect.log`.

The portable guarantee is one provider invocation per log call with the complete message available.
It is not one stdout write, one rendered line, or one allocation. This preserves browser console and
OpenTelemetry providers without forcing their output models through process-stream semantics.

### 4. InMemoryLogger owns observations

The bootstrap test provider copies severity and message bytes into bounded provider-owned storage
before returning. It retains up to eight events and 64 total message bytes, reports deterministic
capacity exhaustion as `LogError`, and introduces no ambient allocator requirement. Inspection
borrows the recorded observations. A configurable failure ordinal independently exercises typed
provider failure and ordering without host access.

This provider supplies semantic acceptance. StandardStreams' existing memory provider remains a
raw byte-write test double and is not reused as the Logger oracle.

### 5. Backends use existing service and stream seams

The evaluator dispatches through the provided Logger witness. Native and direct-Wasm programs
compile the same Logger implementation as ordinary Silk. StdoutLogger may reach the existing
StandardStreams native/private-Wasm boundary using its chosen write strategy, so logging adds no
Logger-specific host import. A browser can instead provide another Logger or a StandardStreams
implementation without a Unix file descriptor model.

### 6. Tooling treats logging declarations as standard-library source

The canonical files enter the standard-library manifest and embedded source table. Existing module
closure, occurrence, presentation, completion, hover, and definition machinery must expose them.
Labs add one preset showing the Logger requirement, provider implementation, HIR/MIR composition,
events, and backend artifacts through the facade.

## Risks / Trade-offs

- **[Borrowed UTF-8 limits dynamic message construction]** -> Accept this minimal boundary and make
  owning String plus formatting the natural follow-up change rather than coupling it to Logger.
- **[One provider's rendering accidentally becomes canonical logging data]** -> Test invocation
  boundaries with InMemoryLogger and keep provider-specific output assertions local to that provider.
- **[Provider storage introduces a hidden allocator dependency]** → Keep the bootstrap retaining
  provider bounded and allocation-free; report capacity through `LogError` and leave dynamically
  allocated retaining providers as ordinary future implementations.
- **[Logging receives compiler privilege for convenience]** → Gate copied user implementations and
  inspect HIR/MIR for ordinary service operations only.
- **[Direct-Wasm tests accidentally rely on process stdout]** → Run semantic parity with the
  in-memory provider and test stdout adaptation separately through the existing host boundary.

## Migration Plan

1. Completed prerequisite: archive `establish-minimal-intrinsic-boundary` and reconcile Logger as a
   `service`, never a static `interface` or intrinsic.
2. Add logging values, contracts, and source modules plus generated-manifest verification.
3. Add `Effect.log` and conformance/row/tooling tests before provider execution.
4. Implement the bounded InMemoryLogger and make it the cross-engine semantic oracle.
5. Implement the allocation-free direct StdoutLogger adapter and add failure translation.
6. Add labs, pressure-program usage, deterministic artifacts, and full repository checks.

Rollback removes the new standard-library modules and fixtures. No persisted format or compatibility
adapter exists in this unreleased language.
