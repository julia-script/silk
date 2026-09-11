## Context

See `proposal.md` for motivation. The current Logger service receives a finished string, whereas
`Format.format` residualizes directly to Writer and Display operations. Formatting outside Logger
would either fragment one semantic event or require caller-visible storage/error capabilities. The
existing providers already own the relevant boundary: `StdoutLogger` translates Writer failure to
LogError, and `InMemoryLogger` owns bounded retained message storage.

## Goals / Non-Goals

**Goals:**

- Keep one generic Logger service operation and one family of thin Effect conveniences.
- Reuse the existing template parser, reflection, Display selection, and Writer residual path.
- Preserve a runtime-selectable `LogLevel` and the exact public Logger/LogError channels.
- Keep retained events atomic without imposing allocation or transactional host output.

**Non-Goals:**

- Compile-time severity filtering, structured fields, spans, metadata, or a new formatting grammar.
- A general formatted-String builder or any new compiler privilege.
- Compatibility overloads for message-only logging.

## Decisions

### Make Logger.log generic over the argument pack

`Logger.log<Args>(level, static template, args)` becomes the single service operation. Effect helpers
only choose a level and forward those inputs. This puts rendering inside the provider's one semantic
invocation and lets each provider choose a suitable Writer.

Formatting only in the Effect façade was rejected because no unbounded, allocation-free temporary
String exists, and forwarding Writer fragments as Logger calls would violate event identity. Adding
Writer or Allocator to the public Effect row was rejected because logging already owns the output
boundary and promises only Logger/LogError.

### Keep LogLevel runtime-selectable

`Effect.logAt` retains a runtime `LogLevel`; the named variants pass fixed enum members. The static
template is required for validation, but a static severity provides no validation benefit. Zig's
compile-time severity supports compile-time filtering, which is outside this change; applying it here
would remove current dynamic selection and create needless specializations.

### Render stdout directly and stage retained events

`StdoutLogger` provides its local `StdoutWriter` to `Format.format`, then maps any WriterError to the
existing LogError code. Several physical writes may occur inside one Logger invocation, and an I/O
failure may leave a physical prefix, matching Writer semantics.

`InMemoryLogger` formats into a private fixed-capacity Writer bounded by the event storage still
available. Only after formatting succeeds does it copy the staged bytes and severity into the event
arrays and advance committed counts. Its attempt count advances once before formatting. A Writer
capacity failure is translated to the existing capacity LogError and discards the private stage, so
retained event state is atomic without allocation.

Alternatives using the retained arrays themselves as the Writer were rejected because rollback would
be error-prone and partial bytes would become observable through mutable provider state. Allocating a
temporary was rejected because it adds Allocator and OutOfMemoryError to a deliberately bounded
bootstrap provider.

### Migrate the surface atomically

Every internal caller supplies `&()` for a literal-only message or passes a real tuple/record pack.
The service, both providers, helpers, examples, project initializer fixtures, shared runtime corpus,
specifications, generated stdlib embedding, and generated documentation change together. No old
overload or adapter remains under the repository's green-field policy.

## Risks / Trade-offs

- [Generic service dispatch exposes a compiler gap] → Cover static service-operation arguments from
  analysis through HIR, residualization, Effect lowering, runner retention, and verification with a
  focused regression before relying on the capability in the logging implementation.
- [A retained message exceeds bounded capacity] → Fail with the provider's existing capacity
  LogError after one attempt and before committing any event metadata or bytes.
- [Direct formatting increases physical stdout writes] → Accept this provider-local trade-off; the
  contract permits multiple physical writes, avoids mandatory allocation, and preserves one semantic
  invocation.
- [Generated surfaces drift] → Regenerate and check the embedded stdlib and documentation, then run
  release-candidate validation because package contents change.

## Migration Plan

Land the breaking service/provider/helper migration in one commit series, update every repository
caller in the same branch, regenerate derived artifacts, and verify no one-argument logging calls or
message-only Logger signature remain. Rollback is a revert of the complete change; no dual API is
maintained.
