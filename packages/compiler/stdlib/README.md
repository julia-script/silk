# Silk standard library sources

The files under `silk/` are the canonical compiler-shipped Silk modules. The generated TypeScript
manifest is only a build artifact; editors and language-server definition results point back to
these source files.

`Logger` is the portable semantic logging boundary. Each `Effect.log` or `Effect.logAt` invocation
supplies one complete borrowed UTF-8 message; callers cannot append fragments to an open event.
Providers own formatting, allocation, destinations, buffering, and physical writes. The initial
`StdoutLogger` can forward message bytes directly, while `InMemoryLogger` copies messages only
because it retains ordered observations. The bootstrap in-memory implementation is deliberately
bounded to eight events and 64 total message bytes; capacity exhaustion is a deterministic
`LogError`, not an ambient allocator requirement.

`StandardStreams` is the lower-level process-output boundary. It writes immutable byte views to
stdout or stderr and reports `StreamWriteFailure`; it does not invent log levels or Logger
requirements. It is also not the future `Stream`/`Sink` model, which will own composition, flow,
buffering, and backpressure.

`StandardStreams` and `Logger` remain explicit Effect requirements until a
general default-provider mechanism exists; neither logging nor process output receives an ambient
exception.
