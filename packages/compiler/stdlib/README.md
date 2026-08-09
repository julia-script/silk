# Silk standard library sources

The files under `silk/` are the canonical compiler-shipped Silk modules. The generated TypeScript
manifest is only a build artifact; editors and language-server definition results point back to
these source files.

`StandardStreams` is the minimal host-output boundary. It writes complete immutable byte views to
stdout or stderr and reports `StreamWriteFailure`. It is not a logging API: Logger owns structured
events, levels, spans, OpenTelemetry routing, and fan-out. It is also not the future `Stream`/`Sink`
model, which will own composition, flow, buffering, and backpressure.

For now `StandardStreams` is an explicit effect requirement. A future default-provider mechanism
may supply overridable services at an application boundary, but that mechanism must apply uniformly
to every service rather than making Logger or process streams ambient exceptions.
