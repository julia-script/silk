# Silk standard library sources

The files under `silk/` are the canonical compiler-shipped Silk modules. The generated TypeScript
manifest is only a build artifact; editors and language-server definition results point back to
these source files.

`StandardStreams` is the minimal process-output boundary. It writes complete immutable byte views
to stdout or stderr and reports `StreamWriteFailure`. It is not a logging API: the planned portable
`Logger` receives complete semantic events through `Effect.log` and decides whether to render them
to standard output, retain them in memory, send them to a browser or OpenTelemetry provider, or fan
them out. It is also not the future `Stream`/`Sink` model, which will own composition, flow,
buffering, and backpressure.

`StandardStreams` remains an explicit Effect requirement. Logger will also remain explicit until a
general default-provider mechanism exists; neither logging nor process output receives an ambient
exception.
