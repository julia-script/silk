# Context

`Backend.emit` must remain pure backend code generation from MIR. A native object is a materialized
toolchain artifact, while a linked executable/library is the result of consuming a complete physical
plan and inputs whose lifetimes are owned by an enclosing build scope.

# Decisions

- `ObjectEmission.materialize` owns bitcode-to-object execution and returns object inventory,
  helper requirements, command metadata, and a scope-bound path artifact.
- Helper, runtime, translation-unit, and native-input preparation remains outside `Linker.link`.
- `Linker.link` validates the complete plan, applies `Disabled | ReadWrite` cache policy, executes or
  reuses the final artifact, commits it, and returns plan/cache metadata.
- The caller passes the build scope explicitly and remains responsible for its cleanup after durable
  outputs have been committed.
- NativeToolchain keeps only internal primitives beneath these actor boundaries.

# Risks

Moving cache policy can alter phase labels. The linker exposes `metadata.reused` so reporting can
distinguish reuse without duplicating cache reads in the driver.
