/**
 * The minimal WebAssembly surface `Panels.execute` uses.
 *
 * Every runtime this package targets (browsers for the docs workbench, Node for the language
 * server) provides the global, but the workspace compiles against a bare ES lib that does not
 * declare it — and pulling in the whole DOM lib for two constructors would be the larger lie.
 */
declare namespace WebAssembly {
  class Module {
    constructor(bytes: Uint8Array)
  }
  class Instance {
    constructor(module: Module, imports?: Record<string, Record<string, unknown>>)
    readonly exports: Record<string, unknown>
  }
}
