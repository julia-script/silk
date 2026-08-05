/**
 * Minimal ambient typing for the WebAssembly JavaScript API used by tests. The package's
 * compiler lib is ES2023 only, so the runtime global is declared here rather than pulling in the
 * full DOM lib.
 */
declare namespace WebAssembly {
  class Module {
    private readonly moduleBrand: unique symbol
    private constructor()
  }
  class Instance {
    readonly exports: Record<string, unknown>
    constructor(module: Module, imports?: Record<string, Record<string, unknown>>)
  }
  class Global {
    readonly value: unknown
    constructor(descriptor: { value: string; mutable?: boolean }, initial?: unknown)
  }
  function validate(bytes: ArrayBufferView | ArrayBuffer): boolean
  function compile(bytes: ArrayBufferView | ArrayBuffer): Promise<Module>
  function instantiate(
    source: Module,
    imports?: Record<string, Record<string, unknown>>,
  ): Promise<Instance>
  function instantiate(
    bytes: ArrayBufferView | ArrayBuffer,
    imports?: Record<string, Record<string, unknown>>,
  ): Promise<{ module: Module; instance: Instance }>
}
