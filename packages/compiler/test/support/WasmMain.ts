import * as WasmError from '@silklang/wasm/WasmError'
import * as Effect from 'effect/Effect'

export const invoke = Effect.fnUntraced(function* (bytes: Uint8Array, operation: string) {
  return yield* Effect.try({
    try: () => {
      const instance = new WebAssembly.Instance(new WebAssembly.Module(bytes.slice()), {})
      const main = instance.exports.silk_main
      if (typeof main !== 'function') throw new TypeError('Wasm module omitted silk_main')
      const result = main()
      if (typeof result !== 'number') throw new TypeError('Wasm silk_main returned a non-number')
      return result
    },
    catch: (cause) =>
      WasmError.wrappedFailure({
        operation,
        message: 'The host could not execute the emitted WebAssembly entry',
        cause,
      }),
  })
})
