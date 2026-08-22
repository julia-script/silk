import * as Effect from 'effect/Effect'
import type * as Backend from './Backend.js'
import { BackendError, terminationOf } from './Backend.js'
import type * as Mir from './Mir.js'
import { llvmControl } from './MirLinearization.js'
import * as NativeProgram from './NativeProgram.js'
import * as Target from './Target.js'

/** The bootstrap LLVM backend over the Silk LLVM builder. */
export const LlvmBackend: Backend.Backend<Backend.LlvmBitcodeArtifact> = Object.freeze({
  _tag: 'Backend',
  id: 'llvm',
  name: 'LLVM',
  targets: Object.freeze([
    ...Target.native.map((target) => target.id),
    Target.wasm32UnknownUnknown.id,
  ]),
  emit: Effect.fn('Backend.LLVM.emit')(function* (
    program: Mir.Module,
    request: Backend.CodegenRequest,
  ): Effect.fn.Return<Backend.LlvmBitcodeArtifact, BackendError> {
    const output = yield* NativeProgram.emit(program, request).pipe(
      Effect.catchTag('LlvmError', (cause) =>
        Effect.fail(
          new BackendError({
            operation: 'Backend.emit',
            backend: 'LLVM',
            message: `LLVM emission failed for ${program.module}`,
            reason: { _tag: 'WrappedFailure', cause },
          }),
        ),
      ),
    )
    return Object.freeze({
      _tag: 'LlvmBitcodeArtifact',
      backend: 'llvm',
      module: program.module,
      target: program.layout.target,
      symbols: Object.freeze(output.symbols),
      termination: terminationOf(program),
      nativeRuntimeSymbols: output.nativeRuntimeSymbols,
      control: llvmControl(program),
      bitcode: output.bitcode,
      ir: output.ir,
    })
  }),
})
