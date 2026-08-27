import { NodeRuntime, NodeServices } from '@effect/platform-node'
import * as LlvmError from '@silk-lang/llvm/LlvmError'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Stdio from 'effect/Stdio'
import * as Stream from 'effect/Stream'
import * as Compiler from './Compiler.js'
import * as Diagnostic from './Diagnostic.js'

const renderDiagnostic = (error: unknown): string => {
  if (
    error instanceof Diagnostic.LexError ||
    error instanceof Diagnostic.ParseError ||
    error instanceof Diagnostic.ResolutionError ||
    error instanceof Diagnostic.CompileError
  ) {
    return `${error._tag}: ${error.message} [${error.start}, ${error.end})`
  }
  if (error instanceof LlvmError.LlvmError) {
    return `LlvmError: ${error.operation}: ${error.message}`
  }
  if (error instanceof Diagnostic.CliError) return `CliError: ${error.message}`
  if (typeof error === 'object' && error !== null && 'message' in error) {
    const message = error.message
    if (typeof message === 'string') return message
  }
  return String(error)
}

const program = Effect.gen(function* () {
  const stdio = yield* Stdio.Stdio
  const fileSystem = yield* FileSystem.FileSystem
  const args = yield* stdio.args
  const sourcePath = args.at(0)
  if (sourcePath === undefined) {
    return yield* new Diagnostic.CliError({ message: 'Usage: tiny <source.tiny>' })
  }

  const source = yield* fileSystem.readFileString(sourcePath)
  const compilation = yield* Compiler.compile(source)
  yield* Stream.make(compilation.ir).pipe(Stream.run(stdio.stdout()))
}).pipe(
  Effect.tapError((error) =>
    Stdio.Stdio.use((stdio) =>
      Stream.make(`${renderDiagnostic(error)}\n`).pipe(Stream.run(stdio.stderr())),
    ),
  ),
  Effect.provide(NodeServices.layer),
)

NodeRuntime.runMain(program, { disableErrorReporting: true })
