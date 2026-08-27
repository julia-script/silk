import * as Analysis from '@silk-lang/compiler/Analysis'
import * as Diagnostic from '@silk-lang/compiler/Diagnostic'
import * as SourceFile from '@silk-lang/compiler/SourceFile'
import * as Json from '@silk-lang/documentation/Json'
import * as DocumentationProject from '@silk-lang/documentation/Project'
import * as Console from 'effect/Console'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Path from 'effect/Path'
import * as Result from 'effect/Result'
import * as FileSourceResolver from './FileSourceResolver.js'
import * as Project from './Project.js'
import * as Report from './Report.js'
import type * as Workflow from './Workflow.js'

export interface Options {
  readonly manifestPath?: string
  readonly workingDirectory?: string
  readonly output?: string
  readonly includePrivate?: boolean
}

const writeAtomically = Effect.fnUntraced(function* (destination: string, json: string) {
  const fileSystem = yield* FileSystem.FileSystem
  const path = yield* Path.Path
  const directory = path.dirname(destination)
  yield* fileSystem.makeDirectory(directory, { recursive: true })
  yield* Effect.acquireUseRelease(
    fileSystem.makeTempFile({ directory, prefix: '.silk-doc-', suffix: '.tmp' }),
    (temporary) =>
      Effect.gen(function* () {
        yield* fileSystem.writeFileString(temporary, json)
        yield* fileSystem.rename(temporary, destination)
      }),
    (temporary) => fileSystem.remove(temporary, { force: true }).pipe(Effect.ignore),
  )
})

/** Analyzes a project and atomically writes its complete formatter-neutral documentation JSON. */
export const run = Effect.fn('DocumentationWorkflow.run')(function* (
  options: Options = {},
): Effect.fn.Return<Workflow.ExitStatus, never, FileSystem.FileSystem | Path.Path> {
  const path = yield* Path.Path
  const loaded = yield* Effect.result(
    Project.load({
      ...(options.manifestPath === undefined ? {} : { manifestPath: options.manifestPath }),
      ...(options.workingDirectory === undefined
        ? {}
        : { workingDirectory: options.workingDirectory }),
    }),
  )
  if (Result.isFailure(loaded)) {
    yield* Console.error(loaded.failure.message)
    return 2
  }
  const project = loaded.success
  const resolver = FileSourceResolver.make(project.entry.sourceRoot)
  const snapshot = yield* Analysis.make({
    root: SourceFile.make(project.entry.module, project.entry.bytes),
  }).pipe(Effect.provide(FileSourceResolver.layer(resolver)))
  const catalog = Report.catalog(resolver, Analysis.sources(snapshot), path)
  const renderedDiagnostics = Report.diagnostics(Analysis.diagnostics(snapshot), catalog)
  const resolutionFailures = Report.resolutionFailures(Analysis.resolutionFailures(snapshot))
  if (renderedDiagnostics.length > 0) yield* Console.error(renderedDiagnostics)
  if (resolutionFailures.length > 0) yield* Console.error(resolutionFailures.join('\n'))
  if (resolutionFailures.length > 0) return 2
  if (Diagnostic.hasErrors(Analysis.diagnostics(snapshot))) return 1

  const destination =
    options.output === undefined
      ? path.join(project.build.outputDirectory, 'documentation.json')
      : path.resolve(project.directory, options.output)
  const documentation = DocumentationProject.make(snapshot, {
    includePrivate: options.includePrivate ?? false,
  })
  const written = yield* Effect.result(writeAtomically(destination, Json.encode(documentation)))
  if (Result.isFailure(written)) {
    yield* Console.error(`Cannot write documentation to ${destination}`)
    return 2
  }
  yield* Console.log(`Documentation: ${destination}`)
  return 0
})
