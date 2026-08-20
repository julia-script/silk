import { assert, it } from '@effect/vitest'
import * as ProjectAnalysis from '@silk-effect/compiler/ProjectAnalysis'
import * as SourceFile from '@silk-effect/compiler/SourceFile'
import * as SourceResolver from '@silk-effect/compiler/SourceResolver'
import * as WorkspaceInventory from '@silk-effect/compiler/WorkspaceInventory'
import * as Effect from 'effect/Effect'
import * as Exit from 'effect/Exit'
import * as Option from 'effect/Option'
import * as Document from '../src/Document.js'
import * as ProjectSession from '../src/ProjectSession.js'

const encoder = new TextEncoder()
const editCount = 50

const document = (version: number) =>
  Document.make({
    uri: 'file:///workspace/Main.silk',
    version,
    workspace: 'project:/workspace/silk.toml',
    module: 'Main',
    sourceRoot: '/workspace',
    bytes: encoder.encode(`pub fn main() -> i32 { return ${version} }`),
  })

const utilityDocument = Document.make({
  uri: 'file:///workspace/Util.silk',
  version: 1,
  workspace: 'project:/workspace/silk.toml',
  module: 'Util',
  sourceRoot: '/workspace',
  bytes: encoder.encode('pub fn stable() -> i32 { return 1 }'),
})

it.live('reports latest-state scheduler pressure without a latency threshold', () =>
  Effect.gen(function* () {
    let analysisStarts = 0
    let analysisCompletions = 0
    let analysisInterruptions = 0
    let active = 0
    let maximumActive = 0
    let semanticReuse = 0
    let finalEditAcceptedAt = 0
    let finalEditCommittedAt = 0
    let finalPhases: ReadonlyArray<{ readonly phase: string; readonly elapsedMs: number }> = []
    const published: Array<number> = []
    const project = yield* ProjectSession.make({
      workspace: 'project:/workspace/silk.toml',
      sourceRoot: '/workspace',
      debounce: 1,
      analyze: (documents, previous) => {
        const roots = documents.map((self) => SourceFile.make(self.module, self.bytes))
        const previousProject = previous.values().next().value?.project
        return Effect.gen(function* () {
          analysisStarts += 1
          active += 1
          maximumActive = Math.max(maximumActive, active)
          yield* Effect.sleep(4)
          const analyzed = yield* (
            previousProject === undefined
              ? ProjectAnalysis.make(roots)
              : ProjectAnalysis.revise(previousProject, roots)
          ).pipe(Effect.provide(SourceResolver.empty))
          analysisCompletions += 1
          semanticReuse += analyzed.semanticInvalidation.totals.reusable
          finalPhases = analyzed.report.map(({ phase, elapsedMs }) => ({ phase, elapsedMs }))
          const moduleUris = new Map(documents.map((self) => [self.module, self.uri]))
          const inventory = WorkspaceInventory.make()
          return new Map(
            documents.flatMap((self) => {
              const snapshot = ProjectAnalysis.view(analyzed, self.module)
              return snapshot === undefined
                ? []
                : [
                    [
                      self.uri,
                      Object.freeze({
                        document: self,
                        project: analyzed,
                        snapshot,
                        moduleUris,
                        inventory,
                      }),
                    ] as const,
                  ]
            }),
          )
        }).pipe(
          Effect.onExit((exit) =>
            Effect.sync(() => {
              active -= 1
              if (Exit.isFailure(exit)) analysisInterruptions += 1
            }),
          ),
        )
      },
      publish: Effect.fnUntraced(function* (session) {
        published.push(session.document.version)
        if (session.document.version === editCount) finalEditCommittedAt = performance.now()
      }),
    })

    yield* project.open(utilityDocument)
    yield* project.open(document(0))
    const warm = yield* project.acquire('file:///workspace/Main.silk', 0)
    assert.isTrue(Option.isSome(warm))
    analysisStarts = 0
    analysisCompletions = 0
    analysisInterruptions = 0
    semanticReuse = 0
    published.length = 0

    for (let version = 1; version <= editCount; version += 1) {
      if (version === editCount) finalEditAcceptedAt = performance.now()
      yield* project.open(document(version))
      yield* Effect.sleep(1)
    }
    const final = yield* project.acquire('file:///workspace/Main.silk', editCount)
    yield* project.shutdown()

    assert.isTrue(Option.isSome(final))
    assert.strictEqual(maximumActive, 1)
    assert.include(published, editCount)
    yield* Effect.sync(() =>
      console.log(
        JSON.stringify(
          {
            acceptedEdits: editCount,
            analysisStarts,
            analysisCompletions,
            analysisInterruptions,
            semanticReuse,
            maximumActive,
            publishedRevisions: published,
            finalEditCommitLatencyMs: finalEditCommittedAt - finalEditAcceptedAt,
            phaseDurations: finalPhases,
          },
          undefined,
          2,
        ),
      ),
    )
  }),
)
