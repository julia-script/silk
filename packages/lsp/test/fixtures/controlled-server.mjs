import { NodeServices } from '@effect/platform-node'
import * as Effect from 'effect/Effect'
import * as ProjectWorker from '../../dist/ProjectWorker.js'
import * as Server from '../../dist/Server.js'
import * as Workspace from '../../dist/Workspace.js'

const decoder = new TextDecoder()

Server.start({
  policy: { debounce: 5 },
  workerFactory: {
    spawn: (epoch) =>
      ProjectWorker.makeInProcess({
        epoch,
        analyze: Effect.fnUntraced(function* (documents, previous, invalidation, onProgress) {
          yield* Effect.sleep(40)
          if (
            documents.some((document) => decoder.decode(document.bytes).includes('LSP_TEST_DEFECT'))
          )
            return yield* Effect.die(new Error('controlled LSP analysis defect'))
          return yield* Workspace.analyzeProject(
            documents,
            previous,
            invalidation,
            onProgress,
          ).pipe(Effect.provide(NodeServices.layer))
        }),
      }),
  },
})
