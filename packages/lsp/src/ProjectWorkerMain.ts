import { parentPort, workerData } from 'node:worker_threads'
import { NodeServices } from '@effect/platform-node'
import * as Effect from 'effect/Effect'
import * as ManagedRuntime from 'effect/ManagedRuntime'
import * as Option from 'effect/Option'
import * as Queue from 'effect/Queue'
import * as Result from 'effect/Result'
import * as Schema from 'effect/Schema'
import * as ProjectWorkerRuntime from './ProjectWorkerRuntime.js'
import * as WorkerEpoch from './WorkerEpoch.js'
import * as WorkerProtocol from './WorkerProtocol.js'
import * as Workspace from './Workspace.js'

const Bootstrap = Schema.Struct({ epoch: WorkerEpoch.schema })
const bootstrap = Schema.decodeUnknownOption(Bootstrap)(workerData)

if (parentPort === null || Option.isNone(bootstrap)) {
  process.stderr.write('Invalid project-worker bootstrap\n')
  process.exitCode = 1
} else {
  const port = parentPort
  const managed = ManagedRuntime.make(NodeServices.layer)
  const inbox = await managed.runPromise(Queue.unbounded<unknown>())
  port.on('message', (input: unknown) => {
    Queue.offerUnsafe(inbox, input)
  })
  const program = Effect.gen(function* () {
    const runtime = yield* ProjectWorkerRuntime.make({
      epoch: bootstrap.value.epoch,
      analyze: Workspace.analyzeProject,
      emit: Effect.fnUntraced(function* (message) {
        yield* Effect.try(() => port.postMessage(message)).pipe(Effect.orDie)
      }),
    })
    while (true) {
      const input = yield* Queue.take(inbox)
      const decoded = WorkerProtocol.decodeHost(input)
      if (Result.isFailure(decoded)) return yield* Effect.die(decoded.failure)
      yield* runtime.accept(decoded.success)
      if (decoded.success._tag === 'Shutdown') return
    }
  })
  void managed
    .runPromise(program)
    .then(() => {
      port.close()
      return managed.dispose()
    })
    .catch((cause: unknown) => {
      process.stderr.write(`${String(cause)}\n`)
      process.exitCode = 1
      port.close()
      return managed.dispose()
    })
}
