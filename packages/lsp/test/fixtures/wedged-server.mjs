import * as ProjectWorker from '../../dist/ProjectWorker.js'
import * as Server from '../../dist/Server.js'

const wedgedWorker = new URL('./non-cooperative-worker.mjs', import.meta.url)
let spawns = 0

Server.start({
  policy: {
    debounce: 5,
    noProgressLease: 5_000,
    retirementDeadline: 2_000,
    queryDeadline: 4_000,
    diagnosticDeadline: 4_000,
  },
  workerFactory: {
    spawn: (epoch) => {
      spawns += 1
      return spawns === 1
        ? ProjectWorker.makeNode(epoch, wedgedWorker)
        : ProjectWorker.makeNode(epoch)
    },
  },
})
