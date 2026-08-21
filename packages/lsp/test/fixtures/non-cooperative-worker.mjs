import { parentPort, workerData } from 'node:worker_threads'

const epoch = workerData.epoch

parentPort.on('message', (message) => {
  switch (message._tag) {
    case 'Initialize':
      parentPort.postMessage({ protocolVersion: 1, _tag: 'Ready', epoch })
      return
    case 'Analyze':
      parentPort.postMessage({
        protocolVersion: 1,
        _tag: 'Progress',
        epoch,
        generation: message.generation,
        phase: 'EnteringNonCooperativeWork',
      })
      // Deliberately blocks this worker's event loop so the host must terminate the thread.
      Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0)
  }
})
