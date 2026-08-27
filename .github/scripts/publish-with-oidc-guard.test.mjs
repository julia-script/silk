import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import { Readable } from 'node:stream'
import { test } from 'node:test'

import { publishWithGuard, runPublish } from './publish-with-oidc-guard.mjs'

function fakeSpawn({ stdout = '', stderr = '', code = 0 }) {
  return () => {
    const child = new EventEmitter()
    child.stdout = Readable.from([stdout])
    child.stderr = Readable.from([stderr])

    let openStreams = 2
    const closeAfterStreamsDrain = () => {
      openStreams--
      if (openStreams === 0) setImmediate(() => child.emit('close', code))
    }
    child.stdout.on('end', closeAfterStreamsDrain)
    child.stderr.on('end', closeAfterStreamsDrain)

    return child
  }
}

test('forwards publish output', async () => {
  const output = 'New tag: @silklang/llvm@0.1.0\n'
  let seen = ''
  const code = await runPublish('pnpm', ['run', 'release:publish'], {
    onOutput: (chunk) => {
      seen += chunk
    },
    spawnImpl: fakeSpawn({ stdout: output }),
  })

  assert.equal(code, 0)
  assert.equal(seen, output)
})

test('passes through a clean publish', async () => {
  const result = await publishWithGuard({
    spawnImpl: fakeSpawn({ stdout: 'New tag: @silklang/llvm@0.1.0\n' }),
  })

  assert.deepEqual(result, { exitCode: 0, skippedOidc: false })
})

test('detects a skipped OIDC exchange', async () => {
  const result = await publishWithGuard({
    spawnImpl: fakeSpawn({ stderr: 'Skipped OIDC: no token available\n' }),
  })

  assert.deepEqual(result, { exitCode: 0, skippedOidc: true })
})

test('propagates a failed publish', async () => {
  const result = await publishWithGuard({ spawnImpl: fakeSpawn({ code: 1 }) })

  assert.deepEqual(result, { exitCode: 1, skippedOidc: false })
})
