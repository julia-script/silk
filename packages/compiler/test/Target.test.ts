import { readFileSync } from 'node:fs'
import { dirname, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as HeapObservation from '../src/HeapObservation.js'
import * as Target from '../src/Target.js'

it('keeps the compiler root import graph free of Node built-ins', () => {
  const sourceRoot = resolve(dirname(fileURLToPath(import.meta.url)), '../src')
  const pending = [resolve(sourceRoot, 'index.ts')]
  const visited = new Set<string>()
  while (pending.length > 0) {
    const file = pending.pop()
    if (file === undefined || visited.has(file)) continue
    visited.add(file)
    const source = readFileSync(file, 'utf8')
    assert.notMatch(source, /from ['"]node:/, file)
    for (const match of source.matchAll(/from ['"](\.\.?\/[^'"]+)\.js['"]/g)) {
      const relative = match[1]
      if (relative !== undefined) pending.push(resolve(dirname(file), `${relative}.ts`))
    }
  }
  assert.isAbove(visited.size, 1)
})

it.effect('provides browser heap observation only when the browser layer is explicit', () =>
  Effect.gen(function* () {
    const observation = yield* HeapObservation.HeapObservation
    assert.strictEqual(observation.heapBytes(), 0)
  }).pipe(Effect.provide(HeapObservation.layerBrowser)),
)

it('defines the four canonical profiles in deterministic order', () => {
  assert.deepEqual(
    Target.all.map((target) => target.id),
    [
      'aarch64-apple-darwin',
      'aarch64-unknown-linux-gnu',
      'wasm32-unknown-unknown',
      'x86_64-unknown-linux-gnu',
    ],
  )
  assert.deepEqual(
    Target.native.map((target) => target.id),
    ['aarch64-apple-darwin', 'aarch64-unknown-linux-gnu', 'x86_64-unknown-linux-gnu'],
  )
  assert.strictEqual(Target.wasm32UnknownUnknown.pointerSize, 4)
  assert.strictEqual(
    Target.native.every((target) => target.pointerSize === 8),
    true,
  )
})

it.effect('resolves every exact canonical identity', () =>
  Effect.gen(function* () {
    for (const target of Target.all) {
      assert.strictEqual(yield* Target.resolve(target.id), target)
      assert.strictEqual(Target.isCanonical(target), true)
    }
  }),
)

it.effect('resolves each supported native host', () =>
  Effect.gen(function* () {
    assert.strictEqual((yield* Target.fromHost('darwin', 'arm64')).id, 'aarch64-apple-darwin')
    assert.strictEqual((yield* Target.fromHost('linux', 'x64')).id, 'x86_64-unknown-linux-gnu')
    assert.strictEqual((yield* Target.fromHost('linux', 'arm64')).id, 'aarch64-unknown-linux-gnu')
  }),
)

it.effect('rejects unsupported requests and hosts as typed errors', () =>
  Effect.gen(function* () {
    const target = yield* Effect.flip(Target.resolve('mips-unknown-linux-gnu'))
    const host = yield* Effect.flip(Target.fromHost('win32', 'x64'))
    assert.strictEqual(target._tag, 'TargetError')
    assert.strictEqual(target.operation, 'Target.resolve')
    assert.strictEqual(host.operation, 'Target.host')
  }),
)

it('encodes target facts deterministically', () => {
  assert.strictEqual(
    Target.encode(Target.wasm32UnknownUnknown),
    'wasm32-unknown-unknown kind=WebAssembly pointer=4/4 endian=little',
  )
})
