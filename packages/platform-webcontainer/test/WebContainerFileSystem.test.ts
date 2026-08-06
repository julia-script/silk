import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Layer from 'effect/Layer'
import * as Stream from 'effect/Stream'
import * as WebContainer from '../src/WebContainer.js'
import * as WebContainerError from '../src/WebContainerError.js'
import * as WebContainerFileSystem from '../src/WebContainerFileSystem.js'
import * as VirtualFileSystem from './support/VirtualFileSystem.js'

const withVirtualFileSystem = <A, E>(
  virtual: VirtualFileSystem.VirtualFileSystem,
  effect: Effect.Effect<A, E, FileSystem.FileSystem>,
): Effect.Effect<A, E> =>
  effect.pipe(
    Effect.provide(
      WebContainerFileSystem.layer.pipe(Layer.provide(WebContainer.layerTest(virtual.service))),
    ),
  )

it.effect('supports native filesystem operations', () => {
  const virtual = VirtualFileSystem.make()
  return withVirtualFileSystem(
    virtual,
    Effect.gen(function* () {
      const fs = yield* FileSystem.FileSystem
      yield* fs.makeDirectory('src/nested', { recursive: true })
      yield* fs.writeFile('src/nested/data.bin', new Uint8Array([1, 2, 3]))
      yield* fs.writeFileString('src/main.ts', 'export const answer = 42')
      assert.deepStrictEqual([...(yield* fs.readFile('src/nested/data.bin'))], [1, 2, 3])
      assert.strictEqual(yield* fs.readFileString('src/main.ts'), 'export const answer = 42')
      assert.deepStrictEqual(yield* fs.readDirectory('src'), ['main.ts', 'nested'])
      yield* fs.rename('src/main.ts', 'src/index.ts')
      assert.isFalse(yield* fs.exists('src/main.ts'))
      assert.isTrue(yield* fs.exists('src/index.ts'))
      yield* fs.access('src/index.ts')
      yield* fs.remove('src/index.ts')
      assert.isFalse(yield* fs.exists('src/index.ts'))
    }),
  )
})

it.effect('derives recursive listing and byte-preserving copy', () => {
  const virtual = VirtualFileSystem.make()
  return withVirtualFileSystem(
    virtual,
    Effect.gen(function* () {
      const fs = yield* FileSystem.FileSystem
      yield* fs.makeDirectory('source/nested', { recursive: true })
      yield* fs.writeFile('source/root.bin', new Uint8Array([0, 255]))
      yield* fs.writeFile('source/nested/child.bin', new Uint8Array([4, 5, 6]))
      assert.deepStrictEqual(yield* fs.readDirectory('source', { recursive: true }), [
        'nested',
        'nested/child.bin',
        'root.bin',
      ])
      yield* fs.copy('source', 'copied')
      assert.deepStrictEqual([...(yield* fs.readFile('copied/root.bin'))], [0, 255])
      assert.deepStrictEqual([...(yield* fs.readFile('copied/nested/child.bin'))], [4, 5, 6])
      const overwrite = yield* Effect.result(fs.copy('source', 'copied', { overwrite: false }))
      assert.strictEqual(overwrite._tag, 'Failure')
      if (overwrite._tag === 'Failure') {
        assert.strictEqual(overwrite.failure.reason._tag, 'AlreadyExists')
      }
    }),
  )
})

it.effect('cleans scoped temporary entries and resolves lexical real paths', () => {
  const virtual = VirtualFileSystem.make()
  return withVirtualFileSystem(
    virtual,
    Effect.gen(function* () {
      const fs = yield* FileSystem.FileSystem
      let temporary = ''
      yield* Effect.scoped(
        Effect.gen(function* () {
          temporary = yield* fs.makeTempFileScoped({ prefix: 'test-', suffix: '.tmp' })
          assert.isTrue(yield* fs.exists(temporary))
        }),
      )
      assert.isFalse(yield* fs.exists(temporary))
      yield* fs.makeDirectory('src', { recursive: true })
      yield* fs.writeFileString('src/file.ts', '')
      assert.strictEqual(
        yield* fs.realPath('src/../src/file.ts'),
        '/home/projects/test/src/file.ts',
      )
    }),
  )
})

it.effect('derives truncate, ranged streams, and ordered whole-file sinks', () => {
  const virtual = VirtualFileSystem.make()
  return withVirtualFileSystem(
    virtual,
    Effect.gen(function* () {
      const fs = yield* FileSystem.FileSystem
      yield* fs.writeFile('data.bin', new Uint8Array([1, 2, 3, 4, 5, 6]))
      const streamed = yield* fs
        .stream('data.bin', { offset: 1, bytesToRead: 4, chunkSize: 2 })
        .pipe(Stream.runCollect)
      assert.deepStrictEqual(
        streamed.map((chunk) => [...chunk]),
        [
          [2, 3],
          [4, 5],
        ],
      )
      yield* fs.truncate('data.bin', 3)
      assert.deepStrictEqual([...(yield* fs.readFile('data.bin'))], [1, 2, 3])
      yield* fs.truncate('data.bin', 5)
      assert.deepStrictEqual([...(yield* fs.readFile('data.bin'))], [1, 2, 3, 0, 0])
      yield* Stream.make(new Uint8Array([7, 8]), new Uint8Array([9])).pipe(
        Stream.run(fs.sink('sink.bin')),
      )
      assert.deepStrictEqual([...(yield* fs.readFile('sink.bin'))], [7, 8, 9])
    }),
  )
})

it.effect('approximates stat without inventing host metadata', () => {
  const virtual = VirtualFileSystem.make()
  return withVirtualFileSystem(
    virtual,
    Effect.gen(function* () {
      const fs = yield* FileSystem.FileSystem
      yield* fs.makeDirectory('dir')
      yield* fs.writeFile('file.bin', new Uint8Array([1, 2, 3, 4]))
      const file = yield* fs.stat('file.bin')
      const directory = yield* fs.stat('dir')
      assert.strictEqual(file.type, 'File')
      assert.strictEqual(file.size, 4n)
      assert.strictEqual(file.mtime._tag, 'None')
      assert.strictEqual(file.ino._tag, 'None')
      assert.strictEqual(directory.type, 'Directory')
      assert.strictEqual(directory.size, 0n)
      assert.strictEqual(directory.mode, 0)
    }),
  )
})

it('classifies known and unknown boundary failures', () => {
  const missing = WebContainerFileSystem.platformError(
    'readFile',
    'missing.txt',
    WebContainerError.wrappedFailure(
      'WebContainer.fileSystem.readFile',
      'missing',
      Object.assign(new Error('no such file'), { code: 'ENOENT' }),
    ),
  )
  const unknownCause = new Error('unexpected')
  const unknown = WebContainerFileSystem.platformError(
    'readFile',
    'broken.txt',
    WebContainerError.wrappedFailure('WebContainer.fileSystem.readFile', 'broken', unknownCause),
  )
  assert.strictEqual(missing.reason._tag, 'NotFound')
  assert.strictEqual(unknown.reason._tag, 'Unknown')
  assert.strictEqual(unknown.cause, unknownCause)
})

it.effect('fails every unavailable capability explicitly', () => {
  const virtual = VirtualFileSystem.make()
  return withVirtualFileSystem(
    virtual,
    Effect.gen(function* () {
      const fs = yield* FileSystem.FileSystem
      const effects = [
        fs.chmod('file', 0o644),
        fs.chown('file', 1, 1),
        fs.glob('*.ts'),
        fs.link('from', 'to'),
        Effect.scoped(fs.open('file')),
        fs.readLink('file'),
        fs.symlink('from', 'to'),
        fs.utimes('file', 0, 0),
        fs.watch('file').pipe(Stream.runDrain),
      ]
      for (const operation of effects) {
        const result = yield* Effect.result(operation)
        assert.strictEqual(result._tag, 'Failure')
        if (result._tag === 'Failure') {
          assert.strictEqual(result.failure.reason._tag, 'Unknown')
          assert.match(result.failure.message, /unsupported/)
        }
      }
    }),
  )
})
