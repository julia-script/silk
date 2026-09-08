import * as TestToolchain from './support/TestToolchain.js'
import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Driver from './support/TestDriver.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const failureSource = `pub struct SomeError { code: i32 }
pub effect fn main() -> () ! SomeError { fail SomeError { code: 42 } }`

const nativeFailureReport =
  'unhandled error: effect-entry/native.SomeError\n  at effect-entry/native.main (effect-entry/native:2:41)\n  at silk/effect.Effect.flatMap (silk/effect:294:18)\n'

const successSource = `pub struct SomeError { code: i32 }
pub effect fn main() -> () ! SomeError { return () }`

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-effect-entry-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

it.effect('reports an unhandled effect entry through the native runtime', () =>
  Effect.gen(function* () {
    const compiled = yield* Driver.compile({
      compilation: {
        root: SourceFile.make('effect-entry/native', ascii(failureSource)),
      },
      toolchain: yield* TestToolchain.configured,
      optimization: 'release',
      artifactKind: 'NativeExecutable',
      destination: join(destinationRoot, 'native-failure'),
    }).pipe(Effect.provide(SourceResolver.empty))
    assert.strictEqual(compiled._tag, 'Compiled')
    if (compiled._tag !== 'Compiled') return
    const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
    assert.strictEqual(run.status, 1)
    assert.strictEqual(run.stderr, nativeFailureReport)
    const closedStderr = spawnSync(
      '/bin/sh',
      ['-c', 'exec 2>&-; exec "$1"', 'silk-effect-entry', compiled.path],
      { encoding: 'utf8' },
    )
    assert.strictEqual(closedStderr.status, 1)

    const succeeded = yield* Driver.compile({
      compilation: {
        root: SourceFile.make('effect-entry/native-success', ascii(successSource)),
      },
      toolchain: yield* TestToolchain.configured,
      optimization: 'release',
      artifactKind: 'NativeExecutable',
      destination: join(destinationRoot, 'native-success'),
    }).pipe(Effect.provide(SourceResolver.empty))
    assert.strictEqual(succeeded._tag, 'Compiled')
    if (succeeded._tag !== 'Compiled') return
    const successRun = spawnSync(succeeded.path, [], { encoding: 'utf8' })
    assert.strictEqual(successRun.status, 0)
    assert.strictEqual(successRun.stderr, '')

    // Source startup owns process inputs; unused arguments preserve its status and report policy.
    const withArguments = spawnSync(compiled.path, ['one', 'two', 'three'], { encoding: 'utf8' })
    assert.strictEqual(withArguments.status, 1)
    assert.strictEqual(withArguments.stderr, nativeFailureReport)
    const closedWithArguments = spawnSync(
      '/bin/sh',
      ['-c', 'exec 2>&-; exec "$1" one two', 'silk-effect-entry', compiled.path],
      { encoding: 'utf8' },
    )
    assert.strictEqual(closedWithArguments.status, 1)
    const successWithArguments = spawnSync(succeeded.path, ['one', 'two'], { encoding: 'utf8' })
    assert.strictEqual(successWithArguments.status, 0)
    assert.strictEqual(successWithArguments.stderr, '')
  }),
)
