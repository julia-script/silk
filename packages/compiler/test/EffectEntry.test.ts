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
  'unhandled error: effect-entry/native.SomeError\n  at effect-entry/native.main (effect-entry/native:2:41)\n'

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
      toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang', llvmAr: 'llvm-ar' }),
      profile: 'release',
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
    assert.strictEqual(closedStderr.status, 2)

    const succeeded = yield* Driver.compile({
      compilation: {
        root: SourceFile.make('effect-entry/native-success', ascii(successSource)),
      },
      toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang', llvmAr: 'llvm-ar' }),
      profile: 'release',
      artifactKind: 'NativeExecutable',
      destination: join(destinationRoot, 'native-success'),
    }).pipe(Effect.provide(SourceResolver.empty))
    assert.strictEqual(succeeded._tag, 'Compiled')
    if (succeeded._tag !== 'Compiled') return
    const successRun = spawnSync(succeeded.path, [], { encoding: 'utf8' })
    assert.strictEqual(successRun.status, 0)
    assert.strictEqual(successRun.stderr, '')

    // The native entry receives the process command line so a host-input provider can read it. A
    // program that never reads it keeps the exact same statuses and report bytes with arguments
    // present, so the entry shape change is invisible to every existing program.
    const withArguments = spawnSync(compiled.path, ['one', 'two', 'three'], { encoding: 'utf8' })
    assert.strictEqual(withArguments.status, 1)
    assert.strictEqual(withArguments.stderr, nativeFailureReport)
    const closedWithArguments = spawnSync(
      '/bin/sh',
      ['-c', 'exec 2>&-; exec "$1" one two', 'silk-effect-entry', compiled.path],
      { encoding: 'utf8' },
    )
    assert.strictEqual(closedWithArguments.status, 2)
    const successWithArguments = spawnSync(succeeded.path, ['one', 'two'], { encoding: 'utf8' })
    assert.strictEqual(successWithArguments.status, 0)
    assert.strictEqual(successWithArguments.stderr, '')
  }),
)
