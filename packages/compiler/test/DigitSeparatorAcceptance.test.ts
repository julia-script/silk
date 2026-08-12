import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Driver from '../src/Driver.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-digit-separator-acceptance-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

const parity = `const maximumBytes: i32 = 1_048_576
const flags: i32 = 0b1010_0000_1111_0001
const ratio: f64 = 3.141_592

fn identity(value: i32) -> i32 { return value }

pub fn main() -> i32 {
  let separated = 1_000
  let plain = 1000
  let hexadecimal = 0xff_ff
  let octal = 0o1_7
  if separated == plain {} else { return 1 }
  if maximumBytes == 1048576 {} else { return 2 }
  if flags == 41201 {} else { return 3 }
  if hexadecimal == 65535 {} else { return 4 }
  if octal == 15 {} else { return 5 }
  if ratio == 3.141592 {} else { return 6 }
  return identity(separated) - 958
}`

it.effect(
  'reads separated literals as their separator-free values on all three engines',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'digit-separator/parity',
        ascii(parity),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])

      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(
        evaluated._tag,
        'Completed',
        JSON.stringify(evaluated, (_, value) =>
          typeof value === 'bigint' ? value.toString() : value,
        ),
      )
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 42)

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42)

      const compiled = yield* Driver.compile({
        compilation: { root: SourceFile.make('digit-separator/parity', ascii(parity)) },
        toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
        profile: 'release',
        destination: join(destinationRoot, 'parity'),
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.strictEqual(compiled._tag, 'Compiled')
      if (compiled._tag !== 'Compiled') return
      const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
      assert.strictEqual(run.status, 42, run.stderr)
    }),
  60_000,
)

it.effect('keeps a separated float equal to its separator-free spelling', () =>
  Effect.gen(function* () {
    const source = `pub fn main() -> i32 {
  let separated = 1.000_5
  let plain = 1.0005
  if separated == plain {} else { return 1 }
  return 42
}`
    const snapshot = yield* Analysis.ofSourceRealized('digit-separator/float', ascii(source))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42)
  }),
)

it.effect('rejects a misplaced separator before parsing', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'digit-separator/trailing',
      ascii('pub fn main() -> i32 { return 1_ }'),
    )
    assert.include(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      'LEX0005',
    )
    assert.strictEqual(
      Analysis.diagnostics(snapshot).filter((diagnostic) => diagnostic.code === 'LEX0005').length,
      1,
    )
  }),
)
