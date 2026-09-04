import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Layout from '../src/Layout.js'
import * as LayoutVerify from '../src/LayoutVerify.js'
import type * as Mir from '../src/Mir.js'
import * as MirVerification from '../src/MirVerification.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Driver from './support/TestDriver.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const source = (text: string, target: string) =>
  Analysis.ofSourceRealized('usize/program', ascii(text), target)

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-usize-'))
afterAll(() => {
  rmSync(destinationRoot, { recursive: true, force: true })
})

const nativeExact = `import silk.usize as usize
fn increment(value: usize) -> usize { return usize.add(value, 1) }
pub fn main() -> i32 {
  if increment(9007199254740993) == 9007199254740994 { return 42 }
  return 0
}`

const sharedUnsigned = `import silk.usize as usize
fn maximum() -> usize { return 4294967293 |> usize.add(2) }
pub fn main() -> i32 {
  if maximum() > 2147483647 { return 42 }
  return 0
}`

it.effect('retains exact contextual magnitudes and target-owned usize layout facts', () =>
  Effect.gen(function* () {
    const snapshot = yield* source(nativeExact, 'aarch64-apple-darwin')
    assert.deepEqual(snapshot.diagnostics, [])

    const integers = Analysis.expressionsOf(snapshot, 'usize/program').flatMap((expression) =>
      expression._tag === 'Integer' && expression.integer._tag === 'Available'
        ? [expression.integer]
        : [],
    )
    assert.include(
      integers.map((integer) => integer.value),
      9007199254740993n,
    )
    const plan = Analysis.layoutOf(snapshot)
    assert.strictEqual(plan._tag, 'Available')
    if (plan._tag !== 'Available') return
    const usize = Layout.entry(plan.value, 'usize')
    assert.deepEqual(usize?.representation, { _tag: 'UnsignedInteger', bits: 64 })
    // The program's own three usize literals. Reached library modules contribute their own
    // verdicts — silk/usize's shared ZERO and ONE among them — so the count names this module.
    assert.strictEqual(
      plan.value.literalVerdicts.filter((verdict) => verdict.span.sourceId === 'usize/program')
        .length,
      3,
    )
    assert.strictEqual(
      plan.value.literalVerdicts.every((verdict) => verdict._tag === 'AvailableWordLiteral'),
      true,
    )
  }),
)

it.effect('rejects an exact usize magnitude outside the selected target before MIR', () =>
  Effect.gen(function* () {
    const snapshot = yield* source(
      'fn wide() -> usize { return 4294967296 } pub fn main() -> i32 { if wide() == 0 { return 1 } return 0 }',
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(
      snapshot.diagnostics.map((diagnostic) => diagnostic.code),
      ['LAY0001'],
    )
    const diagnostic = snapshot.diagnostics.at(0)
    assert.strictEqual(diagnostic?.reason._tag, 'WordLiteralOutOfRange')
    if (diagnostic?.reason._tag === 'WordLiteralOutOfRange') {
      assert.strictEqual(diagnostic.reason.type, 'usize')
      assert.strictEqual(diagnostic.reason.spelling, '4294967296')
      assert.strictEqual(diagnostic.reason.bits, 32)
      assert.strictEqual(diagnostic.reason.target, 'wasm32-unknown-unknown')
    }
    assert.strictEqual(Analysis.mirOf(snapshot)._tag, 'Unavailable')
  }),
)

it.effect('lowers native usize lanes and operations as unsigned i64', () =>
  Effect.gen(function* () {
    const snapshot = yield* source(nativeExact, 'aarch64-apple-darwin')
    const artifact = yield* Analysis.codegen(snapshot, { mode: 'release' })

    assert.include(artifact.ir, 'i64 9007199254740993')
    assert.include(artifact.ir, 'llvm.uadd.with.overflow.i64')
    assert.include(artifact.ir, 'icmp eq i64')
  }),
)

it.effect('executes an exact native i64 call and rejects it for the WebAssembly target', () =>
  Effect.gen(function* () {
    const native = yield* Driver.compile({
      compilation: { root: SourceFile.make('usize/program', ascii(nativeExact)) },
      toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang', llvmAr: 'llvm-ar' }),
      profile: 'release',
      artifactKind: 'NativeExecutable',
      destination: join(destinationRoot, 'native-exact'),
    }).pipe(Effect.provide(SourceResolver.empty))
    assert.strictEqual(native._tag, 'Compiled')
    if (native._tag !== 'Compiled') return
    const executed = spawnSync(native.path, [], { encoding: 'utf8' })
    assert.strictEqual(executed.status, 42, executed.stderr)

    const wasm = yield* source(nativeExact, 'wasm32-unknown-unknown')
    assert.include(
      wasm.diagnostics.map((diagnostic) => diagnostic.code),
      'LAY0001',
    )
    assert.strictEqual(Analysis.mirOf(wasm)._tag, 'Unavailable')
  }),
)

it.effect('discovers one generic usize instance independently of literal magnitude', () =>
  Effect.gen(function* () {
    const snapshot = yield* source(
      `fn identity<T>(value: T) -> T { return move value }
pub fn main() -> i32 {
  if identity<usize>(1) + identity<usize>(4294967294) == 4294967295 { return 42 }
  return 0
}`,
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(snapshot.diagnostics, [])
    const identities = Analysis.instancesOf(snapshot).instances.filter(
      (instance) => instance.key.declaration.name === 'identity',
    )
    assert.strictEqual(identities.length, 1)
    assert.deepEqual(identities.at(0)?.key.typeArguments, ['usize'])
  }),
)

it.effect('rejects malformed usize target verdicts and MIR literals as verifier data', () =>
  Effect.gen(function* () {
    const snapshot = yield* source(sharedUnsigned, 'wasm32-unknown-unknown')
    const layout = Analysis.layoutOf(snapshot)
    assert.strictEqual(layout._tag, 'Available')
    if (layout._tag !== 'Available') return
    const first = layout.value.literalVerdicts.at(0)
    if (first === undefined) return assert.fail('expected a usize literal verdict')
    const malformedLayout: Layout.Plan = {
      ...layout.value,
      literalVerdicts: [{ ...first, bits: 64 }],
    }
    assert.include(
      LayoutVerify.verify(malformedLayout).map((violation) => violation.rule),
      'InvalidLiteralVerdict',
    )

    const program = Analysis.loweredMir(snapshot)
    const malformed: Mir.Module = {
      ...program,
      functions: program.functions.map((fn) => ({
        ...fn,
        regions: fn.regions.map((region) => {
          if (region._tag !== 'OperationRegion') return region
          return {
            ...region,
            operations: region.operations.map((operation) => {
              if (operation._tag !== 'Literal' || operation.type._tag !== 'usize') return operation
              return { ...operation, value: 4294967296n }
            }),
          }
        }),
      })),
    }
    assert.include(
      MirVerification.verify(malformed).map((violation) => violation.rule),
      'InvalidIntegerOperation',
    )
  }),
)
