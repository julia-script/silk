import { createHash } from 'node:crypto'
import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import type * as Backend from '../src/Backend.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const nestedSource = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`

const emit = Effect.fnUntraced(function* (text: string, request: Backend.CodegenRequest) {
  const snapshot = yield* Analysis.ofSource('golden/program', ascii(text), 'aarch64-apple-darwin')
  return yield* Analysis.codegen(snapshot, request)
})

const golden = (name: string): string =>
  readFileSync(new URL(`./goldens/${name}`, import.meta.url), 'utf8')

it.effect('emits one artifact per program with deterministic symbols', () =>
  Effect.gen(function* () {
    const artifact = yield* emit(nestedSource, { mode: 'release' })

    assert.strictEqual(artifact.module, 'golden/program')
    assert.deepEqual(
      artifact.symbols.map((entry) => entry.symbol),
      ['silk_main', 'silk_1_identity'],
    )
    assert.strictEqual(artifact.symbols.at(0)?.declaration.name, 'main')
  }),
)

it.effect('matches the IR golden and the bitcode digest golden', () =>
  Effect.gen(function* () {
    const artifact = yield* emit(nestedSource, { mode: 'release' })

    assert.strictEqual(artifact.ir, golden('program.ll.txt'))
    assert.strictEqual(
      `${createHash('sha256').update(artifact.bitcode).digest('hex')}\n`,
      golden('program.bc.sha256'),
    )
  }),
)

it.effect('emits byte-identical bitcode across repeated fresh runs', () =>
  Effect.gen(function* () {
    const first = yield* emit(nestedSource, { mode: 'release' })
    const second = yield* emit(nestedSource, { mode: 'release' })

    assert.deepEqual(first.bitcode, second.bitcode)
    assert.strictEqual(first.ir, second.ir)
  }),
)

it.effect('refuses diagnosed trap bodies before backend emission', () =>
  Effect.gen(function* () {
    const result = yield* Effect.result(
      emit('pub fn main() -> I32 { return missing() }', { mode: 'release' }),
    )
    assert.strictEqual(result._tag, 'Failure')
    if (result._tag === 'Failure') assert.strictEqual(result.failure._tag, 'CodegenUnavailable')
  }),
)

it.effect('emits native debug metadata only for debug requests', () =>
  Effect.gen(function* () {
    const debug = yield* emit(nestedSource, {
      mode: 'debug',
      sources: new Map([['golden/program', ascii(nestedSource)]]),
    })
    const release = yield* emit(nestedSource, { mode: 'release' })

    assert.include(debug.ir, '!DICompileUnit(')
    assert.include(debug.ir, '!DISubprogram(')
    assert.include(debug.ir, '!dbg')
    assert.notInclude(release.ir, 'DICompileUnit')
    assert.notInclude(release.ir, '!dbg')
  }),
)

const arithmeticSource = 'pub fn main() -> I32 { return I32.subtract(I32.multiply(6, 7), 0) }'

it.effect('emits checked arithmetic through overflow intrinsics and guarded division', () =>
  Effect.gen(function* () {
    const artifact = yield* emit(arithmeticSource, { mode: 'release' })
    const division = yield* emit('pub fn main() -> I32 { return I32.divide(1, 0) }', {
      mode: 'release',
    })

    assert.include(artifact.ir, 'llvm.smul.with.overflow')
    assert.include(artifact.ir, 'llvm.ssub.with.overflow')
    assert.include(artifact.ir, 'arith_trap')
    assert.include(division.ir, 'sdiv')
    assert.include(division.ir, 'icmp eq')
    assert.include(division.ir, '@llvm.trap()')
  }),
)

it.effect('matches the arithmetic IR golden and stays deterministic', () =>
  Effect.gen(function* () {
    const first = yield* emit(arithmeticSource, { mode: 'release' })
    const second = yield* emit(arithmeticSource, { mode: 'release' })

    assert.strictEqual(first.ir, golden('arithmetic.ll.txt'))
    assert.deepEqual(first.bitcode, second.bitcode)
  }),
)

it.effect('emits comparisons as icmp with zero-extension and branches natively', () =>
  Effect.gen(function* () {
    const artifact = yield* emit(
      'pub fn main() -> I32 { if I32.lessThan(1, 2) { return 42 } return 0 }',
      {
        mode: 'release',
      },
    )

    assert.include(artifact.ir, 'icmp slt')
    assert.include(artifact.ir, 'zext')
    assert.include(artifact.ir, 'br i1')
  }),
)

it.effect('realizes fixed arrays and checked mixed place reads from compiler-owned lanes', () =>
  Effect.gen(function* () {
    const source = `struct Pair { left: I32 right: I32 }
fn choose(values: Array<Pair, 2>, index: I32) -> I32 { return values[index].left }
pub fn main() -> I32 { return choose([Pair { left: 10, right: 11 }, Pair { left: 42, right: 43 }], 1) }`
    const first = yield* emit(source, { mode: 'release' })
    const second = yield* emit(source, { mode: 'release' })

    assert.include(first.ir, 'icmp ult')
    assert.include(first.ir, 'select i1')
    assert.include(first.ir, '@llvm.trap()')
    assert.deepEqual(first.bitcode, second.bitcode)
    assert.strictEqual(first.ir, second.ir)
  }),
)
