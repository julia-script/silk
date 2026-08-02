import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Builder from '../src/Builder.js'
import * as IrText from '../src/IrText.js'
import { raise } from './support/raise.js'

it.effect('rejects a handle from another builder without changing either builder', () =>
  Effect.gen(function* () {
    const first = yield* Builder.make()
    const second = yield* Builder.make()
    const handle = yield* Builder.allocateHandle(first)
    const error = yield* Effect.flip(Builder.validateHandle(second, handle))

    assert.strictEqual(error._tag, 'LlvmError')
    assert.strictEqual(error.operation, 'Builder.validateHandle')
    assert.strictEqual(yield* IrText.render(first), '')
    assert.strictEqual(yield* IrText.render(second), '')
  }),
)

it.effect('serializes concurrent mutations without losing committed fragments', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const fragments = Array.from({ length: 64 }, (_, index) => `asm-${index}`)
    yield* Effect.all(
      fragments.map((fragment) => Builder.appendModuleAssembly(builder, fragment)),
      { concurrency: 'unbounded' },
    )

    const rendered = yield* IrText.render(builder)
    for (const fragment of fragments) assert.include(rendered, `module asm "${fragment}"`)
    assert.lengthOf(
      rendered.match(/^module asm /gm) ?? raise('expected assembly fragments'),
      fragments.length,
    )
  }),
)
