import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Builder from '../src/Builder.js'
import * as IrText from '../src/IrText.js'
import { raise } from './support/raise.js'

it.effect('serializes concurrent mutations without losing committed fragments', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const fragments = Array.from({ length: 64 }, (_, index) => `asm-${index}`)
    yield* Effect.forEach(
      fragments,
      (fragment) => Builder.appendModuleAssembly(builder, fragment),
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
