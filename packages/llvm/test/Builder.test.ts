import * as Effect from 'effect/Effect'
import * as Layer from 'effect/Layer'
import * as ManagedRuntime from 'effect/ManagedRuntime'
import { expect, test } from 'vitest'
import * as Builder from '../src/Builder.js'
import * as IrText from '../src/IrText.js'

const TestRuntime = ManagedRuntime.make(Layer.empty)

test(
  'rejects a handle from another builder without changing either builder',
  Effect.fnUntraced(function* () {
    const first = yield* Builder.make()
    const second = yield* Builder.make()
    const handle = yield* Builder.allocateHandle(first)
    const error = yield* Effect.flip(Builder.validateHandle(second, handle))

    expect(error._tag).toBe('SilkError')
    expect(error.operation).toBe('Builder.validateHandle')
    expect(yield* IrText.render(first)).toBe('')
    expect(yield* IrText.render(second)).toBe('')
  }, TestRuntime.runPromise),
)

test(
  'serializes concurrent mutations without losing committed fragments',
  Effect.fnUntraced(function* () {
    const builder = yield* Builder.make()
    const fragments = Array.from({ length: 64 }, (_, index) => `asm-${index}`)
    yield* Effect.all(
      fragments.map((fragment) => Builder.appendModuleAssembly(builder, fragment)),
      { concurrency: 'unbounded' },
    )

    const rendered = yield* IrText.render(builder)
    for (const fragment of fragments) expect(rendered).toContain(`module asm "${fragment}"`)
    expect(rendered.match(/^module asm /gm)).toHaveLength(fragments.length)
  }, TestRuntime.runPromise),
)
