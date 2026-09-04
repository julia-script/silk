import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as RowAlgebra from '../src/RowAlgebra.js'
import * as Type from '../src/Type.js'

const encoder = new TextEncoder()

const snapshot = (source: string, target = 'aarch64-apple-darwin') =>
  Analysis.ofSourceRealized('logging/main', encoder.encode(source), target)

it.effect('keeps missing providers and invalid message inputs explicit', () =>
  Effect.gen(function* () {
    const missing = yield* snapshot(`import silk.effect { Effect }
import silk.logger { LogError }
pub effect fn main() -> () ! LogError {
  return run Effect.log("missing")
}`)
    assert.include(
      Analysis.diagnostics(missing).map((diagnostic) => diagnostic.code),
      'SEM0071',
    )

    const invalidMessage = yield* snapshot(`import silk.effect { Effect }
pub fn main() -> i32 {
  let effect = Effect.log(42)
  return 0
}`)
    assert.isAbove(Analysis.diagnostics(invalidMessage).length, 0)

    const invalidLevel = yield* snapshot(`import silk.effect { Effect }
pub fn main() -> i32 {
  let effect = Effect.logAt(42, "message")
  return 0
}`)
    assert.isAbove(Analysis.diagnostics(invalidLevel).length, 0)
  }),
)

it.effect('forwards provider-selection evidence only from an exact enclosing constraint', () =>
  Effect.gen(function* () {
    const wrapper = (constraint: string) => `import silk.logger { StdoutLogger }
import silk.effect { Effect }
import silk.logger { Logger, LogError }

effect fn bind<?S, A, P, E, ?R>(
  self: once Effect<A ! E ? R>,
  provider: &mut P
) -> A ! E ? Without<R, S>
${constraint} {
  return run Intrinsic.bindRequirementMut<S>(move self, provider)
}

effect fn read() -> () ! LogError ? &mut Logger {
  run Effect.log("Reading")
}

pub effect fn main() -> () ! LogError {
  let mut logger = Logger.stdoutProvider()
  return run bind(read(), &mut logger)
}`

    const constrained = yield* snapshot(wrapper('where &mut P provides S from R'))
    assert.deepEqual(Analysis.diagnostics(constrained), [])
    const bind = Analysis.instancesOf(constrained).instances.find(
      (instance) => instance.key.declaration.name === 'bind',
    )
    assert.isDefined(bind)
    if (bind !== undefined) {
      assert.strictEqual(
        RowAlgebra.concretize(
          Type.requirementRowPolicy(),
          bind.specialization.requirementRow ??
            RowAlgebra.concrete(Type.requirementRowPolicy(), []),
        )._tag,
        'Concrete',
      )
      assert.isTrue(bind.specialization.evidence.length > 0)
      assert.include(
        bind.specialization.evidence.map((proof) => proof._tag),
        'RequirementSelection',
      )
    }

    const unconstrained = yield* snapshot(wrapper(''))
    assert.isAbove(Analysis.diagnostics(unconstrained).length, 0)
  }),
)

it.effect('rejects a callable relay whose leading binding has observable work', () =>
  Effect.gen(function* () {
    const frontend = yield* snapshot(`import silk.logger { InMemoryLogger }
import silk.effect { Effect }
import silk.logger { Logger }

fn observeThenForward<F>(value: F) -> F {
  let boom = 1 / 0
  return move value
}

effect fn read() -> i32 ? &mut Logger { return 42 }

pub fn main() -> i32 {
  let mut logger = Logger.inMemoryProvider()
  let bind = observeThenForward(Effect.provideMut<Logger>(&mut logger))
  return run bind(read())
}`)
    assert.include(
      Analysis.diagnostics(frontend).map((diagnostic) => diagnostic.code),
      'SEM0122',
    )
    assert.strictEqual(frontend.mir._tag, 'Unavailable')
  }),
)

it.effect(
  'rejects constrained provider sections at aggregate generic and indirect boundaries',
  () =>
    Effect.gen(function* () {
      const cases = [
        `import silk.effect { Effect }
import silk.logger { InMemoryLogger }
import silk.logger { Logger }
fn store<F>(value: F) -> [F; 1] {
  return [move value]
}
pub fn main() -> i32 {
  let mut logger = Logger.inMemoryProvider()
  let escaped = store(Effect.provideMut<Logger>(&mut logger))
  return 42
}`,
        `import silk.effect { Effect }
import silk.logger { InMemoryLogger }
import silk.logger { Logger }
fn consume<F>(value: F) -> () { return () }
pub fn main() -> i32 {
  let mut logger = Logger.inMemoryProvider()
  let consumed = consume(Effect.provideMut<Logger>(&mut logger))
  return 42
}`,
        `import silk.effect { Effect }
import silk.logger { InMemoryLogger }
import silk.logger { Logger }
fn invoke<A, E, ?R, F: fn(once Effect<A ! E ? R>) -> Effect<A ! E>>(operation: F, value: once Effect<A ! E ? R>) -> Effect<A ! E> {
  return operation(move value)
}
pub fn main() -> i32 {
  let mut logger = Logger.inMemoryProvider()
  let operation = invoke(Effect.provideMut<Logger>(&mut logger), Effect.log("indirect"))
  return 42
}`,
        `import silk.effect { Effect }
import silk.logger { InMemoryLogger }
import silk.logger { Logger }
union Store<F> { Empty, Stored { value: F } }
pub fn main() -> i32 {
  let mut logger = Logger.inMemoryProvider()
  let escaped = Store.Stored { value: Effect.provideMut<Logger>(&mut logger) }
  return 42
}`,
      ]
      for (const [ordinal, body] of cases.entries()) {
        const frontend = yield* snapshot(`import silk.effect { Effect }
import silk.logger { Logger, LogError }
${body}`)
        assert.include(
          Analysis.diagnostics(frontend).map((diagnostic) => diagnostic.code),
          'SEM0122',
          `case ${ordinal}`,
        )
        assert.strictEqual(frontend.mir._tag, 'Unavailable')
      }
    }),
)
