import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'

class LifecycleFixtureError extends Data.TaggedError('LifecycleFixtureError') {}

/** Produces the same checked C entry and callback instrumentation for native and Wasm probes. */
export const expose = Effect.fnUntraced(
  /** @param {string} source @param {{ failure: boolean, reentrant: boolean }} options */
  function* (source, options) {
    if (options.failure) {
      const original = 'effect fn firstBody() -> i32 { run Execution.park(register) return 20 }'
      if (!source.includes(original))
        return yield* new LifecycleFixtureError({ message: 'Missing suspended failure body' })
      source = source.replace(
        original,
        `struct Failure {}
effect fn failingBody() -> i32 ! Failure { run Execution.park(register) fail Failure {} }
effect fn recoveredFailure(error: Failure) -> i32 { return 20 }
effect fn firstBody() -> i32 { return run Effect.catchAll(failingBody(), recoveredFailure) }`,
      )
    }
    if (!source.includes('pub fn main() -> i32'))
      return yield* new LifecycleFixtureError({ message: 'Missing lifecycle entry' })
    source = source.replace('pub fn main() -> i32', 'export "C" fn storage_lifecycle() -> i32')
    if (options.reentrant) {
      const callbacks = /(fn (?:register|complete|suspend|ready)\([^\n]+?\) -> [^\n{]+\{)/g
      if ([...source.matchAll(callbacks)].length !== 4)
        return yield* new LifecycleFixtureError({ message: 'Missing reentrant lifecycle callback' })
      source =
        'unsafe extern "C" fn storage_reenter() -> ()\n' +
        source.replace(callbacks, '$1 unsafe storage_reenter() ')
    }
    return source
  },
)
