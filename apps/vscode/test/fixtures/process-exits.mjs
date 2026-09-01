import * as Effect from 'effect/Effect'

void Effect.runPromise(Effect.sleep(10)).then(() => process.exit(7))
