import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
const localSchedulerTimerBasic = readFileSync(
  new URL('./fixtures/scheduler-fiber/local-scheduler-timer-basic.silk', import.meta.url),
)
const localSchedulerTimerBasicText = localSchedulerTimerBasic.toString('utf8')
const invalidTimerChildRequirementSource = localSchedulerTimerBasicText
  .replace('struct ParentClock {', 'service Extra {}\n\nstruct ParentClock {')
  .replace(
    '? &mut Scheduler | &mut MonotonicClock {\n  run MonotonicClock.waitFor(1)',
    '? &mut Scheduler | &mut MonotonicClock | &mut Extra {\n  run MonotonicClock.waitFor(1)',
  )
const missingParentClockSource = localSchedulerTimerBasicText.replace(
  `  let mut clock = ParentClock { mark: SystemClock.make(0, 0) }
  let program = LocalScheduler.execute(&mut scheduler, root())
    |> Effect.provideMut<MonotonicClock>(&mut clock)
  return run move program`,
  '  return run LocalScheduler.execute(&mut scheduler, root())',
)

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

it.effect(
  'enforces child and parent clock requirement-row boundaries',
  () =>
    Effect.gen(function* () {
      const invalidChild = yield* Analysis.ofSource(
        'scheduler-fiber/invalid-timer-child-requirement',
        ascii(invalidTimerChildRequirementSource),
      )
      assert.deepEqual(
        Analysis.diagnostics(invalidChild).map((diagnostic) => diagnostic.code),
        ['SEM0012'],
      )

      const missingParentClock = yield* Analysis.ofSource(
        'scheduler-fiber/missing-parent-clock',
        ascii(missingParentClockSource),
      )
      assert.deepEqual(
        Analysis.diagnostics(missingParentClock).map((diagnostic) => diagnostic.code),
        ['SEM0071'],
      )
    }),
  { timeout: 120_000 },
)
