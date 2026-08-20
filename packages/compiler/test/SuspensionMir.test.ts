import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Mir from '../src/Mir.js'
import * as SuspensionMir from '../src/SuspensionMir.js'

const encoder = new TextEncoder()

const source = `import silk.effects as Effect
effect fn delayed() -> i32 {
  return run Effect.suspend(effect { return 2 })
}
effect fn program() -> i32 {
  return 40 + run delayed()
}
pub fn main() -> i32 { return run program() }`

const snapshot = (input = source) =>
  Analysis.ofSourceRealized('suspension-mir/main', encoder.encode(input), 'wasm32-unknown-unknown')

const suspensionRegions = (program: Mir.Module): ReadonlyArray<Mir.SuspensionRegion> =>
  program.functions.flatMap((fn) => fn.suspension?.regions ?? [])

const stateful = (
  program: Mir.Module,
): readonly [
  Mir.MirFunction,
  Extract<Mir.SuspensionRegion, { _tag: 'RunSuspendableEffectRegion' }>,
] => {
  for (const fn of program.functions)
    for (const region of fn.suspension?.regions ?? [])
      if (
        fn.id.name.startsWith('program$effect$') &&
        region._tag === 'RunSuspendableEffectRegion' &&
        region.relay.state !== undefined
      )
        return [fn, region]
  throw new RangeError(SuspensionMir.summary(program))
}

const replaceSuspensionRegion = (
  program: Mir.Module,
  owner: Mir.MirFunction,
  previous: Mir.SuspensionRegion,
  next: Mir.SuspensionRegion,
): Mir.Module =>
  Object.freeze({
    ...program,
    functions: Object.freeze(
      program.functions.map((fn) =>
        fn !== owner || fn.suspension === undefined
          ? fn
          : Object.freeze({
              ...fn,
              suspension: Object.freeze({
                ...fn.suspension,
                regions: Object.freeze(
                  fn.suspension.regions.map((region) => (region === previous ? next : region)),
                ),
              }),
            }),
      ),
    ),
  })

const hasRule = (program: Mir.Module, rule: Mir.Violation['rule']): boolean =>
  Mir.verify(program).some((violation) => violation.rule === rule)

it.effect(
  'finalizes deterministic target-neutral origin, relay, resume, and logical layout facts',
  () =>
    Effect.gen(function* () {
      const first = yield* snapshot()
      const second = yield* snapshot()
      assert.deepEqual(Analysis.diagnostics(first), [])
      const left = Analysis.loweredMir(first)
      const right = Analysis.loweredMir(second)
      assert.deepEqual(Mir.verify(left), [], SuspensionMir.summary(left))
      assert.strictEqual(Mir.encode(left), Mir.encode(right))
      assert.strictEqual(SuspensionMir.summary(left), SuspensionMir.summary(right))
      const regions = suspensionRegions(left)
      assert.lengthOf(
        regions.filter((region) => region._tag === 'SuspendEffectRegion'),
        1,
      )
      assert.isAtLeast(
        regions.filter((region) => region._tag === 'RunSuspendableEffectRegion').length,
        1,
      )
      const [, relay] = stateful(left)
      assert.deepEqual(relay.relay.preserves, ['Child', 'Origin', 'TypedOutcome'])
      assert.strictEqual(relay.complete._tag, 'CompleteInCurrentActivation')
      assert.strictEqual(relay.relay.frame, 'StatefulRelay')
      assert.isDefined(relay.relay.state)
      assert.deepEqual(
        relay.relay.state?.slots.map((slot) => slot.local.ordinal),
        relay.liveLocals.map((local) => local.ordinal),
      )
      assert.deepEqual(
        Mir.suspensionControlEdges(
          left.functions.find((fn) => fn === stateful(left)[0]) ?? stateful(left)[0],
        )
          .filter(
            (edge) =>
              edge.from.sourceId === relay.point.sourceId &&
              edge.from.spanStart === relay.point.spanStart &&
              edge.from.ordinal === relay.point.ordinal,
          )
          .map((edge) => edge.kind),
        ['RelayTransfer', 'ResumeSuccess', 'ResumeFailure'],
      )
    }),
)

it.effect('keeps final synchronous MIR free of suspension machinery', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(
      'effect fn seed(value: i32) -> i32 { return value } pub fn main() -> i32 { return run seed(42) }',
    )
    const program = Analysis.loweredMir(self)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.deepEqual(Mir.verify(program), [])
    assert.deepEqual(suspensionRegions(program), [])
    assert.notInclude(Mir.encode(program), 'suspension-classification')
  }),
)

it.effect(
  'rejects malformed logical layouts, resumes, runner contracts, and orphan machinery',
  () =>
    Effect.gen(function* () {
      const self = yield* snapshot()
      const program = Analysis.loweredMir(self)
      const [owner, relay] = stateful(program)
      const descriptor = relay.relay.state
      assert.isDefined(descriptor)
      if (descriptor === undefined) return

      const omitted = Object.freeze({
        ...relay,
        relay: Object.freeze({
          ...relay.relay,
          state: Object.freeze({
            ...descriptor,
            slots: Object.freeze(descriptor.slots.slice(1)),
          }),
        }),
      })
      assert.isTrue(
        hasRule(replaceSuspensionRegion(program, owner, relay, omitted), 'InvalidCoroutineFrame'),
      )

      const firstSlot = descriptor.slots.at(0)
      if (firstSlot !== undefined) {
        const incompatibleAccess = Object.freeze({
          ...relay,
          relay: Object.freeze({
            ...relay.relay,
            state: Object.freeze({
              ...descriptor,
              slots: Object.freeze([
                Object.freeze({
                  ...firstSlot,
                  type: Object.freeze({
                    _tag: firstSlot.type._tag === 'bool' ? 'i32' : 'bool',
                  }),
                }),
                ...descriptor.slots.slice(1),
              ]),
            }),
          }),
        })
        assert.isTrue(
          hasRule(
            replaceSuspensionRegion(program, owner, relay, incompatibleAccess),
            'InvalidCoroutineFrame',
          ),
        )
      }

      const duplicate = Object.freeze({
        ...relay,
        liveLocals: Object.freeze([...relay.liveLocals, ...relay.liveLocals.slice(0, 1)]),
      })
      assert.isTrue(
        hasRule(replaceSuspensionRegion(program, owner, relay, duplicate), 'InvalidCoroutineFrame'),
      )

      const undeclared = Object.freeze({
        ...relay,
        liveLocals: Object.freeze([
          ...relay.liveLocals,
          Object.freeze({ _tag: 'Local' as const, ordinal: owner.localTypes.length }),
        ]),
      })
      assert.isTrue(
        hasRule(
          replaceSuspensionRegion(program, owner, relay, undeclared),
          'InvalidCoroutineFrame',
        ),
      )

      const badResume = Object.freeze({
        ...relay,
        relay: Object.freeze({
          ...relay.relay,
          state: Object.freeze({
            ...descriptor,
            success: Object.freeze({
              ...descriptor.success,
              resume: Object.freeze({ ...descriptor.success.resume, path: 'Failure' as const }),
            }),
          }),
        }),
      })
      assert.isTrue(
        hasRule(replaceSuspensionRegion(program, owner, relay, badResume), 'InvalidCoroutineFrame'),
      )

      const badRunner = Object.freeze({
        ...relay,
        runner: Object.freeze({ ...relay.runner, classification: 'Synchronous' as const }),
      })
      assert.isTrue(
        hasRule(replaceSuspensionRegion(program, owner, relay, badRunner), 'InvalidSuspension'),
      )

      const badOutcome = Object.freeze({
        ...relay,
        runner: Object.freeze({
          ...relay.runner,
          outcome: Object.freeze({ ...relay.runner.outcome, success: 'bool' as const }),
        }),
      })
      assert.isTrue(
        hasRule(replaceSuspensionRegion(program, owner, relay, badOutcome), 'InvalidSuspension'),
      )

      const differentOwner = program.functions.find((fn) => fn !== owner)
      assert.isDefined(differentOwner)
      if (differentOwner !== undefined) {
        const badIdentity = Object.freeze({
          ...relay,
          point: Object.freeze({ ...relay.point, owner: differentOwner.instance }),
        })
        assert.isTrue(
          hasRule(replaceSuspensionRegion(program, owner, relay, badIdentity), 'InvalidSuspension'),
        )
      }

      const noDescriptor = Object.freeze({
        ...relay,
        relay: Object.freeze({
          _tag: 'RelayExistingTransfer' as const,
          preserves: relay.relay.preserves,
          frame: 'StatefulRelay' as const,
        }),
      })
      assert.isTrue(
        hasRule(
          replaceSuspensionRegion(program, owner, relay, noDescriptor),
          'InvalidCoroutineFrame',
        ),
      )

      const orphan: Mir.Module = Object.freeze({
        ...program,
        functions: Object.freeze(
          program.functions.map((fn) =>
            fn.suspension === undefined
              ? fn
              : Object.freeze({
                  ...fn,
                  suspension: Object.freeze({
                    ...fn.suspension,
                    regions: Object.freeze(
                      fn.suspension.regions.filter(
                        (region) => region._tag !== 'SuspendEffectRegion',
                      ),
                    ),
                  }),
                }),
          ),
        ),
      })
      assert.isTrue(hasRule(orphan, 'OrphanSuspensionMachinery'))
      assert.notInclude(Mir.encode(program), 'provisional-mir')
    }),
)
