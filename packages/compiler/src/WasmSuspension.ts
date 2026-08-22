import * as Backend from './Backend.js'
import * as Instances from './Instances.js'
import type * as Mir from './Mir.js'

export interface Records {
  readonly origins: ReadonlyArray<{
    readonly fn: Mir.MirFunction
    readonly region: Mir.SuspendEffectRegion
  }>
  readonly resumes: ReadonlyArray<{
    readonly fn: Mir.MirFunction
    readonly region: Mir.RunSuspendableEffectRegion
  }>
  readonly originIds: ReadonlyMap<string, number>
  readonly resumeIds: ReadonlyMap<string, number>
  readonly frames: ReadonlyMap<string, Mir.CoroutineFrameTargetLayout>
  readonly layouts: ReadonlyMap<string, Mir.CoroutineFrameTargetStateLayout>
}

/** Collects deterministic suspension records and their compact runtime identifiers. */
export const records = (program: Mir.Module): Records => {
  const origins = program.functions
    .flatMap((fn) =>
      (fn.suspension?.regions ?? []).flatMap((region) =>
        region._tag === 'SuspendEffectRegion' ? [Object.freeze({ fn, region })] : [],
      ),
    )
    .sort((left, right) =>
      Backend.suspensionPointKey(left.region.point).localeCompare(
        Backend.suspensionPointKey(right.region.point),
      ),
    )
  const resumes = program.functions
    .flatMap((fn) =>
      (fn.suspension?.regions ?? []).flatMap((region) =>
        region._tag === 'RunSuspendableEffectRegion' && region.relay.state !== undefined
          ? [Object.freeze({ fn, region })]
          : [],
      ),
    )
    .sort((left, right) =>
      Backend.suspensionPointKey(left.region.point).localeCompare(
        Backend.suspensionPointKey(right.region.point),
      ),
    )
  return Object.freeze({
    origins: Object.freeze(origins),
    resumes: Object.freeze(resumes),
    originIds: new Map(
      origins.map((record, ordinal) => [
        Backend.suspensionPointKey(record.region.point),
        ordinal + 1,
      ]),
    ),
    resumeIds: new Map(
      resumes.map((record, ordinal) => [
        Backend.suspensionPointKey(record.region.point),
        ordinal + 1,
      ]),
    ),
    frames: new Map(
      (program.coroutineFrames?.entries ?? []).map((entry) => [
        Instances.keyText(entry.function),
        entry,
      ]),
    ),
    layouts: new Map(
      (program.coroutineFrames?.entries ?? []).flatMap((entry) =>
        entry.states.map((state) => [Backend.suspensionPointKey(state.point), state] as const),
      ),
    ),
  })
}
