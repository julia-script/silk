import type * as IncidentId from './IncidentId.js'
import type * as ProjectGeneration from './ProjectGeneration.js'
import type * as WorkerEpoch from './WorkerEpoch.js'

/** Exhaustive lifecycle of one exact project generation. */
export type GenerationState = Pending | Committed | Failed | Superseded | Closed

export interface Pending {
  readonly _tag: 'Pending'
  readonly generation: ProjectGeneration.ProjectGeneration
}

export interface Committed {
  readonly _tag: 'Committed'
  readonly generation: ProjectGeneration.ProjectGeneration
  readonly epoch: WorkerEpoch.WorkerEpoch
  readonly uris: ReadonlySet<string>
}

export interface Failed {
  readonly _tag: 'Failed'
  readonly generation: ProjectGeneration.ProjectGeneration
  readonly incident: IncidentId.IncidentId
}

export interface Superseded {
  readonly _tag: 'Superseded'
  readonly generation: ProjectGeneration.ProjectGeneration
}

export interface Closed {
  readonly _tag: 'Closed'
  readonly generation: ProjectGeneration.ProjectGeneration
}

export const pending = (generation: ProjectGeneration.ProjectGeneration): Pending =>
  Object.freeze({ _tag: 'Pending', generation })

export const committed = (
  generation: ProjectGeneration.ProjectGeneration,
  epoch: WorkerEpoch.WorkerEpoch,
  uris: Iterable<string>,
): Committed => Object.freeze({ _tag: 'Committed', generation, epoch, uris: new Set(uris) })

export const failed = (
  generation: ProjectGeneration.ProjectGeneration,
  incident: IncidentId.IncidentId,
): Failed => Object.freeze({ _tag: 'Failed', generation, incident })

export const superseded = (generation: ProjectGeneration.ProjectGeneration): Superseded =>
  Object.freeze({ _tag: 'Superseded', generation })

export const closed = (generation: ProjectGeneration.ProjectGeneration): Closed =>
  Object.freeze({ _tag: 'Closed', generation })
