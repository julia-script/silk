import * as Elaboration from '../../src/Elaboration.js'

type Records = ReturnType<typeof Elaboration.records>

/** The working records construction built a module from; tests inspect construction through them. */
export function records(self: Elaboration.Result): Records
export function records(self: Elaboration.Result | undefined): Records | undefined
export function records(self: Elaboration.Result | undefined): Records | undefined {
  return self === undefined ? undefined : Elaboration.records(self)
}
