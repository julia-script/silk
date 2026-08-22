import type * as Instr from '@silk-effect/wasm/Instr'
import type * as CleanupPlan from './CleanupPlan.js'

export interface Child<State> {
  readonly cleanup: CleanupPlan.CleanupPlan
  readonly state: State
  readonly wrap?: (instructions: ReadonlyArray<Instr.Instr>) => ReadonlyArray<Instr.Instr>
}

export interface Step<State> {
  readonly before?: ReadonlyArray<Instr.Instr>
  readonly children?: ReadonlyArray<Child<State>>
  readonly after?: ReadonlyArray<Instr.Instr>
}

/**
 * The single recursive cleanup traversal used by hook and reclaim emission. Backends describe how
 * one plan node expands; this actor alone owns recursion, child ordering, and guarded branches.
 */
export const emitCleanupWalk = <State>(
  cleanup: CleanupPlan.CleanupPlan,
  state: State,
  expand: (cleanup: CleanupPlan.CleanupPlan, state: State) => Step<State>,
): ReadonlyArray<Instr.Instr> => {
  const step = expand(cleanup, state)
  return Object.freeze([
    ...(step.before ?? []),
    ...(step.children ?? []).flatMap((child) => {
      const instructions = emitCleanupWalk(child.cleanup, child.state, expand)
      return child.wrap?.(instructions) ?? instructions
    }),
    ...(step.after ?? []),
  ])
}

/** Preserves the cleanup contract: user Drop hooks run before owned storage is reclaimed. */
export const release = (
  hooks: ReadonlyArray<Instr.Instr>,
  reclaims: ReadonlyArray<Instr.Instr>,
): ReadonlyArray<Instr.Instr> => Object.freeze([...hooks, ...reclaims])
