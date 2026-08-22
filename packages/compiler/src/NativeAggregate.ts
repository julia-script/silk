import * as CleanupPlan from './CleanupPlan.js'
import type * as DeclarationIndex from './DeclarationIndex.js'

/** Field paths to reclaim contexts, or undefined when guarded structural cleanup is required. */
export const reclaimContextPaths = (
  plan: CleanupPlan.CleanupPlan,
  prefix: ReadonlyArray<DeclarationIndex.FieldId> = [],
): ReadonlyArray<ReadonlyArray<DeclarationIndex.FieldId>> | undefined => {
  if (!CleanupPlan.reclaims(plan)) return CleanupPlan.hasHook(plan) ? undefined : []
  switch (plan._tag) {
    case 'NoCleanup':
    case 'ParameterCleanup':
      return []
    case 'AllocationCleanup':
    case 'RawBufferCleanup':
      return [prefix]
    case 'StructCleanup': {
      const collected: Array<ReadonlyArray<DeclarationIndex.FieldId>> = []
      for (const field of plan.fields) {
        const nested = reclaimContextPaths(field.cleanup, [...prefix, field.field])
        if (nested === undefined) return undefined
        collected.push(...nested)
      }
      return collected
    }
    default:
      return undefined
  }
}
