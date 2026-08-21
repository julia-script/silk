/**
 * CleanupPlan — deterministic target-neutral cleanup plans for ownership-checked programs.
 *
 * Extracted from Ownership.ts per the split-compiler-mid-end OpenSpec change.
 * The implementation stays in Ownership.ts to avoid circular imports; this module
 * re-exports the public CleanupPlan types and helpers.
 */

export type {
  CleanupPlan,
  ExitPlan,
  LoopFixedPoint,
  Verdict,
  FunctionOwnership,
} from '../Ownership.js'

export { cleanupHasHook, cleanupReclaims, cleanupHasEffect } from '../Ownership.js'
