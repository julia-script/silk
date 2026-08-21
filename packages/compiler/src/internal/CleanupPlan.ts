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
  FunctionOwnership,
  LoopFixedPoint,
  Verdict,
} from '../Ownership.js'

export { cleanupHasEffect, cleanupHasHook, cleanupReclaims } from '../Ownership.js'
