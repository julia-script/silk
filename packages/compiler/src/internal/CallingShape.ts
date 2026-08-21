/**
 * CallingShape — ABI lane shapes and selectors for compile-time layout assignment.
 *
 * Extracted from Layout.ts per the split-compiler-layout OpenSpec change.
 * The implementation stays in Layout.ts to avoid circular imports; this module
 * re-exports the public CallingShape types.
 */

export type { CallingShape, CallingScalar, CallingLane, CallingShapeSelector } from '../Layout.js'
