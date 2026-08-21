/**
 * TypeInference — constraint-based generic type inference engine.
 *
 * Extracted from Type.ts per the split-compiler-type-system OpenSpec change.
 * The implementation stays in Type.ts to avoid circular imports; this module
 * re-exports the public inference entry points.
 */

export { infer, inferOpenGenericArguments, prefixSubstitution, substitution } from '../Type.js'
