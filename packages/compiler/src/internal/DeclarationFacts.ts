/**
 * DeclarationFacts — immutable fact model for module-level declarations.
 *
 * Extracted from DeclarationIndex.ts per the split-compiler-declaration-index OpenSpec change.
 * The implementation stays in DeclarationIndex.ts to avoid circular imports; this module
 * re-exports the public fact model types.
 */

export type {
  DeclarationFact,
  StructFact,
  ServiceFact,
  InterfaceFact,
  RoleFact,
  EffectFact,
} from '../DeclarationIndex.js'
