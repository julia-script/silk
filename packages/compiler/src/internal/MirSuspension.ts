/**
 * Suspension — target-neutral suspension control types shared by Mir and ProvisionalMir.
 *
 * Extracted from Mir.ts per the split-compiler-ir-evaluation OpenSpec change.
 * The implementation stays in Mir.ts to avoid circular imports; this module
 * re-exports the public Suspension data types.
 */

export type {
  SuspensionBorrowIdentity,
  SuspensionClassification,
  SuspensionCompletion,
  SuspensionControlEdge,
  SuspensionPointId,
  SuspensionProviderArgument,
  SuspensionRegion,
  SuspensionRunner,
} from '../Mir.js'
