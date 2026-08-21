/**
 * BackendShared — cross-backend contract shared by LlvmBackend and WasmBackend.
 *
 * Extracted from Backend.ts per the split-compiler-backends OpenSpec change.
 * The implementation stays in Backend.ts to avoid circular imports; this module
 * re-exports the public shared contract types.
 */

export type {
  Artifact,
  Backend,
  CodegenRequest,
  ControlProvenance,
} from '../Backend.js'
export {
  BackendError,
  formatModuleViolations,
  suspensionPointKey,
  symbolFor,
  terminationOf,
} from '../Backend.js'
