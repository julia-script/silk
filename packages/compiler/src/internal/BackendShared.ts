/**
 * BackendShared — cross-backend contract shared by LlvmBackend and WasmBackend.
 *
 * Extracted from Backend.ts per the split-compiler-backends OpenSpec change.
 * The implementation stays in Backend.ts to avoid circular imports; this module
 * re-exports the public shared contract types.
 */

export { BackendError, formatModuleViolations, terminationOf, suspensionPointKey, symbolFor } from '../Backend.js'
export type {
  Backend,
  Artifact,
  CodegenRequest,
  ControlProvenance,
} from '../Backend.js'
