import type * as ProjectAnalysis from '@silklang/compiler/ProjectAnalysis'
import type * as WorkspaceInventory from '@silklang/compiler/WorkspaceInventory'
import type * as Document from './Document.js'

/** One document projection owned by a complete immutable project commit. */
export interface DocumentSnapshot {
  readonly document: Document.Document
  readonly project: ProjectAnalysis.ProjectAnalysis
  readonly snapshot: ProjectAnalysis.View
  readonly moduleUris: ReadonlyMap<string, string>
  readonly inventory: WorkspaceInventory.WorkspaceInventory
}
