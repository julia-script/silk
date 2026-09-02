import * as Option from 'effect/Option'
import * as ImportPlan from './ImportPlan.js'
import type * as ModuleSummary from './ModuleSummary.js'
import * as NameResolution from './NameResolution.js'
import * as SemanticOccurrence from './SemanticOccurrence.js'
import * as SourceAction from './SourceAction.js'
import * as SourceFile from './SourceFile.js'
import type * as SyntaxFile from './SyntaxFile.js'
import * as WorkspaceInventory from './WorkspaceInventory.js'

/** The compiler facts needed to validate candidates at one unresolved occurrence. */
export interface Snapshot {
  readonly index: Parameters<typeof NameResolution.lookup>[1]
  readonly resolution: NameResolution.Resolution
  readonly semanticOccurrences: SemanticOccurrence.Index
  readonly results: ReadonlyMap<string, { readonly syntax: SyntaxFile.SyntaxFile }>
}

/** Serializable compiler-owned identity of one candidate choice. */
export interface CandidateKey {
  readonly _tag: 'AutoImportCandidateKey'
  readonly module: string
  readonly spelling: string
  readonly declarationKind: ModuleSummary.DeclarationKind
  readonly ordinal: number
}

/** One discovered auto-import action and the candidate needed to resolve it. */
export interface Action {
  readonly _tag: 'AutoImportAction'
  readonly descriptor: SourceAction.Descriptor
  readonly candidate: CandidateKey
  readonly tier: WorkspaceInventory.Tier
}

export interface Request {
  readonly snapshot: Snapshot
  readonly inventory: WorkspaceInventory.WorkspaceInventory
  readonly module: string
  readonly byteOffset: number
}

const applicableNamespace = (
  role: SemanticOccurrence.Role,
  namespace: ModuleSummary.Namespace,
): boolean => {
  switch (role) {
    case 'Value':
      return namespace === 'Value' || namespace === 'ValueAndType'
    case 'Type':
    case 'Actor':
      return namespace === 'Type' || namespace === 'ValueAndType'
    default:
      return false
  }
}

const candidateKey = (candidate: WorkspaceInventory.Candidate): CandidateKey =>
  Object.freeze({
    _tag: 'AutoImportCandidateKey',
    module: candidate.module,
    spelling: candidate.declaration.spelling,
    declarationKind: candidate.declaration.declarationKind,
    ordinal: candidate.declaration.ordinal,
  })

export const key = (candidate: CandidateKey): string =>
  `auto-import:${candidate.module}:${candidate.declarationKind}:${candidate.ordinal}:${candidate.spelling}`

const alreadyImports = (
  inventory: WorkspaceInventory.WorkspaceInventory,
  module: string,
  imported: string,
): boolean => WorkspaceInventory.summary(inventory, module)?.imports.includes(imported) ?? false

/** Discovers deterministic exact-name candidates for one unresolved semantic occurrence. */
export const discover = (request: Request): ReadonlyArray<Action> => {
  const occurrence = SemanticOccurrence.at(
    request.snapshot.semanticOccurrences,
    request.module,
    request.byteOffset,
  )
  if (occurrence?.resolution._tag !== 'Missing') return Object.freeze([])
  const syntax = request.snapshot.results.get(request.module)?.syntax
  if (syntax === undefined) return Object.freeze([])
  const spelling = Option.getOrUndefined(SourceFile.spelling(syntax.source, occurrence.span))
  if (spelling === undefined) return Object.freeze([])
  const scope = NameResolution.scopeOf(request.snapshot.resolution, request.module)
  if (
    scope === undefined ||
    NameResolution.lookup(scope, request.snapshot.index, spelling)._tag !== 'Missing'
  )
    return Object.freeze([])
  const candidates = WorkspaceInventory.candidates(request.inventory, spelling)
    .filter(
      (candidate) =>
        candidate.module !== request.module &&
        applicableNamespace(occurrence.role, candidate.declaration.namespace),
    )
    .sort((left, right) => {
      const importOrder =
        Number(alreadyImports(request.inventory, request.module, right.module)) -
        Number(alreadyImports(request.inventory, request.module, left.module))
      if (importOrder !== 0) return importOrder
      if (left.tier !== right.tier) return left.tier === 'Project' ? -1 : 1
      return (
        left.module.localeCompare(right.module) ||
        left.declaration.ordinal - right.declaration.ordinal
      )
    })
  return Object.freeze(
    candidates.map((candidate): Action => {
      const selected = candidateKey(candidate)
      return Object.freeze({
        _tag: 'AutoImportAction',
        descriptor: SourceAction.descriptor({
          key: key(selected),
          title: `Import ${spelling} from ${candidate.module}`,
          kind: 'QuickFix',
          target: occurrence.span,
        }),
        candidate: selected,
        tier: candidate.tier,
      })
    }),
  )
}

export interface ResolveRequest extends Request {
  readonly candidate: CandidateKey
}

const sameCandidate = (left: CandidateKey, right: CandidateKey): boolean =>
  left.module === right.module &&
  left.spelling === right.spelling &&
  left.declarationKind === right.declarationKind &&
  left.ordinal === right.ordinal

/** Revalidates one selected candidate, then constructs its syntax-owned import plan. */
export const resolve = (request: ResolveRequest): Option.Option<SourceAction.ChangePlan> => {
  const applicable = discover(request).find((action) =>
    sameCandidate(action.candidate, request.candidate),
  )
  const syntax = request.snapshot.results.get(request.module)?.syntax
  if (applicable === undefined || syntax === undefined) return Option.none()
  return ImportPlan.make({
    syntax,
    module: request.candidate.module,
    spelling: request.candidate.spelling,
  })
}
