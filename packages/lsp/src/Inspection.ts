/**
 * The language server's inspector surface: projects compiler inspector views for workspace
 * documents from the committed analysis, so editor clients can render compiler-phase views
 * without running the compiler themselves.
 *
 * The committed `ProjectAnalysis.View` is a frontend snapshot, and the compiler only accepts a
 * true single-root snapshot for runtime realization (layout, MIR, backend). So every request
 * realizes a single-root snapshot rooted at the requested document, rebuilt from the committed
 * closure's exact sources and cached on the committed view's identity — a new commit drops the
 * whole cache with the view it belonged to.
 */

import * as Analysis from '@silklang/compiler/Analysis'
import type { Fact } from '@silklang/compiler/InspectorRegistry'
import { viewById, views } from '@silklang/compiler/InspectorRegistry'
import type { RowModel } from '@silklang/compiler/InspectorRow'
import type * as ProjectAnalysis from '@silklang/compiler/ProjectAnalysis'
import * as SourceFile from '@silklang/compiler/SourceFile'
import * as SourceResolver from '@silklang/compiler/SourceResolver'
import * as Effect from 'effect/Effect'
import type * as ProjectSnapshot from './ProjectSnapshot.js'

/** The custom request an editor client sends to project one inspector view. */
export const viewRequest = 'silk/inspectorView'

/**
 * The custom request answering the registry of available views.
 *
 * The VS Code extension host is CommonJS and the compiler package is ESM-only, so the client
 * cannot import the registry itself; the server, which already depends on it, serves the list.
 */
export const viewsRequest = 'silk/inspectorViews'

/** One picker entry: the registry's `ViewDefinition` minus its projection function. */
export interface ViewDescriptor {
  readonly id: string
  readonly title: string
  readonly phase: string
  readonly tag: string
  readonly group: string
  readonly hasFilter?: boolean | undefined
}

export const descriptors: ReadonlyArray<ViewDescriptor> = views.map(
  ({ id, title, phase, tag, group, hasFilter }) => ({
    id,
    title,
    phase,
    tag,
    group,
    ...(hasFilter === undefined ? {} : { hasFilter }),
  }),
)

/** The notification announcing that a newer analysis committed for a document's project. */
export const invalidatedNotification = 'silk/inspectorInvalidated'

export interface ViewParameters {
  readonly uri: string
  readonly view: string
  readonly filter?: string | undefined
  readonly showTrivia?: boolean | undefined
}

export interface ViewResponse {
  readonly _tag: 'InspectorView'
  readonly rows: ReadonlyArray<RowModel>
  readonly meta?: string | undefined
  readonly facts?: ReadonlyArray<Fact> | undefined
  readonly unavailable?: string | undefined
  /** The document version the projection was computed from. */
  readonly version: number
  /** Module name → document URI, so a client can navigate module-qualified spans. */
  readonly moduleUris: Readonly<Record<string, string>>
}

export interface InvalidatedParameters {
  readonly workspace: string
  readonly uri: string
  readonly version: number
}

/** An unknown view id is an explicit answer, not an exception. */
export interface UnknownView {
  readonly _tag: 'UnknownView'
  readonly message: string
}

const decoder = new TextDecoder()

/**
 * Realized single-root snapshots, keyed by committed view identity and root module.
 *
 * The weak key is the invalidation strategy: the project worker replaces the view object on every
 * commit, so a stale cache entry becomes unreachable together with the view it described.
 */
const realizations = new WeakMap<ProjectAnalysis.View, Map<string, Analysis.Snapshot>>()

const realizedFor = (session: ProjectSnapshot.DocumentSnapshot): Analysis.Snapshot => {
  const byRoot = realizations.get(session.snapshot) ?? new Map<string, Analysis.Snapshot>()
  realizations.set(session.snapshot, byRoot)
  const root = session.document.module
  const cached = byRoot.get(root)
  if (cached !== undefined) return cached

  const sources = Analysis.sources(session.snapshot)
  const bytes = new Map(
    [...sources].map(([name, file]) => [name, Uint8Array.from(file.bytes)] as const),
  )
  const rootFile = sources.get(root) ?? SourceFile.make(root, session.document.bytes)
  const realized = Effect.runSync(
    Analysis.makeRealized({ root: rootFile }).pipe(Effect.provide(SourceResolver.memory(bytes))),
  )
  byRoot.set(root, realized)
  return realized
}

/** Projects one inspector view for a committed document analysis. */
export const project = (
  session: ProjectSnapshot.DocumentSnapshot,
  parameters: ViewParameters,
): ViewResponse | UnknownView => {
  const definition = viewById(parameters.view)
  if (definition === undefined) {
    return { _tag: 'UnknownView', message: `Unknown inspector view '${parameters.view}'` }
  }

  const snapshot = realizedFor(session)
  const modules = Object.fromEntries(
    [...Analysis.sources(session.snapshot)].map(
      ([name, file]) => [name, decoder.decode(Uint8Array.from(file.bytes))] as const,
    ),
  )
  const result = definition.project({
    snapshot,
    modules,
    root: session.document.module,
    mode: 'release',
    profile: 'release',
    filter: parameters.filter ?? '',
    showTrivia: parameters.showTrivia ?? false,
  })

  return {
    _tag: 'InspectorView',
    rows: result.rows,
    meta: result.meta,
    facts: result.facts,
    unavailable: result.unavailable,
    version: session.document.version,
    moduleUris: Object.fromEntries(session.moduleUris),
  }
}
