import * as Effect from 'effect/Effect'
import * as AuthoredIdentity from '../../src/AuthoredIdentity.js'
import * as AuthoredLowering from '../../src/AuthoredLowering.js'
import * as Elaboration from '../../src/Elaboration.js'
import type * as DeclarationIndex from '../../src/DeclarationIndex.js'
import * as ModuleClosure from '../../src/ModuleClosure.js'
import * as NameResolution from '../../src/NameResolution.js'
import * as Ownership from '../../src/Ownership.js'
import type * as SyntaxFile from '../../src/SyntaxFile.js'

const indices = new WeakMap<Elaboration.Result, DeclarationIndex.Index>()

/** `fixture://semantic-accepted.silk` becomes `fixture/semantic-accepted.silk`. */
export const canonicalName = (sourceId: string): string => sourceId.replace(/:\/*/g, '/')

export const elaborate = (syntax: SyntaxFile.SyntaxFile): Elaboration.Result => {
  // Fixture ids such as `fixture://x.silk` are not canonical module names, so the closure module,
  // its authored owner and every declaration identity share one canonical name derived from the id.
  const name = canonicalName(syntax.source.id)
  const authored = Effect.runSync(
    AuthoredLowering.lower(syntax, AuthoredIdentity.module('memory', name)),
  )
  const module = Object.freeze({
    _tag: 'Module' as const,
    name,
    syntax,
    authored,
    declarations: ModuleClosure.selectedDeclarations(authored.module, new Map()),
    imports: Object.freeze([]),
  })
  const closure: ModuleClosure.Closure = Object.freeze({
    _tag: 'ModuleClosure',
    rootModule: name,
    modules: Object.freeze([module]),
    cycles: Object.freeze([]),
    diagnostics: Object.freeze([]),
    sources: new Map([[name, syntax.source]]),
    missingRoots: Object.freeze([]),
    resolutionFailures: Object.freeze([]),
  })
  const analyzed = NameResolution.analyze(closure)
  const index = analyzed.index
  const headers = index.modules.at(0)
  const scope = NameResolution.scopeOf(analyzed.resolution, name)
  if (headers === undefined || scope === undefined)
    throw new RangeError('Single-module elaboration fixture lost its module')
  const result = Elaboration.elaborateModule({ authored, headers, scope, index })
  indices.set(result, index)
  return result
}

export const ownership = (result: Elaboration.Result): Ownership.ModuleOwnership => {
  const index = indices.get(result)
  if (index === undefined)
    throw new RangeError('Ownership fixture requires its original elaboration result')
  return Ownership.checkModule(
    result,
    index,
    Ownership.localSharedAccessBoundaryPlan(
      new Map([[result.authored.presentation.sourceId, result]]),
    ),
  )
}
