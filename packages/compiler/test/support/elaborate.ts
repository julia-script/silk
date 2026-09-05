import * as Elaboration from '../../src/Elaboration.js'
import type * as DeclarationIndex from '../../src/DeclarationIndex.js'
import type * as ModuleClosure from '../../src/ModuleClosure.js'
import * as NameResolution from '../../src/NameResolution.js'
import * as Ownership from '../../src/Ownership.js'
import type * as SyntaxFile from '../../src/SyntaxFile.js'

const indices = new WeakMap<Elaboration.Result, DeclarationIndex.Index>()

export const elaborate = (syntax: SyntaxFile.SyntaxFile): Elaboration.Result => {
  const module = Object.freeze({
    _tag: 'Module' as const,
    name: syntax.source.id,
    syntax,
    imports: Object.freeze([]),
  })
  const closure: ModuleClosure.Closure = Object.freeze({
    _tag: 'ModuleClosure',
    rootModule: syntax.source.id,
    modules: Object.freeze([module]),
    cycles: Object.freeze([]),
    diagnostics: Object.freeze([]),
    sources: new Map([[syntax.source.id, syntax.source]]),
    resolutionFailures: Object.freeze([]),
  })
  const analyzed = NameResolution.analyze(closure)
  const index = analyzed.index
  const headers = index.modules.at(0)
  const scope = NameResolution.scopeOf(analyzed.resolution, syntax.source.id)
  if (headers === undefined || scope === undefined)
    throw new RangeError('Single-module elaboration fixture lost its module')
  const result = Elaboration.elaborateModule({ syntax, headers, scope, index })
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
    Ownership.localSharedAccessBoundaryPlan(new Map([[result.syntax.source.id, result]])),
  )
}
