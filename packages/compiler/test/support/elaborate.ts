import * as Elaboration from '../../src/Elaboration.js'
import type * as ModuleClosure from '../../src/ModuleClosure.js'
import * as NameResolution from '../../src/NameResolution.js'
import * as Ownership from '../../src/Ownership.js'
import type * as SyntaxFile from '../../src/SyntaxFile.js'

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
  return Elaboration.elaborateModule({ syntax, headers, scope, index })
}

export const ownership = (result: Elaboration.Result): Ownership.ModuleOwnership => {
  const module = Object.freeze({
    _tag: 'Module' as const,
    name: result.syntax.source.id,
    syntax: result.syntax,
    imports: Object.freeze([]),
  })
  const closure: ModuleClosure.Closure = Object.freeze({
    _tag: 'ModuleClosure',
    rootModule: result.syntax.source.id,
    modules: Object.freeze([module]),
    cycles: Object.freeze([]),
    diagnostics: Object.freeze([]),
    sources: new Map([[result.syntax.source.id, result.syntax.source]]),
    resolutionFailures: Object.freeze([]),
  })
  return Ownership.checkModule(result, NameResolution.analyze(closure).index)
}
