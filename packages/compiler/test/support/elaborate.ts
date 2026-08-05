import * as DeclarationIndex from '../../src/DeclarationIndex.js'
import * as Elaboration from '../../src/Elaboration.js'
import type * as ModuleClosure from '../../src/ModuleClosure.js'
import * as NameResolution from '../../src/NameResolution.js'
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
  })
  const index = DeclarationIndex.collect(closure)
  const headers = index.modules.at(0)
  const scope = NameResolution.scopeOf(NameResolution.resolve(closure, index), syntax.source.id)
  if (headers === undefined || scope === undefined)
    throw new RangeError('Single-module elaboration fixture lost its module')
  return Elaboration.elaborateModule({ syntax, headers, scope, index })
}
