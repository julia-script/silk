import type * as AuthoredLowering from './AuthoredLowering.js'
import * as BodyQuery from './BodyQuery.js'
import * as CompilerTrace from './CompilerTrace.js'
import type * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Elaboration from './Elaboration.js'
import type * as ExpressionAnalysis from './ExpressionAnalysis.js'
import type * as NameResolution from './NameResolution.js'
import * as SemanticContext from './SemanticContext.js'
import * as StatementAnalysis from './StatementAnalysis.js'

/** Inputs shared by fresh and reusable declaration-body checking. */
export interface BodyInput {
  readonly query?: BodyQuery.BodyQuery
  readonly authored: AuthoredLowering.Lowered
  readonly headers: DeclarationFacts.ModuleHeaders
  readonly scope: NameResolution.ModuleScope
  readonly index: DeclarationIndex.Index
  readonly declaration: DeclarationFacts.DeclarationFact
  readonly trace?: CompilerTrace.CompilerTrace
}

/** Checks or reuses one declaration body and returns its complete checked unit. */
export const checkBody = (input: BodyInput): Elaboration.CheckedUnit => {
  const trace = input.trace ?? CompilerTrace.none
  const context = SemanticContext.make(input.authored)
  const build = (): BodyQuery.Built => {
    const hiddenFunctions: Array<ExpressionAnalysis.FunctionAnalysis> = []
    const analysis = StatementAnalysis.analyzeFunctionBody(
      context,
      input.declaration,
      input.headers.declarations,
      Object.freeze({ scope: input.scope, index: input.index, hiddenFunctions }),
    )
    const own = Elaboration.checkedBody(
      context,
      input.index,
      analysis.fact,
      undefined,
      undefined,
      analysis.builder,
    )
    return {
      unit: Object.freeze({
        bodies: Object.freeze([
          own,
          ...hiddenFunctions.map((hidden) =>
            Elaboration.checkedBody(
              context,
              input.index,
              hidden.fact,
              own.artifact,
              undefined,
              hidden.builder,
            ),
          ),
        ]),
        diagnostics: analysis.diagnostics,
      }),
    }
  }
  return trace(
    'Semantic.checkBody',
    () =>
      input.query === undefined
        ? trace('Semantic.checkBody.execute', () => build().unit)
        : BodyQuery.check(
            input.query,
            context,
            input.authored,
            input.scope,
            input.declaration,
            build,
            trace,
          ),
    {
      module: input.declaration.owner.module,
      declaration: input.declaration.id.ordinal,
    },
  )
}
