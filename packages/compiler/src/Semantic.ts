import type * as AuthoredLowering from './AuthoredLowering.js'
import * as BodyQuery from './BodyQuery.js'
import * as CompilerTrace from './CompilerTrace.js'
import type * as DeclarationFacts from './DeclarationFacts.js'
import type * as Elaboration from './Elaboration.js'
import type * as NameResolution from './NameResolution.js'
import type * as SemanticContext from './SemanticContext.js'

/** Inputs shared by fresh and reusable declaration-body checking. */
export interface BodyInput {
  readonly query?: BodyQuery.BodyQuery
  readonly context: SemanticContext.SemanticContext
  readonly authored: AuthoredLowering.Lowered
  readonly scope: NameResolution.ModuleScope
  readonly declaration: DeclarationFacts.DeclarationFact
  readonly build: () => BodyQuery.Built
  readonly trace?: CompilerTrace.CompilerTrace
}

/** Checks or reuses one declaration body and returns its complete checked unit. */
export const checkBody = (input: BodyInput): Elaboration.CheckedUnit => {
  const trace = input.trace ?? CompilerTrace.none
  return trace(
    'Semantic.checkBody',
    () =>
      input.query === undefined
        ? trace('Semantic.checkBody.execute', () => input.build().unit)
        : BodyQuery.check(
            input.query,
            input.context,
            input.authored,
            input.scope,
            input.declaration,
            input.build,
            trace,
          ),
    {
      module: input.declaration.owner.module,
      declaration: input.declaration.id.ordinal,
    },
  )
}
