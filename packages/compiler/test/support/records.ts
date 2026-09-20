import * as Elaboration from '../../src/Elaboration.js'
import * as Tir from '../../src/Tir.js'

export interface InspectedBody {
  readonly declaration: Elaboration.DeclarationFact
  readonly statements: ReadonlyArray<Tir.Statement>
  readonly returnedExpression: Tir.Expression
  readonly bindings: ReadonlyArray<Tir.Local>
  readonly lifetimeFlow?: Elaboration.BodyResults['lifetimes']
}

export interface Records {
  readonly functions: ReadonlyArray<InspectedBody>
  readonly hiddenFunctions: ReadonlyArray<InspectedBody>
}

const body = (checked: Elaboration.CheckedBody): InspectedBody =>
  Object.freeze({
    declaration: checked.declaration,
    statements: checked.function.statements,
    returnedExpression: Tir.returned(checked.function),
    bindings: Object.freeze(
      (checked.function.locals ?? []).filter((local) => local.kind === 'Binding'),
    ),
    ...(checked.results.lifetimes === undefined ? {} : { lifetimeFlow: checked.results.lifetimes }),
  })

/** Checked TIR bodies exposed to tests without reconstructing construction records. */
export function records(self: Elaboration.Result): Records
export function records(self: Elaboration.Result | undefined): Records | undefined
export function records(self: Elaboration.Result | undefined): Records | undefined {
  if (self === undefined) return undefined
  return Object.freeze({
    functions: Object.freeze(self.bodies.filter((candidate) => !candidate.hidden).map(body)),
    hiddenFunctions: Object.freeze(self.bodies.filter((candidate) => candidate.hidden).map(body)),
  })
}
