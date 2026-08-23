import type * as Analysis from '../../src/Analysis.js'
import type * as Backend from '../../src/Backend.js'
import type * as BootstrapEvaluation from '../../src/BootstrapEvaluation.js'
import type * as DeclarationFacts from '../../src/DeclarationFacts.js'
import type * as Elaboration from '../../src/Elaboration.js'
import * as Hir from '../../src/Hir.js'
import * as Layout from '../../src/Layout.js'
import * as Mir from '../../src/Mir.js'
import * as ModuleTooling from '../../src/ModuleTooling.js'
import * as ProvisionalMir from '../../src/ProvisionalMir.js'
import * as SuspensionOwnership from '../../src/SuspensionOwnership.js'
import * as Type from '../../src/Type.js'

export const syntaxOf = (self: Analysis.FrontendSnapshot, module: string) =>
  self.results.get(module)?.syntax

const nestedStatementFacts = (
  statement: Elaboration.StatementFact,
): ReadonlyArray<Elaboration.StatementFact> => {
  switch (statement._tag) {
    case 'UnsafeStatement':
      return Object.freeze([statement, ...statement.statements.flatMap(nestedStatementFacts)])
    case 'IfStatement':
    case 'IfLetStatement':
      return Object.freeze([
        statement,
        ...statement.taken.flatMap(nestedStatementFacts),
        ...statement.otherwise.flatMap(nestedStatementFacts),
      ])
    case 'WhileStatement':
      return Object.freeze([statement, ...statement.body.flatMap(nestedStatementFacts)])
    default:
      return Object.freeze([statement])
  }
}

export const statementsOf = (
  self: Analysis.FrontendSnapshot,
  module: string,
): ReadonlyArray<Elaboration.StatementFact> =>
  Object.freeze(
    self.results
      .get(module)
      ?.functions.flatMap((fn) => fn.statements.flatMap(nestedStatementFacts)) ?? [],
  )

export const bindingsOf = (self: Analysis.FrontendSnapshot, module: string) =>
  Object.freeze(self.results.get(module)?.functions.flatMap((fn) => fn.bindings) ?? [])

export const writesOf = (self: Analysis.FrontendSnapshot, module: string) =>
  Object.freeze(
    statementsOf(self, module).filter(
      (
        statement,
      ): statement is Extract<Elaboration.StatementFact, { readonly _tag: 'WriteStatement' }> =>
        statement._tag === 'WriteStatement',
    ),
  )

export const loopsOf = (self: Analysis.FrontendSnapshot, module: string) =>
  Object.freeze(
    statementsOf(self, module).filter(
      (
        statement,
      ): statement is Extract<Elaboration.StatementFact, { readonly _tag: 'WhileStatement' }> =>
        statement._tag === 'WhileStatement',
    ),
  )

export const transfersOf = (self: Analysis.FrontendSnapshot, module: string) =>
  Object.freeze(
    statementsOf(self, module).filter(
      (
        statement,
      ): statement is Extract<
        Elaboration.StatementFact,
        { readonly _tag: 'BreakStatement' | 'ContinueStatement' }
      > => statement._tag === 'BreakStatement' || statement._tag === 'ContinueStatement',
    ),
  )

const expressionsOf = (self: Analysis.FrontendSnapshot, module: string) =>
  Object.freeze(
    self.results
      .get(module)
      ?.functions.flatMap((fn) => fn.statements.flatMap(ModuleTooling.statementExpressions)) ?? [],
  )

export const matchesOf = (self: Analysis.FrontendSnapshot, module: string) =>
  Object.freeze(
    expressionsOf(self, module).filter(
      (expression): expression is Extract<Elaboration.ExpressionFact, { readonly _tag: 'Match' }> =>
        expression._tag === 'Match',
    ),
  )

export const hirOf = (self: Analysis.FrontendSnapshot, module: string) =>
  self.results.get(module)?.hir

export const ownershipFixedPointsOf = (self: Analysis.FrontendSnapshot, module: string) =>
  Object.freeze(self.ownership.get(module)?.functions.flatMap((fn) => fn.fixedPoints) ?? [])

export const cleanupExitsOf = (self: Analysis.FrontendSnapshot, module: string) =>
  Object.freeze(self.ownership.get(module)?.functions.flatMap((fn) => fn.exits) ?? [])

export const genericDeclarationsOf = (
  self: Analysis.FrontendSnapshot,
): ReadonlyArray<DeclarationFacts.MemberFact> =>
  Object.freeze(
    self.index.modules.flatMap((module) =>
      module.members.filter((member) => member.typeParameters.length > 0),
    ),
  )

export const genericCallsOf = (self: Analysis.FrontendSnapshot) =>
  Object.freeze(
    [...self.results.values()].flatMap((result) =>
      result.hir.functions.flatMap((fn) =>
        fn.statements
          .flatMap(Hir.statementExpressions)
          .flatMap(Hir.expressionTree)
          .flatMap((expression) =>
            expression._tag === 'Call' && expression.typeArguments.length > 0 ? [expression] : [],
          ),
      ),
    ),
  )

export interface CallInstanceLink {
  readonly call: Extract<Hir.Expression, { readonly _tag: 'Call' }>
  readonly caller: Analysis.Snapshot['instances']['instances'][number]
  readonly target: Analysis.Snapshot['instances']['instances'][number]
}

export const instancesOfCall = (
  self: Analysis.Snapshot,
  call: Extract<Hir.Expression, { readonly _tag: 'Call' }>,
): ReadonlyArray<CallInstanceLink> =>
  Object.freeze(
    self.instances.instances.flatMap((caller): ReadonlyArray<CallInstanceLink> => {
      const ownsCall = caller.function.statements
        .flatMap(Hir.statementExpressions)
        .flatMap(Hir.expressionTree)
        .some((expression) => expression === call)
      if (!ownsCall) return []
      const arguments_ = call.typeArguments.map((argument) =>
        Type.substituteGenericArgument(argument, caller.substitution),
      )
      const target = self.instances.instances.find(
        (candidate) =>
          candidate.key.declaration.module === call.target.module &&
          candidate.key.declaration.name === call.target.name &&
          candidate.key.typeArguments.length === arguments_.length &&
          candidate.key.typeArguments.every((argument, index) => {
            const callArgument = arguments_.at(index)
            return (
              callArgument !== undefined &&
              Type.genericArgumentKey(argument) === Type.genericArgumentKey(callArgument)
            )
          }),
      )
      return target === undefined ? [] : [Object.freeze({ call, caller, target })]
    }),
  )

export const suspensionFactsOf = (self: Analysis.Snapshot) => self.instances.suspension

export const provisionalMirOf = (
  self: Analysis.Snapshot,
): Analysis.Targeted<ProvisionalMir.Module> =>
  self.layout._tag === 'Available'
    ? Object.freeze({
        _tag: 'Available',
        value: ProvisionalMir.build(self.instances, self.layout.value, self.index),
      })
    : self.layout

export const suspensionOwnershipOf = (
  self: Analysis.Snapshot,
): Analysis.Targeted<SuspensionOwnership.Module> => {
  if (self.mir._tag === 'Unavailable') return self.mir
  const provisional = provisionalMirOf(self)
  return provisional._tag === 'Unavailable'
    ? provisional
    : Object.freeze({
        _tag: 'Available',
        value: SuspensionOwnership.plan(self.mir.value, provisional.value, self.index),
      })
}

export const hirMatchesOf = (self: Analysis.FrontendSnapshot, module: string) =>
  Object.freeze(
    (self.results.get(module)?.hir.functions ?? []).flatMap((fn) =>
      fn.statements
        .flatMap(Hir.statementExpressions)
        .flatMap(Hir.expressionTree)
        .flatMap((expression) => (expression._tag === 'Match' ? [expression] : [])),
    ),
  )

export const callingShapeOf = (self: Analysis.Snapshot, type: Type.Type) =>
  self.layout._tag === 'Available' ? Layout.callingShape(self.layout.value, type) : undefined

export interface ControlRegionFact {
  readonly function: DeclarationFacts.CanonicalId
  readonly region: Mir.Region
}

export const controlRegionsOf = (self: Analysis.Snapshot): ReadonlyArray<ControlRegionFact> =>
  self.mir._tag === 'Unavailable'
    ? Object.freeze([])
    : Object.freeze(
        self.mir.value.functions.flatMap((fn) =>
          Mir.topologicalRegions(fn).map((region) => Object.freeze({ function: fn.id, region })),
        ),
      )

export interface ControlEdgeFact {
  readonly function: DeclarationFacts.CanonicalId
  readonly edge: Mir.ControlEdge
}

export const controlEdgesOf = (self: Analysis.Snapshot): ReadonlyArray<ControlEdgeFact> =>
  self.mir._tag === 'Unavailable'
    ? Object.freeze([])
    : Object.freeze(
        self.mir.value.functions.flatMap((fn) =>
          Mir.controlEdges(fn).map((edge) => Object.freeze({ function: fn.id, edge })),
        ),
      )

export const traceOf = (outcome: BootstrapEvaluation.Outcome) => outcome.trace
export const backendControlOf = (artifact: Backend.Artifact) => artifact.control

export const arrayTraceEventsOf = (outcome: BootstrapEvaluation.Outcome) =>
  Object.freeze(
    outcome.trace.filter(
      (
        event,
      ): event is
        | BootstrapEvaluation.ArrayConstructTraceEvent
        | BootstrapEvaluation.PlaceReadTraceEvent =>
        event._tag === 'ArrayConstruct' || event._tag === 'PlaceRead',
    ),
  )

export const allocationTraceEventsOf = (outcome: BootstrapEvaluation.Outcome) =>
  Object.freeze(
    outcome.trace.filter(
      (event): event is BootstrapEvaluation.AllocationTraceEvent =>
        event._tag === 'AllocationAcquire' ||
        event._tag === 'RawBufferForm' ||
        event._tag === 'SharedInitialize' ||
        event._tag === 'SharedClone' ||
        event._tag === 'SharedDecrement' ||
        event._tag === 'SharedLastCleanup' ||
        event._tag === 'SharedAccessBegin' ||
        event._tag === 'SharedAccessConflict' ||
        event._tag === 'SharedAccessEnd' ||
        event._tag === 'SlotProject' ||
        event._tag === 'SlotWrite' ||
        event._tag === 'SlotTake' ||
        event._tag === 'SlotCopy' ||
        event._tag === 'RawBufferRead' ||
        event._tag === 'RawBufferCopy' ||
        event._tag === 'RawBufferFill' ||
        event._tag === 'SlotDrop' ||
        event._tag === 'AllocationRelease',
    ),
  )
