import type * as AuthoredHir from './AuthoredHir.js'
import * as AuthoredIdentity from './AuthoredIdentity.js'
import * as AuthoredWalk from './AuthoredWalk.js'
import * as SemanticContext from './SemanticContext.js'
import * as MachineFunction from './MachineFunction.js'
import * as Lifetime from './Lifetime.js'
import * as BodyLifetime from './BodyLifetime.js'
import * as LifetimeFlow from './LifetimeFlow.js'
import * as NominalVariance from './NominalVariance.js'
import * as TypeOutlives from './TypeOutlives.js'
import { concreteCallableIdentity, exactCallableOf, executableSites } from './CallResolution.js'
import * as DeclarationCollection from './DeclarationCollection.js'
import * as DeclarationFacts from './DeclarationFacts.js'
import * as DeclarationLifetime from './DeclarationLifetime.js'
import * as DeclarationResolution from './DeclarationResolution.js'
import * as Diagnostic from './Diagnostic.js'
import type {
  BindingDeclarationFact,
  DeclarationFact,
  ExpressionFact,
  PatternSelectionFact,
  PatternBindingFact,
  StatementFact,
  StaticIterationFact,
} from './Elaboration.js'
import {
  assignmentRoot,
  assignmentRootAccess,
  compatible,
  declaredReturnTypesCompatible,
  expressionChildren,
  representationJoinDiagnostic,
  typesCompatible,
  unavailableCompatibility,
  unionConversionDiagnostic,
  visitExpressionFacts,
  visitStatementFacts,
} from './Elaboration.js'
import type {
  BodyContext,
  FunctionAnalysis,
  ResolutionContext,
  Scope,
} from './ExpressionAnalysis.js'
import {
  analyzeExpression,
  analyzePattern,
  patternTests,
  bindingName,
  coverageMembersOf,
  containsOrdinaryArm,
  enumFactByType,
  representationOfExpression,
  patternCoverage,
  unsafeCallAuthorized,
} from './ExpressionAnalysis.js'
import type * as Tir from './Tir.js'
import { directStatementExpressions } from './TirLowering.js'
import * as Match from './Match.js'
import * as NameResolution from './NameResolution.js'
import * as SemanticDisplay from './SemanticDisplay.js'
import * as SourceSpan from './SourceSpan.js'
import * as StaticEvaluation from './StaticEvaluation.js'
import * as StaticValue from './StaticValue.js'
import * as Type from './Type.js'
export const unsafeCallDiagnostic = (
  unsafe: boolean,
  spelling: string,
  call: AuthoredHir.Expression,
  callSpan: SourceSpan.SourceSpan,
  resolution: ResolutionContext | undefined,
): Diagnostic.Diagnostic | undefined =>
  unsafe && !unsafeCallAuthorized(resolution, call, callSpan)
    ? Diagnostic.missingUnsafeBoundary(spelling, callSpan)
    : undefined

interface StaticIterationElement {
  readonly value: StaticValue.Value
  readonly type: Type.Type
}

/** Opens only the two finite compiler-owned iterable representations admitted by `static for`. */
const staticIterationElements = (
  value: StaticValue.Value,
): ReadonlyArray<StaticIterationElement> | undefined => {
  if (value._tag === 'StaticSequenceValue')
    return Object.freeze(
      value.elements.map((element) => Object.freeze({ value: element, type: value.elementType })),
    )
  if (value._tag === 'FieldCollectionValue')
    return Object.freeze(
      value.fields.map((field) =>
        Object.freeze({ value: field, type: StaticValue.fieldDescriptorType(field) }),
      ),
    )
  return undefined
}

/** `return` with no value returns unit; a synthetic literal stands in for the absent expression. */
const unitValueOf = (
  statement: Extract<AuthoredHir.Statement, { readonly _tag: 'ReturnStatement' }>,
): AuthoredHir.Expression =>
  Object.freeze({
    _tag: 'UnitLiteral',
    anchor: statement.anchor,
    origin: Object.freeze({
      _tag: 'Synthetic',
      anchor: statement.anchor,
      role: 'value',
      occurrence: 0,
    }),
    causes: Object.freeze([]),
  })

const residualNodeCount = (statements: ReadonlyArray<StatementFact>): number => {
  let nodes = 0
  visitStatementFacts(statements, {
    statement: () => {
      nodes += 1
    },
    expression: () => {
      nodes += 1
    },
  })
  return nodes
}

const nestedStaticIterationNodeCount = (iterations: ReadonlyArray<StaticIterationFact>): number =>
  iterations.reduce(
    (total, iteration) =>
      total +
      iteration.scopes.reduce(
        (scopeTotal, scope) => scopeTotal + residualNodeCount(scope.statements),
        0,
      ),
    0,
  )

export const analyzeStatements = (
  context: BodyContext,
  blockNode: AuthoredHir.Block,
  initialScope: Scope,
  loopStack: ReadonlyArray<Tir.LoopId> = Object.freeze([]),
  introducedPatterns: ReadonlyArray<PatternBindingFact> = Object.freeze([]),
): ReadonlyArray<StatementFact> => {
  context = { ...context, resolution: { ...context.resolution, execution: { context, loopStack } } }
  const facts: Array<StatementFact> = []
  let scope = initialScope
  const ordinaryControlContext: BodyContext =
    context.declaration.phase !== 'Static' || context.resolution.deferStaticCalls === true
      ? context
      : Object.freeze({
          ...context,
          resolution: Object.freeze({ ...context.resolution, deferStaticCalls: true as const }),
        })
  const blockBindings = new Map<string, SourceSpan.SourceSpan>(
    introducedPatterns.flatMap((binding) =>
      binding.name._tag === 'Present'
        ? [[binding.name.spelling, context.context.spanOf(binding.name.anchor)] as const]
        : [],
    ),
  )
  const callableWrites = context.resolution.writtenCallableBindings
  const snapshotCallableWrites = (): ReadonlySet<number> => new Set(callableWrites)
  const restoreCallableWrites = (snapshot: ReadonlySet<number>): void => {
    if (callableWrites === undefined) return
    callableWrites.clear()
    for (const ordinal of snapshot) callableWrites.add(ordinal)
  }
  const mergeCallableWrites = (...snapshots: ReadonlyArray<ReadonlySet<number>>): void => {
    restoreCallableWrites(new Set(snapshots.flatMap((snapshot) => [...snapshot])))
  }
  const analyzePath = <A>(initial: ReadonlySet<number>, analyze: () => A) => {
    restoreCallableWrites(initial)
    const value = analyze()
    return Object.freeze({ value, writes: snapshotCallableWrites() })
  }
  const replaceMap = <K, V>(target: Map<K, V>, snapshot: ReadonlyMap<K, V>): void => {
    target.clear()
    for (const [key, value] of snapshot) target.set(key, value)
  }
  const staticExpansionCheckpoint = () =>
    Object.freeze({
      bindings: context.bindings.length,
      regions: context.regions.length,
      loops: context.loops.length,
      staticIterations: context.staticIterations.length,
      nextBindingOrdinal: context.nextBindingOrdinal.value,
      callableWrites: snapshotCallableWrites(),
      generatedAggregates: new Map(context.resolution.generatedAggregates),
      values: new Map(context.staticContext?.values),
      valueSpans: new Map(context.staticContext?.valueSpans),
      valueOrigins: new Map(context.staticContext?.valueOrigins),
      expressionSpans: new Map(context.staticContext?.expressionSpans),
      expressionOrigins: new Map(context.staticContext?.expressionOrigins),
    })
  const restoreStaticExpansion = (
    checkpoint: ReturnType<typeof staticExpansionCheckpoint>,
  ): void => {
    context.bindings.length = checkpoint.bindings
    context.regions.length = checkpoint.regions
    context.loops.length = checkpoint.loops
    context.staticIterations.length = checkpoint.staticIterations
    context.nextBindingOrdinal.value = checkpoint.nextBindingOrdinal
    restoreCallableWrites(checkpoint.callableWrites)
    if (context.resolution.generatedAggregates !== undefined)
      replaceMap(context.resolution.generatedAggregates, checkpoint.generatedAggregates)
    if (context.staticContext !== undefined) {
      replaceMap(context.staticContext.values, checkpoint.values)
      replaceMap(context.staticContext.valueSpans, checkpoint.valueSpans)
      replaceMap(context.staticContext.valueOrigins, checkpoint.valueOrigins)
      replaceMap(context.staticContext.expressionSpans, checkpoint.expressionSpans)
      replaceMap(context.staticContext.expressionOrigins, checkpoint.expressionOrigins)
    }
  }
  const unionWriteStates = (
    ...states: ReadonlyArray<ReadonlySet<number> | undefined>
  ): ReadonlySet<number> | undefined => {
    const present = states.filter((state): state is ReadonlySet<number> => state !== undefined)
    return present.length === 0 ? undefined : new Set(present.flatMap((state) => [...state]))
  }
  interface CallableWriteFlow {
    readonly fallthrough?: ReadonlySet<number>
    readonly continues?: ReadonlySet<number>
    readonly breaks?: ReadonlySet<number>
  }
  const expressionWrites = (
    expression: ExpressionFact,
    initial: ReadonlySet<number>,
  ): ReadonlySet<number> => {
    const writes = new Set(initial)
    visitExpressionFacts(expression, {
      descendEffectBlocks: false,
      statement: (statement) => {
        if (
          statement._tag === 'WriteStatement' &&
          !expressionNever(statement.destination) &&
          !expressionNever(statement.value) &&
          statement.root?._tag === 'BindingFact' &&
          statement.root.inferredType._tag === 'Available' &&
          Type.isCallable(statement.root.inferredType.type)
        )
          writes.add(statement.root.id.ordinal)
      },
      expression: (candidate) => {
        if (
          candidate._tag === 'PlaceReplace' &&
          !expressionNever(candidate.destination) &&
          !expressionNever(candidate.value) &&
          candidate.root?._tag === 'BindingFact' &&
          candidate.root.inferredType._tag === 'Available' &&
          Type.isCallable(candidate.root.inferredType.type)
        )
          writes.add(candidate.root.id.ordinal)
      },
    })
    return writes
  }
  const callableWriteFlow = (
    statements: ReadonlyArray<StatementFact>,
    initial: ReadonlySet<number>,
    targetLoop: Tir.LoopId,
  ): CallableWriteFlow => {
    let fallthrough: ReadonlySet<number> | undefined = new Set(initial)
    let continues: ReadonlySet<number> | undefined
    let breaks: ReadonlySet<number> | undefined
    for (const statement of statements) {
      if (fallthrough === undefined) break
      if (statement._tag === 'BindStatement') {
        fallthrough = expressionWrites(statement.binding.initializer, fallthrough)
        if (expressionNever(statement.binding.initializer)) fallthrough = undefined
        continue
      }
      if (statement._tag === 'PatternBindStatement') {
        fallthrough = expressionWrites(statement.selection.source, fallthrough)
        if (expressionNever(statement.selection.source)) fallthrough = undefined
        continue
      }
      if (statement._tag === 'ExpressionStatement' || statement._tag === 'DropStatement') {
        fallthrough = expressionWrites(statement.expression, fallthrough)
        if (expressionNever(statement.expression)) fallthrough = undefined
        continue
      }
      if (statement._tag === 'WriteStatement') {
        fallthrough = expressionWrites(
          statement.value,
          expressionWrites(statement.destination, fallthrough),
        )
        if (
          statement.root?._tag === 'BindingFact' &&
          statement.root.inferredType._tag === 'Available' &&
          Type.isCallable(statement.root.inferredType.type)
        )
          fallthrough = new Set(fallthrough).add(statement.root.id.ordinal)
        if (expressionNever(statement.destination) || expressionNever(statement.value))
          fallthrough = undefined
        continue
      }
      if (statement._tag === 'ReturnStatement' || statement._tag === 'FailStatement') {
        expressionWrites(statement.expression, fallthrough)
        fallthrough = undefined
        continue
      }
      if (statement._tag === 'BreakStatement' || statement._tag === 'ContinueStatement') {
        if (statement.target === targetLoop) {
          if (statement._tag === 'BreakStatement') breaks = unionWriteStates(breaks, fallthrough)
          else continues = unionWriteStates(continues, fallthrough)
        }
        fallthrough = undefined
        continue
      }
      if (statement._tag === 'UnsafeStatement') {
        const nested = callableWriteFlow(statement.statements, fallthrough, targetLoop)
        fallthrough = nested.fallthrough
        continues = unionWriteStates(continues, nested.continues)
        breaks = unionWriteStates(breaks, nested.breaks)
        continue
      }
      if (statement._tag === 'IfStatement' || statement._tag === 'IfLetStatement') {
        const afterCondition = expressionWrites(
          statement._tag === 'IfStatement' ? statement.condition : statement.selection.source,
          fallthrough,
        )
        if (
          expressionNever(
            statement._tag === 'IfStatement' ? statement.condition : statement.selection.source,
          )
        ) {
          fallthrough = undefined
          continue
        }
        const taken = callableWriteFlow(statement.taken, afterCondition, targetLoop)
        const otherwise = callableWriteFlow(statement.otherwise, afterCondition, targetLoop)
        fallthrough = unionWriteStates(taken.fallthrough, otherwise.fallthrough)
        continues = unionWriteStates(continues, taken.continues, otherwise.continues)
        breaks = unionWriteStates(breaks, taken.breaks, otherwise.breaks)
        continue
      }
      if (statement._tag === 'WhileStatement') {
        const nestedEntry = expressionWrites(statement.condition, fallthrough)
        if (expressionNever(statement.condition)) {
          fallthrough = undefined
          continue
        }
        const nested = callableWriteFlow(statement.body, nestedEntry, statement.loop)
        const nestedBackedge = unionWriteStates(nested.fallthrough, nested.continues)
        fallthrough = unionWriteStates(nestedEntry, nestedBackedge, nested.breaks)
      }
    }
    return Object.freeze({
      ...(fallthrough === undefined ? {} : { fallthrough }),
      ...(continues === undefined ? {} : { continues }),
      ...(breaks === undefined ? {} : { breaks }),
    })
  }

  const nextRegion = (): Tir.RegionId => {
    const region = Object.freeze({
      _tag: 'TirRegion' as const,
      function: context.declaration.id,
      ordinal: (context.regionBase ?? 0) + context.regions.length,
    })
    context.regions.push(region)
    return region
  }

  const staticDiagnostic = (failure: StaticEvaluation.StaticFailure): Diagnostic.Diagnostic =>
    StaticEvaluation.diagnostic(
      failure,
      context.staticContext?.environment.target ?? 'unselected-target',
    )

  const analyzePatternSelection = (
    element: Extract<
      AuthoredHir.Statement,
      { readonly _tag: 'PatternBindingStatement' | 'PatternConditionalStatement' }
    >,
    selectionScope: Scope,
  ): PatternSelectionFact => {
    const initializerNode =
      element._tag === 'PatternBindingStatement' ? element.initializer : element.subject
    const initializer = analyzeExpression(
      context.context,
      initializerNode,
      context.declarations,
      context.declaration,
      selectionScope,
      context.resolution,
      undefined,
    )
    if (initializer === undefined) {
      throw new RangeError(`Semantic analysis cannot analyze ${initializerNode._tag}`)
    }
    context.diagnostics.push(...initializer.diagnostics)
    let access: Match.Access
    if (initializer.fact._tag === 'Move') {
      access = 'Move'
    } else if (initializer.fact._tag === 'Borrow') {
      access = initializer.fact.access
    } else {
      access = 'Copy'
    }
    const subject =
      initializer.fact._tag === 'Move' || initializer.fact._tag === 'Borrow'
        ? initializer.fact.subject
        : initializer.fact
    const id: Match.MatchId = Object.freeze({
      _tag: 'MatchId',
      function: context.declaration.id,
      span: context.context.spanOf(element.anchor),
    })
    const arm: Match.ArmId = Object.freeze({ _tag: 'MatchArmId', match: id, ordinal: 0 })
    const pattern = analyzePattern(
      context.context,
      element.pattern,
      arm,
      access,
      selectionScope,
      context.resolution,
      context.declaration,
      { pattern: 0, binding: 0, invalid: false },
      subject.type._tag === 'Available' ? subject.type.type : undefined,
    )
    context.diagnostics.push(...pattern.diagnostics)
    const subjectEnum =
      subject.type._tag === 'Available'
        ? enumFactByType(context.resolution.index, subject.type.type)
        : undefined
    let members: ReadonlyArray<Match.CoverageIdentity>
    if (subject.type._tag !== 'Available') {
      members = []
    } else if (subjectEnum === undefined) {
      members = coverageMembersOf(context.resolution.index, subject.type.type)
    } else {
      members = Match.enumMembersOf(subjectEnum)
    }
    let member: Match.CoverageIdentity | undefined
    if (pattern.fact._tag === 'EnumMemberPattern') {
      member = pattern.fact.coverage
    } else if (pattern.fact._tag === 'UnionVariantPattern') {
      member = pattern.fact.coverage
    } else if (
      (pattern.fact._tag === 'NominalPattern' || pattern.fact._tag === 'TypePattern') &&
      pattern.fact.member !== undefined
    ) {
      member = Match.structuralMember(pattern.fact.member)
    } else {
      member = undefined
    }
    if (
      subjectEnum === undefined &&
      member !== undefined &&
      subject.type._tag === 'Available' &&
      !members.some((candidate) => Match.selects(member, candidate))
    ) {
      context.diagnostics.push(
        Diagnostic.matchMemberNotInScrutinee(
          Type.encode(member.type),
          Type.encode(subject.type.type),
          context.context.spanOf(pattern.fact.anchor),
        ),
      )
    }
    const tests = patternTests(context.context, context.resolution.index, pattern.fact)
    const coverage = Match.cover(
      members,
      Object.freeze([
        Object.freeze({
          ...(member === undefined ? {} : { member }),
          universal: pattern.fact._tag === 'UniversalPattern',
          tests,
          guarded: false,
        }),
      ]),
    )
    const complete = pattern.fact._tag === 'UniversalPattern' ? true : pattern.fact.complete
    return Object.freeze({
      _tag: 'PatternSelection',
      tests,
      id,
      arm,
      access,
      source: initializer.fact,
      subject,
      members: Object.freeze(members),
      pattern: pattern.fact,
      bindings: pattern.fact.bindings,
      irrefutable: coverage.exhaustive && complete,
      loanEnd:
        element._tag === 'PatternBindingStatement'
          ? context.context.spanOf(blockNode.anchor)
          : context.context.spanOf(element.anchor),
      anchor: element.anchor,
    })
  }

  const analyzeConditional = (
    element: Extract<
      AuthoredHir.Conditional,
      { readonly _tag: 'ConditionalStatement' | 'StaticConditionalStatement' }
    >,
    armScope: Scope,
    armLoopStack: ReadonlyArray<Tir.LoopId>,
  ): StatementFact => {
    const region = nextRegion()
    const conditionNode = element.condition
    const condition = analyzeExpression(
      context.context,
      conditionNode,
      context.declarations,
      context.declaration,
      armScope,
      ordinaryControlContext.resolution,
    )
    if (condition === undefined) {
      throw new RangeError(`Semantic analysis cannot analyze ${conditionNode._tag}`)
    }
    context.diagnostics.push(...condition.diagnostics)
    if (
      condition.fact.type._tag === 'Available' &&
      condition.fact.type.type !== 'bool' &&
      !Type.isNever(condition.fact.type.type)
    ) {
      context.diagnostics.push(
        Diagnostic.conditionNotBool(
          Type.encode(condition.fact.type.type),
          context.context.spanOf(condition.fact.anchor),
        ),
      )
    }

    const branchEntry = snapshotCallableWrites()
    const taken = analyzePath(branchEntry, () =>
      analyzeStatements(ordinaryControlContext, element.thenBranch, armScope, armLoopStack),
    )
    const otherwise = analyzePath(branchEntry, () =>
      analyzeElseBranch(element.elseBranch, armScope, armLoopStack),
    )
    const takenFallsThrough = returnFlowOf(taken.value, false).fallsThrough
    const otherwiseFallsThrough = returnFlowOf(otherwise.value, false).fallsThrough
    mergeCallableWrites(
      ...(takenFallsThrough ? [taken.writes] : []),
      ...(otherwiseFallsThrough ? [otherwise.writes] : []),
      ...(!takenFallsThrough && !otherwiseFallsThrough ? [branchEntry] : []),
    )
    return Object.freeze({
      _tag: 'IfStatement',
      condition: condition.fact,
      taken: Object.freeze([...taken.value]),
      otherwise: Object.freeze([...otherwise.value]),
      region,
      anchor: element.anchor,
    })
  }

  /** Both authored arms are present; an absent else branch contributes no statements. */
  const analyzeElseBranch = (
    branch: AuthoredHir.Block | AuthoredHir.Conditional | undefined,
    armScope: Scope,
    armLoopStack: ReadonlyArray<Tir.LoopId>,
  ): ReadonlyArray<StatementFact> => {
    if (branch === undefined) return Object.freeze([])
    if (branch._tag === 'Block')
      return analyzeStatements(ordinaryControlContext, branch, armScope, armLoopStack)
    return branch._tag === 'PatternConditionalStatement'
      ? [analyzePatternConditional(branch, armScope, armLoopStack)]
      : [analyzeConditional(branch, armScope, armLoopStack)]
  }

  const analyzePatternConditional = (
    element: Extract<AuthoredHir.Conditional, { readonly _tag: 'PatternConditionalStatement' }>,
    armScope: Scope,
    armLoopStack: ReadonlyArray<Tir.LoopId>,
  ): StatementFact => {
    const region = nextRegion()
    const selection = analyzePatternSelection(element, armScope)
    const takenScope: Scope = Object.freeze({
      parameters: armScope.parameters,
      bindings: armScope.bindings,
      patternBindings: Object.freeze([...armScope.patternBindings, ...selection.bindings]),
    })
    const branchEntry = snapshotCallableWrites()
    const taken = analyzePath(branchEntry, () =>
      analyzeStatements(ordinaryControlContext, element.thenBranch, takenScope, armLoopStack),
    )
    const otherwise = analyzePath(branchEntry, () =>
      analyzeElseBranch(element.elseBranch, armScope, armLoopStack),
    )
    const takenFallsThrough = returnFlowOf(taken.value, false).fallsThrough
    const otherwiseFallsThrough = returnFlowOf(otherwise.value, false).fallsThrough
    mergeCallableWrites(
      ...(takenFallsThrough ? [taken.writes] : []),
      ...(otherwiseFallsThrough ? [otherwise.writes] : []),
      ...(!takenFallsThrough && !otherwiseFallsThrough ? [branchEntry] : []),
    )
    return Object.freeze({
      _tag: 'IfLetStatement',
      selection,
      taken: Object.freeze([...taken.value]),
      otherwise: Object.freeze([...otherwise.value]),
      region,
      anchor: element.anchor,
    })
  }

  for (const element of blockNode.statements) {
    if (element._tag === 'UnsafeStatement') {
      const region = nextRegion()
      const statements = analyzeStatements(context, element.body, scope, loopStack)
      facts.push(
        Object.freeze({
          _tag: 'UnsafeStatement',
          statements,
          region,
          anchor: element.anchor,
        }),
      )
      continue
    }

    if (element._tag === 'BindingStatement') {
      const region = nextRegion()
      const bindingOrdinal = context.nextBindingOrdinal.value
      context.nextBindingOrdinal.value += 1
      const initializerNode = element.initializer
      const declaredSyntax = element.type
      const analyzedDeclared =
        declaredSyntax === undefined
          ? undefined
          : DeclarationCollection.analyzeDeclaredType(
              context.context,
              declaredSyntax,
              new Map(
                context.declaration.typeParameters.flatMap((parameter) =>
                  parameter.name._tag === 'Present'
                    ? [[parameter.name.spelling, parameter.type] as const]
                    : [],
                ),
              ),
              false,
              context.resolution.bodyLifetimes === undefined ||
                context.resolution.authoredDeclaration === undefined
                ? undefined
                : DeclarationLifetime.forBody(
                    context.context,
                    context.resolution.bodyLifetimes,
                    context.resolution.authoredDeclaration,
                    new Map(
                      context.declaration.typeParameters.flatMap((parameter) =>
                        parameter.name._tag === 'Present'
                          ? [[parameter.name.spelling, parameter.type] as const]
                          : [],
                      ),
                    ),
                  ),
            )
      const nameResolution: NameResolution.Resolution = Object.freeze({
        _tag: 'NameResolution',
        modules: Object.freeze([context.resolution.scope]),
        contexts: SemanticContext.registry([context.context]),
        diagnostics: Object.freeze([]),
      })
      const resolvedDeclared =
        analyzedDeclared === undefined
          ? undefined
          : DeclarationResolution.resolveTypeFact(
              context.context.spanOf,
              context.resolution.index,
              AuthoredWalk.moduleName(context.context),
              analyzedDeclared.fact,
              (module, path) =>
                NameResolution.resolveType(nameResolution, context.resolution.index, module, path),
            )
      if (analyzedDeclared !== undefined) context.diagnostics.push(...analyzedDeclared.diagnostics)
      if (resolvedDeclared !== undefined) context.diagnostics.push(...resolvedDeclared.diagnostics)
      const expected =
        resolvedDeclared?.fact._tag === 'Resolved' ? resolvedDeclared.fact.type : undefined
      const initializer = analyzeExpression(
        context.context,
        initializerNode,
        context.declarations,
        context.declaration,
        scope,
        context.resolution,
        expected,
      )
      if (initializer === undefined) {
        throw new RangeError(`Semantic analysis cannot analyze ${initializerNode._tag}`)
      }
      context.diagnostics.push(...initializer.diagnostics)
      if (
        expected !== undefined &&
        initializer.type !== undefined &&
        !typesCompatible(initializer.type, expected, context.resolution?.lifetimeCompatibility)
      )
        context.diagnostics.push(
          Diagnostic.assignmentTypeMismatch(
            Type.encode(expected),
            Type.encode(initializer.type),
            context.context.spanOf(initializerNode.anchor),
          ),
        )

      if (element.mutable && initializer.type !== undefined && Type.isEffect(initializer.type))
        context.diagnostics.push(
          Diagnostic.mutableEffectRecipe(context.context.spanOf(element.anchor)),
        )

      const name = bindingName(context.context, element.name)
      const exactCallable = exactCallableOf(
        initializer.fact,
        context.resolution.writtenCallableBindings,
      )
      const hasConcreteCallableIdentity = concreteCallableIdentity(
        initializer.fact,
        context.resolution.writtenCallableBindings,
      )
      const phase: 'Runtime' | 'Static' =
        context.declaration.phase === 'Static' || element.static ? 'Static' : 'Runtime'
      const bindingType = expected ?? initializer.type
      if (
        phase === 'Runtime' &&
        bindingType !== undefined &&
        Type.containsStaticPhaseOnly(bindingType)
      )
        context.diagnostics.push(
          Diagnostic.staticPhaseViolation(
            'runtime binding of a phase-only value',
            context.staticContext?.environment.target ?? 'unselected-target',
            Object.freeze([]),
            context.context.spanOf(element.anchor),
          ),
        )
      const evaluated =
        phase === 'Static' &&
        context.staticContext !== undefined &&
        context.resolution.deferStaticCalls !== true &&
        !containsOrdinaryArm(initializerNode)
          ? StaticEvaluation.evaluateFact(initializer.fact, context.staticContext)
          : undefined
      if (evaluated?._tag === 'Failed')
        context.diagnostics.push(staticDiagnostic(evaluated.failure))
      const staticValue: StaticValue.Value | undefined =
        evaluated?._tag === 'Complete' ? evaluated.value : undefined
      const binding: BindingDeclarationFact = Object.freeze({
        _tag: 'BindingFact',
        id: Object.freeze({
          _tag: 'TirBinding',
          function: context.declaration.id,
          ordinal: bindingOrdinal,
        }),
        name,
        phase,
        mutability: element.mutable ? 'Mutable' : 'Immutable',
        ...(resolvedDeclared === undefined ? {} : { declaredType: resolvedDeclared.fact }),
        // A declared union is the binding's type: the initializer injects at the binding boundary.
        inferredType:
          expected !== undefined &&
          Type.isUnion(expected) &&
          initializer.type !== undefined &&
          typesCompatible(initializer.type, expected, context.resolution?.lifetimeCompatibility)
            ? Object.freeze({ _tag: 'Available', type: expected })
            : initializer.fact.type,
        initializer: initializer.fact,
        ...(staticValue === undefined ? {} : { staticValue }),
        ...(exactCallable === undefined ? {} : { exactCallable }),
        ...(hasConcreteCallableIdentity ? { concreteCallableIdentity: true as const } : {}),
        anchor: element.anchor,
      })
      context.bindings.push(binding)
      if (staticValue !== undefined && context.staticContext !== undefined) {
        const key = StaticEvaluation.localValueKey(binding)
        context.staticContext.values.set(key, staticValue)
        const staticTextSpan = context.staticContext.expressionSpans.get(initializer.fact)
        if (staticTextSpan !== undefined) context.staticContext.valueSpans.set(key, staticTextSpan)
        const staticTextOrigin = context.staticContext.expressionOrigins.get(initializer.fact)
        if (staticTextOrigin !== undefined)
          context.staticContext.valueOrigins.set(key, staticTextOrigin)
      }
      facts.push(Object.freeze({ _tag: 'BindStatement', binding, region }))

      if (name._tag === 'Present') {
        const originalSpan = blockBindings.get(name.spelling)
        if (originalSpan === undefined) {
          blockBindings.set(name.spelling, context.context.spanOf(name.anchor))
          scope = Object.freeze({
            parameters: scope.parameters,
            bindings: Object.freeze([...scope.bindings, binding]),
            patternBindings: scope.patternBindings,
          })
        } else {
          context.diagnostics.push(
            Diagnostic.rebindingName(
              name.spelling,
              originalSpan,
              context.context.spanOf(name.anchor),
            ),
          )
        }
      }
      continue
    }

    if (element._tag === 'PatternBindingStatement') {
      const region = nextRegion()
      const selection = analyzePatternSelection(element, scope)
      if (selection.pattern._tag === 'UniversalPattern') {
        if (selection.subject.type._tag === 'Available')
          context.diagnostics.push(
            Diagnostic.expressionStatementResult(
              SemanticDisplay.type(
                selection.subject.type.type,
                AuthoredWalk.moduleName(context.context),
                context.resolution.scope,
              ),
              context.context.spanOf(selection.pattern.anchor),
            ),
          )
      } else if (selection.pattern._tag !== 'UnavailablePattern' && !selection.irrefutable) {
        let selected: Match.CoverageIdentity | undefined
        if (selection.pattern._tag === 'EnumMemberPattern') {
          selected = selection.pattern.coverage
        } else if (selection.pattern._tag === 'UnionVariantPattern') {
          selected = selection.pattern.coverage
        } else if (
          (selection.pattern._tag === 'NominalPattern' ||
            selection.pattern._tag === 'TypePattern') &&
          selection.pattern.member !== undefined
        ) {
          selected = Match.structuralMember(selection.pattern.member)
        } else {
          selected = undefined
        }
        context.diagnostics.push(
          Diagnostic.refutableLetPattern(
            selection.subject.type._tag === 'Available'
              ? SemanticDisplay.type(
                  selection.subject.type.type,
                  AuthoredWalk.moduleName(context.context),
                  context.resolution.scope,
                )
              : '<unavailable>',
            selection.members
              .filter((member) => selected === undefined || !Match.selects(selected, member))
              .map(Match.encodeIdentity),
            context.context.spanOf(selection.pattern.anchor),
          ),
        )
      }
      facts.push(
        Object.freeze({
          _tag: 'PatternBindStatement',
          selection,
          region,
          anchor: element.anchor,
        }),
      )
      for (const binding of selection.bindings) {
        if (binding.name._tag !== 'Present') continue
        const originalSpan = blockBindings.get(binding.name.spelling)
        if (originalSpan === undefined)
          blockBindings.set(binding.name.spelling, context.context.spanOf(binding.name.anchor))
      }
      scope = Object.freeze({
        parameters: scope.parameters,
        bindings: scope.bindings,
        patternBindings: Object.freeze([...scope.patternBindings, ...selection.bindings]),
      })
      continue
    }

    if (element._tag === 'ExpressionStatement') {
      const region = nextRegion()
      const expressionNode = element.expression
      const expression = analyzeExpression(
        context.context,
        expressionNode,
        context.declarations,
        context.declaration,
        scope,
        context.resolution,
      )
      if (expression === undefined) {
        throw new RangeError(`Semantic analysis cannot analyze ${expressionNode._tag}`)
      }
      context.diagnostics.push(...expression.diagnostics)
      if (
        context.staticContext !== undefined &&
        context.declaration.phase !== 'Static' &&
        expression.fact._tag === 'CompileError'
      ) {
        const evaluated = StaticEvaluation.evaluateFact(expression.fact, context.staticContext)
        if (evaluated._tag === 'Failed')
          context.diagnostics.push(staticDiagnostic(evaluated.failure))
        facts.push(
          Object.freeze({
            _tag: 'ExpressionStatement',
            expression: expression.fact,
            region,
            anchor: element.anchor,
          }),
        )
        continue
      }
      if (
        expression.type !== undefined &&
        !Type.equals(expression.type, Type.unit) &&
        !Type.isNever(expression.type)
      ) {
        context.diagnostics.push(
          Diagnostic.expressionStatementResult(
            SemanticDisplay.type(
              expression.type,
              AuthoredWalk.moduleName(context.context),
              context.resolution.scope,
            ),
            context.context.spanOf(expressionNode.anchor),
          ),
        )
      }
      facts.push(
        Object.freeze({
          _tag: 'ExpressionStatement',
          expression: expression.fact,
          region,
          anchor: element.anchor,
        }),
      )
      continue
    }

    if (element._tag === 'StaticForStatement') {
      const checkpoint = staticExpansionCheckpoint()
      const iterableNode = element.iterable
      const iterable = analyzeExpression(
        context.context,
        iterableNode,
        context.declarations,
        context.declaration,
        scope,
        context.resolution,
      )
      if (iterable === undefined)
        throw new RangeError(`Semantic analysis cannot analyze ${iterableNode._tag}`)
      context.diagnostics.push(...iterable.diagnostics)
      const reject = (): void => {
        restoreStaticExpansion(checkpoint)
        context.staticIterations.push(
          Object.freeze({
            _tag: 'StaticIteration',
            iterable: iterable.fact,
            state: 'Rejected',
            scopes: Object.freeze([]),
            anchor: element.anchor,
          }),
        )
      }
      if (context.staticContext === undefined) {
        context.staticIterations.push(
          Object.freeze({
            _tag: 'StaticIteration',
            iterable: iterable.fact,
            state: 'Deferred',
            scopes: Object.freeze([]),
            anchor: element.anchor,
          }),
        )
        continue
      }
      if (iterable.diagnostics.some((diagnostic) => diagnostic.severity === 'error')) {
        reject()
        continue
      }
      const evaluated = StaticEvaluation.evaluateFact(iterable.fact, context.staticContext)
      if (evaluated._tag === 'Failed') {
        context.diagnostics.push(staticDiagnostic(evaluated.failure))
        reject()
        continue
      }
      const elements = staticIterationElements(evaluated.value)
      if (elements === undefined) {
        context.diagnostics.push(
          Diagnostic.staticPhaseViolation(
            'static for requires a finite static sequence or field collection',
            context.staticContext.environment.target,
            Object.freeze([]),
            context.context.spanOf(iterableNode.anchor),
          ),
        )
        reject()
        continue
      }
      const name: DeclarationFacts.DeclaredName = bindingName(context.context, element.binding)
      const body = element.body
      const scopes: Array<StaticIterationFact['scopes'][number]> = []
      let failed = false
      for (const [ordinal, current] of elements.entries()) {
        const iterationTrace = StaticEvaluation.appendTrace(
          context.staticContext.trace,
          StaticEvaluation.staticIterationFrame(
            ordinal,
            current.value,
            context.context.spanOf(element.anchor),
          ),
        )
        const iterationStaticContext = Object.freeze({
          ...context.staticContext,
          trace: iterationTrace,
        })
        const binding: BindingDeclarationFact = Object.freeze({
          _tag: 'BindingFact',
          id: Object.freeze({
            _tag: 'TirBinding',
            function: context.declaration.id,
            ordinal: context.nextBindingOrdinal.value,
          }),
          name,
          phase: 'Static',
          mutability: 'Immutable',
          inferredType: Object.freeze({ _tag: 'Available', type: current.type }),
          initializer: iterable.fact,
          staticValue: current.value,
          anchor: element.anchor,
        })
        context.nextBindingOrdinal.value += 1
        context.bindings.push(binding)
        context.staticContext.values.set(StaticEvaluation.localValueKey(binding), current.value)
        const diagnosticStart = context.diagnostics.length
        const nestedIterationStart = context.staticIterations.length
        const statements =
          body === undefined
            ? Object.freeze([])
            : analyzeStatements(
                Object.freeze({
                  ...context,
                  staticContext: iterationStaticContext,
                  resolution: Object.freeze({
                    ...context.resolution,
                    staticContext: iterationStaticContext,
                  }),
                }),
                body,
                Object.freeze({
                  parameters: scope.parameters,
                  bindings: Object.freeze([...scope.bindings, binding]),
                  patternBindings: scope.patternBindings,
                }),
                loopStack,
              )
        if (
          context.diagnostics
            .slice(diagnosticStart)
            .some((diagnostic) => diagnostic.severity === 'error')
        ) {
          failed = true
          break
        }
        const nestedIterations = Object.freeze(context.staticIterations.slice(nestedIterationStart))
        context.staticIterations.length = nestedIterationStart
        const residualNodes = Math.max(
          0,
          residualNodeCount(statements) - nestedStaticIterationNodeCount(nestedIterations),
        )
        const chargeFailure = context.staticContext.chargeStaticIteration?.(
          iterationTrace,
          residualNodes,
        )
        if (chargeFailure !== undefined) {
          context.diagnostics.push(staticDiagnostic(chargeFailure))
          failed = true
          break
        }
        if (context.staticContext.chargedStaticIterationNodes !== undefined)
          context.staticContext.chargedStaticIterationNodes.value += residualNodes
        scopes.push(
          Object.freeze({
            _tag: 'StaticIterationScope',
            ordinal,
            binding,
            statements: Object.freeze([...statements]),
            staticIterations: nestedIterations,
          }),
        )
      }
      if (failed) {
        reject()
        continue
      }
      const iteration: StaticIterationFact = Object.freeze({
        _tag: 'StaticIteration',
        iterable: iterable.fact,
        state: 'Expanded',
        scopes: Object.freeze(scopes),
        anchor: element.anchor,
      })
      context.staticIterations.push(iteration)
      for (const iterationScope of scopes) facts.push(...iterationScope.statements)
      continue
    }

    if (element._tag === 'StaticConditionalStatement') {
      if (context.staticContext === undefined) continue
      const conditionNode = element.condition
      const condition = analyzeExpression(
        context.context,
        conditionNode,
        context.declarations,
        context.declaration,
        scope,
        context.resolution,
        'bool',
      )
      if (condition === undefined)
        throw new RangeError(`Semantic analysis cannot analyze ${conditionNode._tag}`)
      context.diagnostics.push(...condition.diagnostics)
      const evaluated = StaticEvaluation.evaluateFact(condition.fact, context.staticContext)
      if (evaluated._tag === 'Failed') {
        context.diagnostics.push(staticDiagnostic(evaluated.failure))
        continue
      }
      if (evaluated.value._tag !== 'BooleanValue') {
        context.diagnostics.push(
          Diagnostic.staticPhaseViolation(
            'static if condition must evaluate to bool',
            context.staticContext.environment.target,
            Object.freeze([]),
            context.context.spanOf(conditionNode.anchor),
          ),
        )
        continue
      }
      // Both authored arms are present; selection picks one and the other is not elaborated.
      const selected = evaluated.value.value ? element.thenBranch : element.elseBranch
      if (selected !== undefined && selected._tag === 'Block') {
        const staticContext = Object.freeze({
          ...context.staticContext,
          trace: StaticEvaluation.appendTrace(
            context.staticContext.trace,
            StaticEvaluation.selectedArmFrame(
              evaluated.value.value ? 'Taken' : 'Otherwise',
              context.context.spanOf(selected.anchor),
            ),
          ),
        })
        facts.push(
          ...analyzeStatements(
            Object.freeze({
              ...context,
              staticContext,
              resolution: Object.freeze({ ...context.resolution, staticContext }),
            }),
            selected,
            scope,
            loopStack,
          ),
        )
      }
      continue
    }

    if (element._tag === 'ConditionalStatement') {
      facts.push(analyzeConditional(element, scope, loopStack))
      continue
    }

    if (element._tag === 'PatternConditionalStatement') {
      facts.push(analyzePatternConditional(element, scope, loopStack))
      continue
    }

    if (element._tag === 'AssignmentStatement') {
      const region = nextRegion()
      const destinationNode = element.target
      const valueNode = element.value
      const destination = analyzeExpression(
        context.context,
        destinationNode,
        context.declarations,
        context.declaration,
        scope,
        context.resolution,
      )
      if (destination === undefined) {
        throw new RangeError(`Semantic analysis cannot analyze ${destinationNode._tag}`)
      }
      context.diagnostics.push(...destination.diagnostics)
      const root = assignmentRoot(destination.fact)
      const activatedStart = context.resolution.bodyLifetimes?.activatedConstraints.length ?? 0
      const writeCompatibility =
        context.resolution.bodyLifetimes === undefined ||
        context.resolution.lifetimeCompatibility === undefined
          ? context.resolution.lifetimeCompatibility
          : BodyLifetime.activatedCompatibility(
              context.resolution.bodyLifetimes,
              context.resolution.lifetimeCompatibility,
              element.anchor,
              root?.anchor,
            )
      const writeResolution: ResolutionContext = {
        ...context.resolution,
        ...(writeCompatibility === undefined ? {} : { lifetimeCompatibility: writeCompatibility }),
      }
      const value = analyzeExpression(
        context.context,
        valueNode,
        context.declarations,
        context.declaration,
        scope,
        writeResolution,
        destination.type,
      )
      if (value === undefined) {
        throw new RangeError(`Semantic analysis cannot analyze ${valueNode._tag}`)
      }
      context.diagnostics.push(...value.diagnostics)
      if (destination.fact._tag === 'ForeignStatic') {
        context.diagnostics.push(
          Diagnostic.immutableAssignment(
            destination.fact.declaration.name._tag === 'Present'
              ? destination.fact.declaration.name.spelling
              : '?',
            context.context.spanOf(destinationNode.anchor),
          ),
        )
      } else if (root === undefined) {
        if (AuthoredWalk.isAvailable(destinationNode) && destination.diagnostics.length === 0) {
          context.diagnostics.push(
            Diagnostic.invalidAssignmentPlace(context.context.spanOf(destinationNode.anchor)),
          )
        }
      } else if (assignmentRootAccess(root, destination.fact) === 'ImmutableOwned') {
        context.diagnostics.push(
          Diagnostic.immutableAssignment(
            root.name._tag === 'Present' ? root.name.spelling : '?',
            context.context.spanOf(destinationNode.anchor),
          ),
        )
      } else if (
        assignmentRootAccess(root, destination.fact) === 'SharedBorrowed' ||
        (assignmentRootAccess(root, destination.fact) === 'ExclusiveBorrowed' &&
          destination.fact._tag !== 'IndexProjection' &&
          destination.fact._tag !== 'ReferentProjection' &&
          destination.fact._tag !== 'FieldProjection')
      ) {
        context.diagnostics.push(
          Diagnostic.invalidAssignmentPlace(context.context.spanOf(destinationNode.anchor)),
        )
      }
      const compatible =
        destination.type !== undefined &&
        value.type !== undefined &&
        typesCompatible(value.type, destination.type, writeCompatibility)
      if (destination.type !== undefined && value.type !== undefined && !compatible) {
        const expectedOrigin =
          root?._tag === 'BindingFact'
            ? context.context.spanOf(root.initializer.anchor)
            : context.context.spanOf(destinationNode.anchor)
        context.diagnostics.push(
          representationJoinDiagnostic(
            destination.type,
            value.type,
            expectedOrigin,
            context.context.spanOf(valueNode.anchor),
            context.context.spanOf(valueNode.anchor),
          ) ??
            unionConversionDiagnostic(
              value.type,
              destination.type,
              context.context.spanOf(valueNode.anchor),
              context.resolution?.lifetimeCompatibility,
            ) ??
            Diagnostic.assignmentTypeMismatch(
              Type.encode(destination.type),
              Type.encode(value.type),
              context.context.spanOf(valueNode.anchor),
            ),
        )
      }
      if (
        root?._tag === 'BindingFact' &&
        root.inferredType._tag === 'Available' &&
        Type.isCallable(root.inferredType.type)
      ) {
        // A callable binding keeps the exact representation it was initialized with; writing a
        // callable of another construction site would erase that identity behind the structural type.
        // Deferred writes inside an Effect body are governed by the captured-callable mutation rule.
        const identityOf = (expression: ExpressionFact): string | undefined => {
          const representation = representationOfExpression(context.context, expression)
          return representation !== undefined &&
            Type.isExactRepresentationArgument(representation) &&
            Type.isCallableIdentityArgument(representation.identity)
            ? Type.genericArgumentKey(representation.identity)
            : undefined
        }
        const current = identityOf(root.initializer)
        const written = identityOf(value.fact)
        if (
          compatible &&
          !context.effectBlock &&
          destination.fact._tag === 'Identifier' &&
          current !== undefined &&
          written !== undefined &&
          current !== written
        )
          context.diagnostics.push(
            Diagnostic.callableIdentityErasure(context.context.spanOf(valueNode.anchor)),
          )
        context.resolution.writtenCallableBindings?.add(root.id.ordinal)
      }
      facts.push(
        Object.freeze({
          _tag: 'WriteStatement',
          destination: destination.fact,
          ...(root === undefined ? {} : { root }),
          value: value.fact,
          compatible,
          lifetimeProof: Lifetime.assumptions(
            compatible
              ? (context.resolution.bodyLifetimes?.activatedConstraints
                  .slice(activatedStart)
                  .filter(
                    ({ installed }) =>
                      AuthoredIdentity.anchorKey(installed) ===
                      AuthoredIdentity.anchorKey(element.anchor),
                  )
                  .map(({ bound }) => bound) ?? [])
              : [],
          ).bounds,
          region,
          anchor: element.anchor,
        }),
      )
      continue
    }

    if (element._tag === 'WhileStatement') {
      const region = nextRegion()
      const loop = Object.freeze({
        _tag: 'TirLoop' as const,
        function: context.declaration.id,
        ordinal: context.loops.length,
      })
      context.loops.push(loop)
      const bodyNode = element.body
      const loopEntry = snapshotCallableWrites()
      const checkpoint = Object.freeze({
        bindings: context.bindings.length,
        diagnostics: context.diagnostics.length,
        regions: context.regions.length,
        loops: context.loops.length,
        nextBindingOrdinal: context.nextBindingOrdinal.value,
      })
      const analyzeLoopPass = (entry: ReadonlySet<number>) => {
        restoreCallableWrites(entry)
        const conditionNode = element.condition
        const condition = analyzeExpression(
          context.context,
          conditionNode,
          context.declarations,
          context.declaration,
          scope,
          ordinaryControlContext.resolution,
        )
        if (condition === undefined) {
          throw new RangeError(`Semantic analysis cannot analyze ${conditionNode._tag}`)
        }
        context.diagnostics.push(...condition.diagnostics)
        if (
          condition.fact.type._tag === 'Available' &&
          condition.fact.type.type !== 'bool' &&
          !Type.isNever(condition.fact.type.type)
        ) {
          context.diagnostics.push(
            Diagnostic.conditionNotBool(
              Type.encode(condition.fact.type.type),
              context.context.spanOf(conditionNode.anchor),
            ),
          )
        }
        const bodyEntry = snapshotCallableWrites()
        const body =
          bodyNode === undefined
            ? []
            : analyzeStatements(
                ordinaryControlContext,
                bodyNode,
                scope,
                Object.freeze([...loopStack, loop]),
              )
        const flow = callableWriteFlow(body, bodyEntry, loop)
        const backedge = unionWriteStates(flow.fallthrough, flow.continues)
        return Object.freeze({ condition: condition.fact, body, bodyEntry, flow, backedge })
      }
      let analyzed = analyzeLoopPass(loopEntry)
      const initialBackedge = analyzed.backedge
      if ([...(initialBackedge ?? [])].some((ordinal) => !loopEntry.has(ordinal))) {
        context.bindings.length = checkpoint.bindings
        context.diagnostics.length = checkpoint.diagnostics
        context.regions.length = checkpoint.regions
        context.loops.length = checkpoint.loops
        context.nextBindingOrdinal.value = checkpoint.nextBindingOrdinal
        const fixedEntry = new Set(loopEntry)
        for (const ordinal of initialBackedge ?? []) fixedEntry.add(ordinal)
        analyzed = analyzeLoopPass(fixedEntry)
      }
      mergeCallableWrites(
        analyzed.bodyEntry,
        ...(analyzed.backedge === undefined ? [] : [analyzed.backedge]),
        ...(analyzed.flow.breaks === undefined ? [] : [analyzed.flow.breaks]),
      )
      const parent = loopStack.at(-1)
      facts.push(
        Object.freeze({
          _tag: 'WhileStatement',
          loop,
          ...(parent === undefined ? {} : { parent }),
          condition: analyzed.condition,
          body: Object.freeze([...analyzed.body]),
          region,
          anchor: element.anchor,
        }),
      )
      continue
    }

    if (element._tag === 'BreakStatement' || element._tag === 'ContinueStatement') {
      const region = nextRegion()
      const target = loopStack.at(-1)
      if (target === undefined) {
        context.diagnostics.push(
          Diagnostic.transferOutsideLoop(
            element._tag === 'BreakStatement' ? 'break' : 'continue',
            context.context.spanOf(element.anchor),
          ),
        )
      }
      facts.push(
        Object.freeze({
          _tag: element._tag,
          ...(target === undefined ? {} : { target }),
          region,
          anchor: element.anchor,
        }),
      )
      continue
    }

    if (element._tag === 'ReturnStatement') {
      const region = nextRegion()
      // `return` without a value returns unit; the authored unit literal stands in for it.
      const expressionNode = element.value ?? unitValueOf(element)
      const expression = analyzeExpression(
        context.context,
        expressionNode,
        context.declarations,
        context.declaration,
        scope,
        context.resolution,
        !context.effectBlock ? context.returnType : undefined,
      )
      if (expression === undefined) {
        throw new RangeError(`Semantic analysis cannot analyze ${expressionNode._tag}`)
      }
      context.diagnostics.push(...expression.diagnostics)
      if (
        !context.effectBlock &&
        expression.type !== undefined &&
        Type.isCallable(expression.type) &&
        expression.type.mode !== 'Shared' &&
        !concreteCallableIdentity(expression.fact, context.resolution.writtenCallableBindings)
      ) {
        context.diagnostics.push(
          Diagnostic.unknownOwnedCallableReturn(context.context.spanOf(expressionNode.anchor)),
        )
      }
      facts.push(
        Object.freeze({
          _tag: 'ReturnStatement',
          expression: expression.fact,
          // A trailing block expression carries no `return` keyword, so lowering marks the
          // statement it synthesizes `implicit-return` instead of leaving a token to inspect.
          ...(element.origin._tag === 'Synthetic' && element.origin.role === 'implicit-return'
            ? { implicit: true as const }
            : {}),
          region,
          anchor: element.anchor,
        }),
      )
      break
    }

    if (element._tag === 'FailStatement') {
      const region = nextRegion()
      const expressionNode = element.value
      const expression = analyzeExpression(
        context.context,
        expressionNode,
        context.declarations,
        context.declaration,
        scope,
        context.resolution,
      )
      if (expression === undefined)
        throw new RangeError(`Semantic analysis cannot analyze ${expressionNode._tag}`)
      context.diagnostics.push(...expression.diagnostics)
      const failure =
        expression.type !== undefined && Type.isFailureValue(expression.type)
          ? expression.type
          : undefined
      if (!context.effectBlock && context.declaration.functionKind !== 'Effect')
        context.diagnostics.push(
          Diagnostic.failOutsideEffect(context.context.spanOf(element.anchor)),
        )
      if (expression.type !== undefined && failure === undefined)
        context.diagnostics.push(
          Diagnostic.invalidFailureType(
            Type.encode(expression.type),
            context.context.spanOf(expressionNode.anchor),
          ),
        )
      if (
        !context.effectBlock &&
        failure !== undefined &&
        !(Type.isUnion(failure) ? failure.members : [failure]).every((member) =>
          Type.isParameter(member)
            ? Type.failureMemberParameters(context.declaration.failureRow.row).some((parameter) =>
                Type.equals(parameter, member),
              )
            : context.declaration.failureRow.failures.some((candidate) =>
                typesCompatible(member, candidate, context.resolution.lifetimeCompatibility),
              ),
        )
      )
        context.diagnostics.push(
          Diagnostic.undeclaredFailure(
            Type.encode(failure),
            context.context.spanOf(expressionNode.anchor),
          ),
        )
      facts.push(
        Object.freeze({
          _tag: 'FailStatement',
          expression: expression.fact,
          ...(failure === undefined ? {} : { failure }),
          transfer: element.move ? 'Move' : 'Copy',
          region,
          anchor: element.anchor,
        }),
      )
      break
    }

    if (element._tag === 'DropStatement') {
      const region = nextRegion()
      const expressionNode = element.value
      const expression = analyzeExpression(
        context.context,
        expressionNode,
        context.declarations,
        context.declaration,
        scope,
        context.resolution,
      )
      if (expression === undefined)
        throw new RangeError(`Semantic analysis cannot analyze ${expressionNode._tag}`)
      context.diagnostics.push(...expression.diagnostics)
      facts.push(
        Object.freeze({
          _tag: 'DropStatement',
          expression: expression.fact,
          region,
          anchor: element.anchor,
        }),
      )
    }
  }

  return Object.freeze(facts)
}

export interface ReturnFlow {
  readonly fallsThrough: boolean
  readonly returns: ReadonlyArray<Extract<StatementFact, { readonly _tag: 'ReturnStatement' }>>
}

export const implicitReturn = (
  statement: Extract<StatementFact, { readonly _tag: 'ReturnStatement' }>,
): boolean => statement.implicit === true

/** Eager expression flow; constructing a callable or Effect never executes its body. */
export const expressionReturnFlow = (expression: ExpressionFact): ReturnFlow => {
  const returns: Array<Extract<StatementFact, { readonly _tag: 'ReturnStatement' }>> = []
  const visit = (child: ExpressionFact): boolean => {
    const flow = expressionReturnFlow(child)
    returns.push(...flow.returns)
    return flow.fallsThrough
  }
  let fallsThrough = true
  if (expression._tag === 'EffectBlock')
    return Object.freeze({ fallsThrough, returns: Object.freeze(returns) })
  if (expression._tag === 'Match') {
    fallsThrough = visit(expression.scrutinee)
    if (fallsThrough) {
      fallsThrough = false
      let remaining = [...expression.members]
      for (const arm of expression.arms) {
        const member = patternCoverage(arm.pattern)
        const selected = remaining.filter(
          (candidate) =>
            arm.pattern._tag === 'UniversalPattern' ||
            (member !== undefined && Match.selects(member, candidate)),
        )
        if (!arm.reachable || selected.length === 0) continue
        const guardCompletes = arm.guard === undefined || visit(arm.guard)
        remaining = remaining.filter((candidate) =>
          arm.after.some((member) => Match.identityEquals(candidate, member)),
        )
        if (!guardCompletes && arm.tests.length === 0)
          remaining = remaining.filter((candidate) => !selected.includes(candidate))
        if (!guardCompletes) continue
        if (arm.body._tag === 'Expression') {
          if (visit(arm.body.expression)) fallsThrough = true
        } else {
          const flow = returnFlowOf(arm.body.statements, false)
          returns.push(...flow.returns)
          if (flow.fallsThrough) fallsThrough = true
        }
      }
    }
  } else if (expression._tag === 'ShortCircuit') {
    const left = expression.arguments.at(0)?.expression
    fallsThrough = left === undefined || visit(left)
    const right = expression.arguments.at(1)?.expression
    if (fallsThrough && right !== undefined) visit(right)
  } else {
    for (const child of expressionChildren(expression)) {
      if (!visit(child)) {
        fallsThrough = false
        break
      }
    }
  }
  if (expression.type._tag === 'Available' && Type.isNever(expression.type.type))
    fallsThrough = false
  return Object.freeze({ fallsThrough, returns: Object.freeze(returns) })
}

export const expressionNever = (expression: ExpressionFact): boolean =>
  !expressionReturnFlow(expression).fallsThrough

/** Ordinary statements share eager operand transfers with their current execution boundary. */
export const returnFlowOf = (
  body: ReadonlyArray<StatementFact>,
  implicitReturnFallsThrough = true,
): ReturnFlow => {
  const returns: Array<Extract<StatementFact, { readonly _tag: 'ReturnStatement' }>> = []
  let fallsThrough = true
  for (const statement of body) {
    if (!fallsThrough) break
    for (const expression of directStatementExpressions(statement)) {
      const flow = expressionReturnFlow(expression)
      returns.push(...flow.returns)
      if (!flow.fallsThrough) {
        fallsThrough = false
        break
      }
    }
    if (!fallsThrough) break
    if (statement._tag === 'ReturnStatement') {
      if (implicitReturn(statement)) {
        fallsThrough = implicitReturnFallsThrough
        if (!implicitReturnFallsThrough) returns.push(statement)
      } else {
        returns.push(statement)
        fallsThrough = false
      }
    } else if (
      statement._tag === 'FailStatement' ||
      statement._tag === 'BreakStatement' ||
      statement._tag === 'ContinueStatement'
    ) {
      fallsThrough = false
    } else if (statement._tag === 'UnsafeStatement') {
      const nested = returnFlowOf(statement.statements, implicitReturnFallsThrough)
      returns.push(...nested.returns)
      fallsThrough = nested.fallsThrough
    } else if (statement._tag === 'IfStatement' || statement._tag === 'IfLetStatement') {
      const taken = returnFlowOf(statement.taken, implicitReturnFallsThrough)
      const otherwise = returnFlowOf(statement.otherwise, implicitReturnFallsThrough)
      returns.push(...taken.returns, ...otherwise.returns)
      fallsThrough = taken.fallsThrough || otherwise.fallsThrough
    } else if (statement._tag === 'WhileStatement') {
      returns.push(...returnFlowOf(statement.body, implicitReturnFallsThrough).returns)
    }
  }
  return Object.freeze({ fallsThrough, returns: Object.freeze(returns) })
}

/** Keeps only statements that can execute, treating an implicit unit completion as a real return. */
export const executableStatements = (
  body: ReadonlyArray<StatementFact>,
): ReadonlyArray<StatementFact> => {
  const reachable: Array<StatementFact> = []
  for (const statement of body) {
    reachable.push(statement)
    if (!returnFlowOf([statement], false).fallsThrough) break
  }
  return Object.freeze(reachable)
}

/** Callable binding roots written on any reachable path through these already-analyzed facts. */
export const reachableCallableWrites = (
  body: ReadonlyArray<StatementFact>,
): ReadonlySet<number> => {
  const writes = new Set<number>()
  const expression = (fact: ExpressionFact): boolean => {
    if (fact._tag === 'EffectBlock') return true
    if (fact._tag === 'ShortCircuit') {
      const left = fact.arguments.at(0)?.expression
      if (left !== undefined && !expression(left)) return false
      const right = fact.arguments.at(1)?.expression
      if (right !== undefined) expression(right)
      return !expressionNever(fact)
    }
    if (fact._tag === 'Match') {
      if (!expression(fact.scrutinee)) return false
      let completes = false
      for (const arm of fact.arms) {
        if (!arm.reachable) continue
        if (arm.guard !== undefined && !expression(arm.guard)) continue
        if (
          arm.body._tag === 'Expression'
            ? expression(arm.body.expression)
            : visit(arm.body.statements)
        )
          completes = true
      }
      return completes
    }
    for (const child of expressionChildren(fact)) if (!expression(child)) return false
    if (
      fact._tag === 'PlaceReplace' &&
      fact.root?._tag === 'BindingFact' &&
      fact.root.inferredType._tag === 'Available' &&
      Type.isCallable(fact.root.inferredType.type)
    )
      writes.add(fact.root.id.ordinal)
    return !expressionNever(fact)
  }
  const visit = (statements: ReadonlyArray<StatementFact>): boolean => {
    let fallsThrough = true
    for (const statement of statements) {
      if (!fallsThrough) break
      if (statement._tag === 'BindStatement') {
        fallsThrough = expression(statement.binding.initializer)
      } else if (statement._tag === 'PatternBindStatement') {
        fallsThrough = expression(statement.selection.source)
      } else if (statement._tag === 'ExpressionStatement' || statement._tag === 'DropStatement') {
        fallsThrough = expression(statement.expression)
      } else if (statement._tag === 'WriteStatement') {
        const destinationContinues = expression(statement.destination)
        if (!destinationContinues) {
          fallsThrough = false
          continue
        }
        const valueContinues = expression(statement.value)
        if (
          valueContinues &&
          statement.root?._tag === 'BindingFact' &&
          statement.root.inferredType._tag === 'Available' &&
          Type.isCallable(statement.root.inferredType.type)
        )
          writes.add(statement.root.id.ordinal)
        fallsThrough = valueContinues
      } else if (statement._tag === 'ReturnStatement' || statement._tag === 'FailStatement') {
        expression(statement.expression)
        fallsThrough = false
      } else if (statement._tag === 'BreakStatement' || statement._tag === 'ContinueStatement') {
        fallsThrough = false
      } else if (statement._tag === 'UnsafeStatement') {
        fallsThrough = visit(statement.statements)
      } else if (statement._tag === 'IfStatement' || statement._tag === 'IfLetStatement') {
        const condition =
          statement._tag === 'IfStatement' ? statement.condition : statement.selection.source
        if (!expression(condition)) {
          fallsThrough = false
        } else {
          const takenFallsThrough = visit(statement.taken)
          const otherwiseFallsThrough = visit(statement.otherwise)
          fallsThrough = takenFallsThrough || otherwiseFallsThrough
        }
      } else {
        fallsThrough = expression(statement.condition)
        if (fallsThrough) visit(statement.body)
      }
    }
    return fallsThrough
  }
  visit(body)
  return writes
}


/**
 * Where a body that falls off its end is reported: the closing brace, which a presented block
 * span always ends with unless recovery left the block damaged.
 */
const closingBraceSpan = (
  semantic: SemanticContext.SemanticContext,
  block: AuthoredHir.Block,
): SourceSpan.SourceSpan => {
  const span = semantic.spanOf(block.anchor)
  if (block.causes.length > 0 || span.end <= span.start) return span
  return SourceSpan.fromOffsets(span.sourceId, span.end - 1, span.end) ?? span
}
/**
 * Joins the Effects constructed at distinct return sites of one function into one finite
 * composite representation under the declared contract (EFF-013). A single construction site
 * needs no composite; sites without an exact static representation cannot join and report
 * `SEM0132`.
 */
const returnSiteEffectJoin = (
  context: BodyContext,
  declaration: DeclarationFact,
  returns: ReadonlyArray<Extract<StatementFact, { readonly _tag: 'ReturnStatement' }>>,
): Type.Type | undefined => {
  if (declaration.returnType._tag !== 'Resolved') return undefined
  const declared = declaration.returnType.type
  if (!Type.isEffect(declared) || returns.length < 2) return undefined
  const sites = returns.filter(
    (statement) =>
      statement.expression.type._tag === 'Available' &&
      !Type.isNever(statement.expression.type.type),
  )
  const alternatives: Array<Type.ExactRepresentationArgument> = []
  const missing: Array<SourceSpan.SourceSpan> = []
  for (const statement of sites) {
    const representation = representationOfExpression(context.context, statement.expression)
    if (
      representation !== undefined &&
      Type.isExactRepresentationArgument(representation) &&
      Type.isEffectIdentityArgument(representation.identity)
    )
      alternatives.push(representation)
    else if (
      representation !== undefined &&
      Type.isCompositeEffectRepresentationArgument(representation)
    )
      alternatives.push(...representation.alternatives)
    else missing.push(context.context.spanOf(statement.expression.anchor))
  }
  const composite = Type.compositeEffectRepresentationArgument(declared, alternatives)
  if (composite.alternatives.length < 2 && missing.length === 0) return undefined
  if (missing.length > 0) {
    if (composite.alternatives.length === 0) return undefined
    for (const span of missing)
      context.diagnostics.push(
        Diagnostic.nonFiniteEffectJoin(
          'every reachable return site must retain one exact static Effect representation',
          span,
        ),
      )
    return undefined
  }
  return Type.represented(declared, declared, composite)
}

export const analyzeFunctionBody = (
  semantic: SemanticContext.SemanticContext,
  declaration: DeclarationFact,
  declarations: ReadonlyArray<DeclarationFact>,
  resolution: ResolutionContext,
  staticContext?: BodyContext['staticContext'],
  initialScope?: Scope,
): FunctionAnalysis => {
  const returnType =
    declaration.returnType._tag === 'Resolved'
      ? Type.substitute(declaration.returnType.type, staticContext?.typeSubstitution ?? new Map())
      : undefined
  const blockNode = AuthoredWalk.bodyBlock(semantic, declaration)
  const unsafeSpans = Object.freeze(
    AuthoredWalk.statements(blockNode)
      .filter((statement) => statement._tag === 'UnsafeStatement')
      .map((statement) => semantic.spanOf(statement.anchor)),
  )
  const nextBindingOrdinal = { value: 0 }
  const declaredOutlives = TypeOutlives.context(resolution.index.modules)
  const outlivesScope =
    resolution.anonymousDepth === 1
      ? TypeOutlives.withInputs(
          declaredOutlives,
          declaration.parameters.flatMap((parameter) =>
            parameter.declaredType._tag === 'Resolved' ? [parameter.declaredType.type] : [],
          ),
        )
      : declaredOutlives
  const bodyLifetimes = BodyLifetime.make(
    declaration.canonical._tag === 'Canonical'
      ? declaration.canonical.id
      : {
          module: AuthoredWalk.moduleName(semantic),
          name: `#${declaration.id.ordinal}`,
        },
    AuthoredWalk.anchors(blockNode),
    outlivesScope.parameterBounds,
  )
  const authoredDeclaration = AuthoredWalk.declarationOf(semantic.module, declaration.owner)
  const bodyResolution: ResolutionContext = Object.freeze({
    ...resolution,
    ...(authoredDeclaration === undefined ? {} : { authoredDeclaration }),
    unsafeSpans,
    nextBindingOrdinal,
    executableFunction: declaration.id,
    ...(declaration.canonical._tag === 'Canonical'
      ? { executableOwner: declaration.canonical.id }
      : {}),
    executableSites: executableSites(blockNode),
    bodyLifetimes,
    lifetimeCompatibility: BodyLifetime.compatibility(
      bodyLifetimes,
      Lifetime.mergeAssumptions(
        Lifetime.assumptions(
          DeclarationFacts.executableLifetimes(declaration).lifetimeBounds ?? [],
        ),
        outlivesScope.assumptions,
      ),
      NominalVariance.derive(resolution.index).summaries,
    ),
    writtenCallableBindings: new Set<number>(),
    generatedAggregates: new Map(),
    ...(staticContext === undefined ? {} : { staticContext }),
  })
  const context: BodyContext = {
    context: semantic,
    declaration,
    declarations,
    bindings: [],
    diagnostics: [],
    regions: [],
    loops: [],
    staticIterations: [],
    resolution: bodyResolution,
    nextBindingOrdinal,
    ...(staticContext === undefined ? {} : { staticContext }),
    ...(returnType === undefined ? {} : { returnType }),
  }
  context.diagnostics.push(...MachineFunction.bodyDiagnostics(semantic, declaration))
  if (declaration.phase === 'Runtime') {
    const target = staticContext?.environment.target ?? 'unselected-target'
    for (const parameter of declaration.parameters) {
      if (
        parameter.phase === 'Runtime' &&
        parameter.declaredType._tag === 'Resolved' &&
        Type.containsStaticPhaseOnly(parameter.declaredType.type)
      )
        context.diagnostics.push(
          Diagnostic.staticPhaseViolation(
            'runtime parameter with a phase-only type',
            target,
            Object.freeze([]),
            context.context.spanOf(parameter.anchor),
          ),
        )
    }
    if (
      declaration.returnType._tag === 'Resolved' &&
      Type.containsStaticPhaseOnly(declaration.returnType.type)
    )
      context.diagnostics.push(
        Diagnostic.staticPhaseViolation(
          'runtime return with a phase-only type',
          target,
          Object.freeze([]),
          context.context.spanOf(declaration.returnType.anchor),
        ),
      )
  }
  const statements = analyzeStatements(
    context,
    blockNode,
    initialScope ??
      Object.freeze({ parameters: declaration.parameters, bindings: [], patternBindings: [] }),
  )
  const hasDeferredStaticControl =
    staticContext === undefined &&
    AuthoredWalk.statements(blockNode).some(
      (statement) =>
        statement._tag === 'StaticConditionalStatement' || statement._tag === 'StaticForStatement',
    )
  type Terminal = Extract<StatementFact, { _tag: 'ReturnStatement' | 'FailStatement' }>
  const terminalOf = (body: ReadonlyArray<StatementFact>): Terminal | undefined => {
    for (const statement of [...body].reverse()) {
      if (statement._tag === 'ReturnStatement' || statement._tag === 'FailStatement')
        return statement
      if (statement._tag === 'UnsafeStatement') {
        const nested = terminalOf(statement.statements)
        if (nested !== undefined) return nested
      } else if (statement._tag === 'IfStatement' || statement._tag === 'IfLetStatement') {
        const nested = terminalOf(statement.otherwise) ?? terminalOf(statement.taken)
        if (nested !== undefined) return nested
      }
    }
    return undefined
  }
  const terminal = terminalOf(statements)
  if (terminal === undefined)
    throw new RangeError('Semantic analysis expected a terminal statement')
  const expression = terminal.expression
  const returnFlow = returnFlowOf(statements)
  const staticEvaluationFailureCodes = new Set<Diagnostic.Code>([
    Diagnostic.staticPhaseViolationCode,
    Diagnostic.selectedCompileErrorCode,
    Diagnostic.staticEvaluationCycleCode,
    Diagnostic.staticStepLimitCode,
    Diagnostic.staticCallDepthLimitCode,
    Diagnostic.staticRetainedValueLimitCode,
    Diagnostic.staticResidualGrowthLimitCode,
  ])
  const hasStaticEvaluationFailure = context.diagnostics.some((diagnostic) =>
    staticEvaluationFailureCodes.has(diagnostic.code),
  )
  let validReturnContract = declaration.returnType._tag === 'Resolved'
  if (declaration.returnType._tag === 'Resolved') {
    for (const returned of returnFlow.returns) {
      if (returned.expression.type._tag !== 'Available') {
        validReturnContract = false
        continue
      }
      const actual = returned.expression.type.type
      if (
        staticContext?.typeSubstitution === undefined
          ? declaredReturnTypesCompatible(
              semantic,
              declaration,
              returned.expression,
              bodyResolution?.lifetimeCompatibility,
            )
          : typesCompatible(
              actual,
              returnType ?? declaration.returnType.type,
              bodyResolution?.lifetimeCompatibility,
            )
      )
        continue
      validReturnContract = false
      context.diagnostics.push(
        representationJoinDiagnostic(
          returnType ?? declaration.returnType.type,
          actual,
          context.context.spanOf(declaration.returnType.anchor),
          context.context.spanOf(returned.expression.anchor),
          context.context.spanOf(returned.expression.anchor),
        ) ??
          unionConversionDiagnostic(
            actual,
            returnType ?? declaration.returnType.type,
            context.context.spanOf(returned.expression.anchor),
            bodyResolution?.lifetimeCompatibility,
          ) ??
          Diagnostic.returnTypeMismatch(
            Type.encode(returnType ?? declaration.returnType.type),
            Type.encode(actual),
            context.context.spanOf(returned.expression.anchor),
          ),
      )
    }
    if (
      returnFlow.fallsThrough &&
      !Type.equals(returnType ?? declaration.returnType.type, Type.unit) &&
      !hasDeferredStaticControl &&
      !hasStaticEvaluationFailure
    ) {
      validReturnContract = false
      context.diagnostics.push(
        Diagnostic.missingReturn(
          Type.encode(returnType ?? declaration.returnType.type),
          closingBraceSpan(semantic, blockNode),
        ),
      )
    }
  }
  const returnCompatibility = validReturnContract ? compatible : unavailableCompatibility
  const resultRepresentation = validReturnContract
    ? returnSiteEffectJoin(context, declaration, returnFlow.returns)
    : undefined
  const lifetimeFlow = LifetimeFlow.analyze(
    declaration,
    statements,
    bodyLifetimes,
    resolution.index,
    semantic,
    outlivesScope,
  )
  context.diagnostics.push(...lifetimeFlow.diagnostics)
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'FunctionFact',
      ...(bodyResolution.lifetimeCompatibility === undefined
        ? {}
        : { comparisonWork: Object.freeze({ ...bodyResolution.lifetimeCompatibility.work }) }),
      lifetimeFlow,
      declaration,
      statements,
      bindings: Object.freeze([...context.bindings]),
      regionOrder: Object.freeze([...context.regions]),
      returnedExpression: expression,
      returnCompatibility,
      ...(resultRepresentation === undefined ? {} : { resultRepresentation }),
      generatedAggregates: Object.freeze([...(bodyResolution.generatedAggregates?.values() ?? [])]),
      staticIterations: Object.freeze([...context.staticIterations]),
    }),
    diagnostics: Object.freeze([...context.diagnostics]),
  })
}
