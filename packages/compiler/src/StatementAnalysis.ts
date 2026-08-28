import { concreteCallableIdentity, exactCallableOf, executableSites } from './CallResolution.js'
import * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import type {
  BindingDeclarationFact,
  DeclarationFact,
  ExpressionFact,
  PatternSelectionFact,
  StatementFact,
} from './Elaboration.js'
import {
  assignmentRoot,
  assignmentRootAccess,
  childNode,
  compatible,
  declaredReturnTypesCompatible,
  expressionChildren,
  isExpressionNode,
  representationJoinDiagnostic,
  returnedBorrowArgument,
  returnedBorrowExpression,
  typesCompatible,
  unavailableCompatibility,
  unionConversionDiagnostic,
  visitExpressionFacts,
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
  bindingName,
  coverageMembersOf,
  enumFactByType,
  statementExpressionNode,
  unsafeCallAuthorized,
} from './ExpressionAnalysis.js'
import type * as Hir from './Hir.js'
import * as Match from './Match.js'
import * as Presentation from './Presentation.js'
import type * as SourceFile from './SourceFile.js'
import type * as SourceSpan from './SourceSpan.js'
import * as SyntaxTree from './SyntaxTree.js'
import * as Type from './Type.js'
export const unsafeCallDiagnostic = (
  unsafe: boolean,
  spelling: string,
  call: SyntaxTree.Node,
  resolution: ResolutionContext | undefined,
): Diagnostic.Diagnostic | undefined =>
  unsafe && !unsafeCallAuthorized(resolution, call)
    ? Diagnostic.missingUnsafeBoundary(spelling, call.span)
    : undefined

/** Whether a borrow-shaped value is visibly backed only by program-lifetime immutable data. */
export const isStaticallyDetachedFailure = (
  expression: ExpressionFact,
  index: DeclarationIndex.Index,
): boolean => {
  if (
    expression.type._tag === 'Available' &&
    !DeclarationFacts.containsLexicalBorrow(index, expression.type.type)
  )
    return true
  switch (expression._tag) {
    case 'StaticText':
      return expression.data !== undefined
    case 'Constant':
      return expression.value?._tag === 'String'
    case 'Grouped':
      return isStaticallyDetachedFailure(expression.expression, index)
    case 'Move':
      return isStaticallyDetachedFailure(expression.subject, index)
    case 'StructLiteral':
      return expression.fields.every((field) =>
        isStaticallyDetachedFailure(field.initializer.expression, index),
      )
    case 'ArrayLiteral':
      return expression.elements.every((element) =>
        isStaticallyDetachedFailure(element.expression, index),
      )
    default:
      return false
  }
}

export const analyzeStatements = (
  context: BodyContext,
  blockNode: SyntaxTree.Node,
  initialScope: Scope,
  loopStack: ReadonlyArray<Hir.LoopId> = Object.freeze([]),
): ReadonlyArray<StatementFact> => {
  const facts: Array<StatementFact> = []
  let scope = initialScope
  const blockBindings = new Map<string, SourceSpan.SourceSpan>()
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
    targetLoop: Hir.LoopId,
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

  const nextRegion = (): Hir.RegionId => {
    const region = Object.freeze({
      _tag: 'HirRegion' as const,
      function: context.declaration.id,
      ordinal: (context.regionBase ?? 0) + context.regions.length,
    })
    context.regions.push(region)
    return region
  }

  const analyzePatternSelection = (
    element: SyntaxTree.Node,
    selectionScope: Scope,
  ): PatternSelectionFact => {
    const initializerNode = statementExpressionNode(element)
    const initializer = analyzeExpression(
      context.source,
      initializerNode,
      context.declarations,
      context.declaration,
      selectionScope,
      context.resolution,
      undefined,
      true,
    )
    if (initializer === undefined) {
      throw new RangeError(`Semantic analysis cannot analyze ${initializerNode.kind}`)
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
      span: element.span,
    })
    const arm: Match.ArmId = Object.freeze({ _tag: 'MatchArmId', match: id, ordinal: 0 })
    const patternNode =
      SyntaxTree.directNode(element, 'ErrorPattern') ??
      SyntaxTree.directNode(element, 'EnumMemberPattern') ??
      SyntaxTree.directNode(element, 'IntegerPattern') ??
      SyntaxTree.directNode(element, 'UnionVariantPattern') ??
      SyntaxTree.directNode(element, 'NominalPattern') ??
      SyntaxTree.directNode(element, 'BindingPattern') ??
      SyntaxTree.directNode(element, 'UniversalPattern')
    if (patternNode === undefined) throw new RangeError('Pattern statement requires a pattern')
    const pattern = analyzePattern(
      context.source,
      patternNode,
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
          pattern.fact.syntax.span,
        ),
      )
    }
    const coverage = Match.cover(
      members,
      Object.freeze([
        Object.freeze({
          ...(member === undefined ? {} : { member }),
          universal: pattern.fact._tag === 'UniversalPattern',
          guarded: false,
        }),
      ]),
    )
    const complete = pattern.fact._tag === 'UniversalPattern' ? true : pattern.fact.complete
    return Object.freeze({
      _tag: 'PatternSelection',
      id,
      arm,
      access,
      source: initializer.fact,
      subject,
      members: Object.freeze(members),
      pattern: pattern.fact,
      bindings: pattern.fact.bindings,
      irrefutable: coverage.exhaustive && complete,
      loanEnd: element.kind === 'PatternBindingStatement' ? blockNode.span : element.span,
      syntax: element,
    })
  }

  const analyzeConditional = (
    element: SyntaxTree.Node,
    armScope: Scope,
    armLoopStack: ReadonlyArray<Hir.LoopId>,
  ): StatementFact => {
    const region = nextRegion()
    const conditionNode = statementExpressionNode(element)
    const condition = analyzeExpression(
      context.source,
      conditionNode,
      context.declarations,
      context.declaration,
      armScope,
      context.resolution,
    )
    if (condition === undefined) {
      throw new RangeError(`Semantic analysis cannot analyze ${conditionNode.kind}`)
    }
    context.diagnostics.push(...condition.diagnostics)
    if (condition.fact.type._tag === 'Available' && condition.fact.type.type !== 'bool') {
      context.diagnostics.push(
        Diagnostic.conditionNotBool(
          Type.encode(condition.fact.type.type),
          condition.fact.syntax.span,
        ),
      )
    }

    const arms = SyntaxTree.directNodes(element, 'Block')
    const firstArm = arms.at(0)
    const branchEntry = snapshotCallableWrites()
    const taken = analyzePath(branchEntry, () =>
      firstArm === undefined ? [] : analyzeStatements(context, firstArm, armScope, armLoopStack),
    )
    const chained = SyntaxTree.directNode(element, 'ConditionalStatement')
    const otherwiseArm = arms.at(1)
    const otherwise = analyzePath(branchEntry, () => {
      if (chained !== undefined) {
        return [analyzeConditional(chained, armScope, armLoopStack)]
      }
      if (otherwiseArm === undefined) {
        return []
      }
      return analyzeStatements(context, otherwiseArm, armScope, armLoopStack)
    })
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
      syntax: element,
    })
  }

  const analyzePatternConditional = (
    element: SyntaxTree.Node,
    armScope: Scope,
    armLoopStack: ReadonlyArray<Hir.LoopId>,
  ): StatementFact => {
    const region = nextRegion()
    const selection = analyzePatternSelection(element, armScope)
    const takenScope: Scope = Object.freeze({
      parameters: armScope.parameters,
      bindings: armScope.bindings,
      patternBindings: Object.freeze([...armScope.patternBindings, ...selection.bindings]),
    })
    const arms = SyntaxTree.directNodes(element, 'Block')
    const firstArm = arms.at(0)
    const branchEntry = snapshotCallableWrites()
    const taken = analyzePath(branchEntry, () =>
      firstArm === undefined ? [] : analyzeStatements(context, firstArm, takenScope, armLoopStack),
    )
    const chained =
      SyntaxTree.directNode(element, 'ConditionalStatement') ??
      SyntaxTree.directNode(element, 'PatternConditionalStatement')
    const otherwiseArm = arms.at(1)
    const otherwise = analyzePath(branchEntry, () => {
      if (chained?.kind === 'PatternConditionalStatement') {
        return [analyzePatternConditional(chained, armScope, armLoopStack)]
      }
      if (chained !== undefined) {
        return [analyzeConditional(chained, armScope, armLoopStack)]
      }
      if (otherwiseArm === undefined) {
        return []
      }
      return analyzeStatements(context, otherwiseArm, armScope, armLoopStack)
    })
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
      syntax: element,
    })
  }

  for (const element of blockNode.children) {
    if (!SyntaxTree.isNode(element)) continue

    if (element.kind === 'UnsafeStatement') {
      const region = nextRegion()
      const body = SyntaxTree.directNode(element, 'Block')
      const statements =
        body === undefined
          ? Object.freeze<StatementFact[]>([])
          : analyzeStatements(context, body, scope, loopStack)
      facts.push(
        Object.freeze({
          _tag: 'UnsafeStatement',
          statements,
          region,
          syntax: element,
        }),
      )
      continue
    }

    if (element.kind === 'BindingStatement') {
      const region = nextRegion()
      const bindingOrdinal = context.nextBindingOrdinal.value
      context.nextBindingOrdinal.value += 1
      const initializerNode = statementExpressionNode(element)
      const initializer = analyzeExpression(
        context.source,
        initializerNode,
        context.declarations,
        context.declaration,
        scope,
        context.resolution,
        undefined,
        true,
      )
      if (initializer === undefined) {
        throw new RangeError(`Semantic analysis cannot analyze ${initializerNode.kind}`)
      }
      context.diagnostics.push(...initializer.diagnostics)

      if (
        SyntaxTree.directToken(element, 'MutKeyword') !== undefined &&
        initializer.type !== undefined &&
        Type.isEffect(initializer.type)
      )
        context.diagnostics.push(Diagnostic.mutableEffectRecipe(element.span))

      const name = bindingName(context.source, element)
      const exactCallable = exactCallableOf(
        initializer.fact,
        context.resolution.writtenCallableBindings,
      )
      const hasConcreteCallableIdentity = concreteCallableIdentity(
        initializer.fact,
        context.resolution.writtenCallableBindings,
      )
      const binding: BindingDeclarationFact = Object.freeze({
        _tag: 'BindingFact',
        id: Object.freeze({
          _tag: 'HirBinding',
          function: context.declaration.id,
          ordinal: bindingOrdinal,
        }),
        name,
        mutability:
          SyntaxTree.directToken(element, 'MutKeyword') === undefined ? 'Immutable' : 'Mutable',
        inferredType: initializer.fact.type,
        initializer: initializer.fact,
        ...(exactCallable === undefined ? {} : { exactCallable }),
        ...(hasConcreteCallableIdentity ? { concreteCallableIdentity: true as const } : {}),
        syntax: element,
      })
      context.bindings.push(binding)
      facts.push(Object.freeze({ _tag: 'BindStatement', binding, region }))

      if (name._tag === 'Present') {
        const originalSpan = blockBindings.get(name.spelling)
        if (originalSpan === undefined) {
          blockBindings.set(name.spelling, name.token.span)
          scope = Object.freeze({
            parameters: scope.parameters,
            bindings: Object.freeze([...scope.bindings, binding]),
            patternBindings: scope.patternBindings,
          })
        } else {
          context.diagnostics.push(
            Diagnostic.rebindingName(name.spelling, originalSpan, name.token.span),
          )
        }
      }
      continue
    }

    if (element.kind === 'PatternBindingStatement') {
      const region = nextRegion()
      const selection = analyzePatternSelection(element, scope)
      if (selection.pattern._tag === 'UniversalPattern') {
        if (selection.subject.type._tag === 'Available')
          context.diagnostics.push(
            Diagnostic.expressionStatementResult(
              Presentation.type(
                selection.subject.type.type,
                context.source.id,
                context.resolution.scope,
              ),
              selection.pattern.syntax.span,
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
              ? Presentation.type(
                  selection.subject.type.type,
                  context.source.id,
                  context.resolution.scope,
                )
              : '<unavailable>',
            selection.members
              .filter((member) => selected === undefined || !Match.selects(selected, member))
              .map(Match.encodeIdentity),
            selection.pattern.syntax.span,
          ),
        )
      }
      facts.push(
        Object.freeze({
          _tag: 'PatternBindStatement',
          selection,
          region,
          syntax: element,
        }),
      )
      for (const binding of selection.bindings) {
        if (binding.name._tag !== 'Present') continue
        const originalSpan = blockBindings.get(binding.name.spelling)
        if (originalSpan === undefined)
          blockBindings.set(binding.name.spelling, binding.name.token.span)
      }
      scope = Object.freeze({
        parameters: scope.parameters,
        bindings: scope.bindings,
        patternBindings: Object.freeze([...scope.patternBindings, ...selection.bindings]),
      })
      continue
    }

    if (element.kind === 'ExpressionStatement') {
      const region = nextRegion()
      const expressionNode = statementExpressionNode(element)
      const expression = analyzeExpression(
        context.source,
        expressionNode,
        context.declarations,
        context.declaration,
        scope,
        context.resolution,
      )
      if (expression === undefined) {
        throw new RangeError(`Semantic analysis cannot analyze ${expressionNode.kind}`)
      }
      context.diagnostics.push(...expression.diagnostics)
      if (
        expression.type !== undefined &&
        !Type.equals(expression.type, Type.unit) &&
        !Type.isNever(expression.type)
      ) {
        context.diagnostics.push(
          Diagnostic.expressionStatementResult(
            Presentation.type(expression.type, context.source.id, context.resolution.scope),
            expressionNode.span,
          ),
        )
      }
      facts.push(
        Object.freeze({
          _tag: 'ExpressionStatement',
          expression: expression.fact,
          region,
          syntax: element,
        }),
      )
      continue
    }

    if (element.kind === 'ConditionalStatement') {
      facts.push(analyzeConditional(element, scope, loopStack))
      continue
    }

    if (element.kind === 'PatternConditionalStatement') {
      facts.push(analyzePatternConditional(element, scope, loopStack))
      continue
    }

    if (element.kind === 'AssignmentStatement') {
      const region = nextRegion()
      const nodes = element.children.filter(
        (child): child is SyntaxTree.Node => SyntaxTree.isNode(child) && isExpressionNode(child),
      )
      const destinationNode = nodes.at(0)
      const valueNode = nodes.at(1)
      if (destinationNode === undefined || valueNode === undefined) {
        context.diagnostics.push(Diagnostic.invalidAssignmentPlace(element.span))
        continue
      }
      const destination = analyzeExpression(
        context.source,
        destinationNode,
        context.declarations,
        context.declaration,
        scope,
        context.resolution,
      )
      if (destination === undefined) {
        throw new RangeError(`Semantic analysis cannot analyze ${destinationNode.kind}`)
      }
      context.diagnostics.push(...destination.diagnostics)
      const value = analyzeExpression(
        context.source,
        valueNode,
        context.declarations,
        context.declaration,
        scope,
        context.resolution,
        destination.type,
      )
      if (value === undefined) {
        throw new RangeError(`Semantic analysis cannot analyze ${valueNode.kind}`)
      }
      context.diagnostics.push(...value.diagnostics)
      const root = assignmentRoot(destination.fact)
      if (root === undefined) {
        if (SyntaxTree.isAvailableSyntax(destinationNode) && destination.diagnostics.length === 0) {
          context.diagnostics.push(Diagnostic.invalidAssignmentPlace(destinationNode.span))
        }
      } else if (assignmentRootAccess(root) === 'ImmutableOwned') {
        context.diagnostics.push(
          Diagnostic.immutableAssignment(
            root.name._tag === 'Present' ? root.name.spelling : '?',
            destinationNode.span,
          ),
        )
      } else if (
        assignmentRootAccess(root) === 'SharedBorrowed' ||
        (assignmentRootAccess(root) === 'ExclusiveBorrowed' &&
          destination.fact._tag !== 'IndexProjection' &&
          destination.fact._tag !== 'FieldProjection')
      ) {
        context.diagnostics.push(Diagnostic.invalidAssignmentPlace(destinationNode.span))
      }
      const compatible =
        destination.type !== undefined &&
        value.type !== undefined &&
        typesCompatible(value.type, destination.type)
      if (destination.type !== undefined && value.type !== undefined && !compatible) {
        const expectedOrigin =
          root?._tag === 'BindingFact' ? root.initializer.syntax.span : destinationNode.span
        context.diagnostics.push(
          representationJoinDiagnostic(
            destination.type,
            value.type,
            expectedOrigin,
            valueNode.span,
            valueNode.span,
          ) ??
            unionConversionDiagnostic(value.type, destination.type, valueNode.span) ??
            Diagnostic.assignmentTypeMismatch(
              Type.encode(destination.type),
              Type.encode(value.type),
              valueNode.span,
            ),
        )
      }
      if (
        root?._tag === 'BindingFact' &&
        root.inferredType._tag === 'Available' &&
        Type.isCallable(root.inferredType.type)
      ) {
        context.resolution.writtenCallableBindings?.add(root.id.ordinal)
      }
      facts.push(
        Object.freeze({
          _tag: 'WriteStatement',
          destination: destination.fact,
          ...(root === undefined ? {} : { root }),
          value: value.fact,
          compatible,
          region,
          syntax: element,
        }),
      )
      continue
    }

    if (element.kind === 'WhileStatement') {
      const region = nextRegion()
      const loop = Object.freeze({
        _tag: 'HirLoop' as const,
        function: context.declaration.id,
        ordinal: context.loops.length,
      })
      context.loops.push(loop)
      const bodyNode = SyntaxTree.directNode(element, 'Block')
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
        const conditionNode = statementExpressionNode(element)
        const condition = analyzeExpression(
          context.source,
          conditionNode,
          context.declarations,
          context.declaration,
          scope,
          context.resolution,
        )
        if (condition === undefined) {
          throw new RangeError(`Semantic analysis cannot analyze ${conditionNode.kind}`)
        }
        context.diagnostics.push(...condition.diagnostics)
        if (condition.fact.type._tag === 'Available' && condition.fact.type.type !== 'bool') {
          context.diagnostics.push(
            Diagnostic.conditionNotBool(Type.encode(condition.fact.type.type), conditionNode.span),
          )
        }
        const bodyEntry = snapshotCallableWrites()
        const body =
          bodyNode === undefined
            ? []
            : analyzeStatements(context, bodyNode, scope, Object.freeze([...loopStack, loop]))
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
          syntax: element,
        }),
      )
      continue
    }

    if (element.kind === 'BreakStatement' || element.kind === 'ContinueStatement') {
      const region = nextRegion()
      const target = loopStack.at(-1)
      if (target === undefined) {
        context.diagnostics.push(
          Diagnostic.transferOutsideLoop(
            element.kind === 'BreakStatement' ? 'break' : 'continue',
            element.span,
          ),
        )
      }
      facts.push(
        Object.freeze({
          _tag: element.kind,
          ...(target === undefined ? {} : { target }),
          region,
          syntax: element,
        }),
      )
      continue
    }

    if (element.kind === 'ReturnStatement') {
      const region = nextRegion()
      const expressionNode = statementExpressionNode(element)
      const expression = analyzeExpression(
        context.source,
        expressionNode,
        context.declarations,
        context.declaration,
        scope,
        context.resolution,
        !context.effectBlock && context.declaration.returnType._tag === 'Resolved'
          ? context.declaration.returnType.type
          : undefined,
        !context.effectBlock && DeclarationFacts.returnedBorrow(context.declaration) !== undefined,
      )
      if (expression === undefined) {
        throw new RangeError(`Semantic analysis cannot analyze ${expressionNode.kind}`)
      }
      context.diagnostics.push(...expression.diagnostics)
      if (
        !context.effectBlock &&
        expression.type !== undefined &&
        Type.isCallable(expression.type) &&
        expression.type.mode !== 'Shared' &&
        !concreteCallableIdentity(expression.fact, context.resolution.writtenCallableBindings)
      ) {
        context.diagnostics.push(Diagnostic.unknownOwnedCallableReturn(expressionNode.span))
      }
      facts.push(
        Object.freeze({
          _tag: 'ReturnStatement',
          expression: expression.fact,
          region,
          syntax: element,
        }),
      )
      break
    }

    if (element.kind === 'FailStatement') {
      const region = nextRegion()
      const expressionNode = statementExpressionNode(element)
      const expression = analyzeExpression(
        context.source,
        expressionNode,
        context.declarations,
        context.declaration,
        scope,
        context.resolution,
      )
      if (expression === undefined)
        throw new RangeError(`Semantic analysis cannot analyze ${expressionNode.kind}`)
      context.diagnostics.push(...expression.diagnostics)
      const failure =
        expression.type !== undefined &&
        (Type.isRuntimeConcrete(expression.type) ||
          (Type.isParameter(expression.type) && expression.type.kind === 'Value'))
          ? expression.type
          : undefined
      if (!context.effectBlock && context.declaration.functionKind !== 'Effect')
        context.diagnostics.push(Diagnostic.failOutsideEffect(element.span))
      if (expression.type !== undefined && failure === undefined)
        context.diagnostics.push(
          Diagnostic.invalidFailureType(Type.encode(expression.type), expressionNode.span),
        )
      if (
        failure !== undefined &&
        DeclarationFacts.containsLexicalBorrow(context.resolution.index, failure) &&
        !isStaticallyDetachedFailure(expression.fact, context.resolution.index)
      )
        context.diagnostics.push(
          Diagnostic.providerBackedFailure(Type.encode(failure), expressionNode.span),
        )
      if (
        !context.effectBlock &&
        failure !== undefined &&
        !(Type.isParameter(failure)
          ? Type.failureMemberParameters(context.declaration.failureRow.row).some((parameter) =>
              Type.equals(parameter, failure),
            )
          : context.declaration.failureRow.failures.some((candidate) =>
              Type.equals(candidate, failure),
            ))
      )
        context.diagnostics.push(
          Diagnostic.undeclaredFailure(Type.encode(failure), expressionNode.span),
        )
      facts.push(
        Object.freeze({
          _tag: 'FailStatement',
          expression: expression.fact,
          ...(failure === undefined ? {} : { failure }),
          transfer: SyntaxTree.directToken(element, 'MoveKeyword') === undefined ? 'Copy' : 'Move',
          region,
          syntax: element,
        }),
      )
      break
    }

    if (element.kind === 'DropStatement') {
      const region = nextRegion()
      const expressionNode = statementExpressionNode(element)
      const expression = analyzeExpression(
        context.source,
        expressionNode,
        context.declarations,
        context.declaration,
        scope,
        context.resolution,
      )
      if (expression === undefined)
        throw new RangeError(`Semantic analysis cannot analyze ${expressionNode.kind}`)
      context.diagnostics.push(...expression.diagnostics)
      facts.push(
        Object.freeze({
          _tag: 'DropStatement',
          expression: expression.fact,
          region,
          syntax: element,
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

export const expressionNever = (expression: ExpressionFact): boolean =>
  expression.type._tag === 'Available' && Type.isNever(expression.type.type)

export const implicitReturn = (
  statement: Extract<StatementFact, { readonly _tag: 'ReturnStatement' }>,
): boolean => SyntaxTree.directToken(statement.syntax, 'ReturnKeyword') === undefined

/**
 * Computes source-level return reachability. Parser-created zero-width unit returns preserve a
 * recoverable terminal node, but remain ordinary fallthrough for contract checking.
 */
export const returnFlowOf = (
  body: ReadonlyArray<StatementFact>,
  implicitReturnFallsThrough = true,
): ReturnFlow => {
  const returns: Array<Extract<StatementFact, { readonly _tag: 'ReturnStatement' }>> = []
  let fallsThrough = true
  for (const statement of body) {
    if (!fallsThrough) break
    if (statement._tag === 'ReturnStatement') {
      if (implicitReturn(statement)) {
        fallsThrough = implicitReturnFallsThrough
      } else {
        returns.push(statement)
        fallsThrough = false
      }
      continue
    }
    if (
      statement._tag === 'FailStatement' ||
      statement._tag === 'BreakStatement' ||
      statement._tag === 'ContinueStatement'
    ) {
      fallsThrough = false
      continue
    }
    if (statement._tag === 'UnsafeStatement') {
      const nested = returnFlowOf(statement.statements, implicitReturnFallsThrough)
      returns.push(...nested.returns)
      fallsThrough = nested.fallsThrough
      continue
    }
    if (statement._tag === 'IfStatement' || statement._tag === 'IfLetStatement') {
      if (statement._tag === 'IfStatement' && expressionNever(statement.condition)) {
        fallsThrough = false
        continue
      }
      const taken = returnFlowOf(statement.taken, implicitReturnFallsThrough)
      const otherwise = returnFlowOf(statement.otherwise, implicitReturnFallsThrough)
      returns.push(...taken.returns, ...otherwise.returns)
      fallsThrough = taken.fallsThrough || otherwise.fallsThrough
      continue
    }
    if (statement._tag === 'WhileStatement') {
      if (expressionNever(statement.condition)) {
        fallsThrough = false
        continue
      }
      returns.push(...returnFlowOf(statement.body, implicitReturnFallsThrough).returns)
      fallsThrough = true
      continue
    }
    if (statement._tag === 'BindStatement') {
      fallsThrough = !expressionNever(statement.binding.initializer)
      continue
    }
    if (statement._tag === 'PatternBindStatement') {
      fallsThrough = !expressionNever(statement.selection.subject)
      continue
    }
    if (statement._tag === 'ExpressionStatement' || statement._tag === 'DropStatement') {
      fallsThrough = !expressionNever(statement.expression)
      continue
    }
    if (statement._tag === 'WriteStatement')
      fallsThrough = !expressionNever(statement.destination) && !expressionNever(statement.value)
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
        if (expression(arm.result)) completes = true
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

export const analyzeFunctionBody = (
  source: SourceFile.SourceFile,
  declaration: DeclarationFact,
  declarations: ReadonlyArray<DeclarationFact>,
  resolution: ResolutionContext,
): FunctionAnalysis => {
  const blockNode = childNode(declaration.syntax, 'Block')
  const unsafeSpans: Array<SourceSpan.SourceSpan> = []
  const collectUnsafeSpans = (node: SyntaxTree.Node): void => {
    if (node.kind === 'UnsafeStatement') unsafeSpans.push(node.span)
    for (const child of node.children) if (SyntaxTree.isNode(child)) collectUnsafeSpans(child)
  }
  collectUnsafeSpans(declaration.syntax)
  const nextBindingOrdinal = { value: 0 }
  const bodyResolution: ResolutionContext = Object.freeze({
    ...resolution,
    unsafeSpans: Object.freeze(unsafeSpans),
    nextBindingOrdinal,
    executableFunction: declaration.id,
    ...(declaration.canonical._tag === 'Canonical'
      ? { executableOwner: declaration.canonical.id }
      : {}),
    executableSites: executableSites(declaration.syntax),
    writtenCallableBindings: new Set<number>(),
  })
  const context: BodyContext = {
    source,
    declaration,
    declarations,
    bindings: [],
    diagnostics: [],
    regions: [],
    loops: [],
    resolution: bodyResolution,
    nextBindingOrdinal,
  }
  const statements = analyzeStatements(
    context,
    blockNode,
    Object.freeze({ parameters: declaration.parameters, bindings: [], patternBindings: [] }),
  )
  const returnedBorrow = DeclarationFacts.returnedBorrow(declaration)

  const bindingOrigins = new Map<number, DeclarationFacts.ParameterFact | undefined>()
  const originOf = (
    expression: ExpressionFact,
    patternOrigins: ReadonlyMap<string, DeclarationFacts.ParameterFact | undefined> = new Map(),
  ): DeclarationFacts.ParameterFact | undefined => {
    if (expression._tag === 'Grouped') return originOf(expression.expression, patternOrigins)
    if (expression._tag === 'Move') return originOf(expression.subject, patternOrigins)
    if (expression._tag === 'Identifier') {
      if (expression.reference._tag === 'Resolved') return expression.reference.parameter
      if (expression.reference._tag === 'ResolvedBinding') {
        const ordinal = expression.reference.binding.id.ordinal
        if (!bindingOrigins.has(ordinal)) {
          bindingOrigins.set(
            ordinal,
            originOf(expression.reference.binding.initializer, patternOrigins),
          )
        }
        return bindingOrigins.get(ordinal)
      }
      if (expression.reference._tag === 'ResolvedPattern') {
        const id = expression.reference.binding.id
        return patternOrigins.get(`${id.arm.match.span.start}:${id.arm.ordinal}:${id.ordinal}`)
      }
      return undefined
    }
    if (expression._tag === 'Borrow') {
      if (expression.formation._tag === 'Unavailable') return undefined
      const root = expression.formation.root
      if (root._tag === 'ParameterRoot') return root.parameter
      if (root._tag === 'BindingRoot') return originOf(root.binding.initializer, patternOrigins)
      if (root._tag === 'TemporaryRoot') return undefined
      const id = root.binding.id
      return patternOrigins.get(`${id.arm.match.span.start}:${id.arm.ordinal}:${id.ordinal}`)
    }
    if (expression._tag === 'FieldProjection' || expression._tag === 'IndexProjection') {
      return originOf(expression.subject, patternOrigins)
    }
    if (expression._tag === 'Call' || expression._tag === 'CallableApply') {
      const source = returnedBorrowExpression(expression)
      return source === undefined ? undefined : originOf(source, patternOrigins)
    }
    if (expression._tag === 'Match') {
      const scrutinee = originOf(expression.scrutinee, patternOrigins)
      const origins = expression.arms
        .filter((arm) => arm.reachable)
        .map((arm) => {
          const armOrigins = new Map(patternOrigins)
          for (const binding of arm.bindings) {
            const id = binding.id
            armOrigins.set(`${id.arm.match.span.start}:${id.arm.ordinal}:${id.ordinal}`, scrutinee)
          }
          return originOf(arm.result, armOrigins)
        })
      if (origins.some((origin) => origin === undefined)) return undefined
      const first = origins.at(0)
      return first !== undefined &&
        origins.every((origin) => origin?.id.ordinal === first.id.ordinal)
        ? first
        : undefined
    }
    return undefined
  }

  if (returnedBorrow !== undefined) {
    const isBorrowFreeReturn = (expression: ExpressionFact): boolean => {
      if (expression._tag === 'Grouped') return isBorrowFreeReturn(expression.expression)
      if (expression._tag === 'StaticText') return expression.data?.kind === 'Text'
      if (expression._tag !== 'Call' || expression.reference._tag !== 'Resolved') return false
      const argument = returnedBorrowArgument(expression)
      return (
        argument === undefined &&
        expression.type._tag === 'Available' &&
        Type.containsViewBorrow(expression.type.type) &&
        expression.arguments.every(
          (candidate) =>
            candidate.type._tag === 'Available' && !Type.containsViewBorrow(candidate.type.type),
        )
      )
    }
    const validateReturns = (body: ReadonlyArray<StatementFact>): void => {
      for (const statement of body) {
        if (statement._tag === 'ReturnStatement') {
          const origin = originOf(statement.expression)
          if (
            origin?.id.ordinal !== returnedBorrow.parameter.id.ordinal &&
            !isBorrowFreeReturn(statement.expression)
          ) {
            context.diagnostics.push(
              Diagnostic.invalidReturnedBorrowOrigin(statement.expression.syntax.span),
            )
          }
        } else if (statement._tag === 'UnsafeStatement') {
          validateReturns(statement.statements)
        } else if (statement._tag === 'IfStatement' || statement._tag === 'IfLetStatement') {
          validateReturns(statement.taken)
          validateReturns(statement.otherwise)
        } else if (statement._tag === 'WhileStatement') {
          validateReturns(statement.body)
        }
      }
    }
    validateReturns(statements)
  }

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
  let validReturnContract = declaration.returnType._tag === 'Resolved'
  if (declaration.returnType._tag === 'Resolved') {
    for (const returned of returnFlow.returns) {
      if (returned.expression.type._tag !== 'Available') {
        validReturnContract = false
        continue
      }
      const actual = returned.expression.type.type
      if (declaredReturnTypesCompatible(declaration, returned.expression)) continue
      validReturnContract = false
      context.diagnostics.push(
        representationJoinDiagnostic(
          declaration.returnType.type,
          actual,
          declaration.returnType.syntax.span,
          returned.expression.syntax.span,
          returned.expression.syntax.span,
        ) ??
          unionConversionDiagnostic(
            actual,
            declaration.returnType.type,
            returned.expression.syntax.span,
          ) ??
          Diagnostic.returnTypeMismatch(
            Type.encode(declaration.returnType.type),
            Type.encode(actual),
            returned.expression.syntax.span,
          ),
      )
    }
    if (returnFlow.fallsThrough && !Type.equals(declaration.returnType.type, Type.unit)) {
      validReturnContract = false
      context.diagnostics.push(
        Diagnostic.missingReturn(
          Type.encode(declaration.returnType.type),
          SyntaxTree.directToken(blockNode, 'RightBrace')?.span ?? blockNode.span,
        ),
      )
    }
  }
  const returnCompatibility = validReturnContract ? compatible : unavailableCompatibility

  return Object.freeze({
    fact: Object.freeze({
      _tag: 'FunctionFact',
      declaration,
      statements,
      bindings: Object.freeze([...context.bindings]),
      regionOrder: Object.freeze([...context.regions]),
      returnedExpression: expression,
      returnCompatibility,
      ...(returnedBorrow === undefined ? {} : { returnedBorrow }),
    }),
    diagnostics: Object.freeze([...context.diagnostics]),
  })
}
