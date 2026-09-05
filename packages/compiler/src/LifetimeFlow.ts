import * as BodyLifetime from './BodyLifetime.js'
import * as BodyControlFlow from './BodyControlFlow.js'
import * as CleanupPlan from './CleanupPlan.js'
import * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as Elaboration from './Elaboration.js'
import * as HirLowering from './HirLowering.js'
import * as Lifetime from './Lifetime.js'
import type * as MovePath from './MovePath.js'
import * as Ownership from './Ownership.js'
import type * as SourceSpan from './SourceSpan.js'
import * as SyntaxTree from './SyntaxTree.js'
import * as Type from './Type.js'
import * as TypeOutlives from './TypeOutlives.js'
import * as TypeInference from './internal/TypeInference.js'

/** One concrete source of a semantic borrow, separate from its reusable lifetime contract. */
export interface Origin {
  readonly lifetime: Lifetime.Lifetime
  readonly root?: Ownership.BindingSite
  readonly path?: ReadonlyArray<Elaboration.BorrowSelectorFact>
  readonly parent?: Lifetime.Lifetime
  readonly span: SourceSpan.SourceSpan
}

/** Inspectable finite region proof retained independently of HIR and runtime specialization. */
export interface LifetimeFlow {
  readonly controlFlow: BodyControlFlow.BodyControlFlow
  readonly retiredUses: ReadonlyMap<string, ReadonlySet<number>>
  readonly retirementWork: { readonly comparisons: number; readonly comparisonCacheHits: number }
  readonly syntaxPointCount: number
  readonly input: Lifetime.Input
  readonly solution: Lifetime.Solution
  readonly origins: ReadonlyMap<string, Origin>
  readonly spans: ReadonlyMap<number, SourceSpan.SourceSpan>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

interface Region {
  readonly lifetime: Lifetime.Lifetime
  readonly available: Set<number>
  readonly required: Set<number>
}

const encloses = (outer: SourceSpan.SourceSpan, inner: SourceSpan.SourceSpan): boolean =>
  outer.sourceId === inner.sourceId && outer.start <= inner.start && inner.end <= outer.end

const rootSite = (root: Elaboration.BorrowRootFact): Ownership.BindingSite => {
  switch (root._tag) {
    case 'BindingRoot':
      return { _tag: 'Let', binding: root.binding.id }
    case 'ParameterRoot':
      return { _tag: 'Parameter', parameter: root.parameter.id }
    case 'PatternRoot':
      return { _tag: 'Pattern', binding: root.binding.id }
    case 'TemporaryRoot':
      return { _tag: 'Temporary', owner: root.owner }
  }
}

const expressionRoot = (
  expression: Elaboration.ExpressionFact,
  throughBorrow = false,
): Elaboration.BorrowRootFact | undefined => {
  if (expression._tag === 'Identifier') {
    if (expression.reference._tag === 'ResolvedBinding')
      return { _tag: 'BindingRoot', binding: expression.reference.binding, path: [] }
    if (expression.reference._tag === 'Resolved')
      return { _tag: 'ParameterRoot', parameter: expression.reference.parameter, path: [] }
    if (expression.reference._tag === 'ResolvedPattern')
      return { _tag: 'PatternRoot', binding: expression.reference.binding, path: [] }
  }
  if (expression._tag === 'Grouped') return expressionRoot(expression.expression, throughBorrow)
  if (expression._tag === 'Move') return expressionRoot(expression.subject, throughBorrow)
  if (expression._tag === 'FieldProjection' && expression.state._tag === 'Resolved') {
    if (
      !throughBorrow &&
      expression.subject.type._tag === 'Available' &&
      Type.isReference(expression.subject.type.type)
    )
      return undefined
    const root = expressionRoot(expression.subject, throughBorrow)
    return root === undefined
      ? undefined
      : {
          ...root,
          path: [
            ...root.path,
            { _tag: 'Field', field: expression.state.field.id, span: expression.syntax.span },
          ],
        }
  }
  if (
    expression._tag === 'IndexProjection' &&
    expression.array !== undefined &&
    (expression.bounds._tag === 'Proven' || expression.bounds._tag === 'Runtime')
  ) {
    const root = expressionRoot(expression.subject, throughBorrow)
    return root === undefined
      ? undefined
      : {
          ...root,
          path: [
            ...root.path,
            {
              _tag: 'Index',
              index: expression.index,
              array: expression.array,
              bounds: expression.bounds,
              span: expression.syntax.span,
            },
          ],
        }
  }
  if (throughBorrow && expression._tag === 'ReferentProjection')
    return expressionRoot(expression.subject, true)
  if (expression._tag === 'Borrow' && expression.formation._tag !== 'Unavailable')
    return expression.formation.root
  return undefined
}

// A referent read still demands the lifetime stored in its reference or slice carrier.
// Keep this distinct from expressionRoot: a write through a borrow does not replace its carrier.
const carrierExpression = (expression: Elaboration.ExpressionFact): Elaboration.ExpressionFact => {
  if (expression._tag === 'Grouped') return carrierExpression(expression.expression)
  if (
    expression._tag === 'Borrow' ||
    expression._tag === 'Move' ||
    expression._tag === 'ReferentProjection' ||
    (expression._tag === 'IndexProjection' && expression.array === undefined) ||
    (expression._tag === 'FieldProjection' &&
      expression.subject.type._tag === 'Available' &&
      Type.isReference(expression.subject.type.type))
  )
    return carrierExpression(expression.subject)
  return expression
}

const pathsOverlap = (
  left: ReadonlyArray<Elaboration.BorrowSelectorFact>,
  right: ReadonlyArray<Elaboration.BorrowSelectorFact>,
): boolean => {
  for (const [ordinal, selector] of left.entries()) {
    const other = right.at(ordinal)
    if (other === undefined) return true
    if (
      selector._tag === 'Field' &&
      other._tag === 'Field' &&
      selector.field.ordinal !== other.field.ordinal
    )
      return false
    if (
      selector._tag === 'Index' &&
      other._tag === 'Index' &&
      selector.bounds._tag === 'Proven' &&
      other.bounds._tag === 'Proven' &&
      selector.bounds.index !== other.bounds.index
    )
      return false
  }
  return true
}

/** Validates one generic body using only its declared assumptions and selected semantic facts. */
export const analyze = (
  declaration: DeclarationFacts.DeclarationFact,
  statements: ReadonlyArray<Elaboration.StatementFact>,
  body: BodyLifetime.BodyLifetime,
  index: DeclarationIndex.Index,
): LifetimeFlow => {
  const outlivesScope = TypeOutlives.context(index.modules)
  const applicationDiagnostics = new Map<string, Diagnostic.Diagnostic>()
  const entries = [...body.points]
  const root = entries.at(0)?.[0] ?? declaration.syntax
  const controlFlow = BodyControlFlow.make(statements, root)
  const boundaries = new Map<SyntaxTree.Node, number>()
  const terminalSpans = new Map<number, SourceSpan.SourceSpan>()
  Elaboration.visitStatementFacts(statements, {
    statement: (statement) => {
      if (statement._tag !== 'ReturnStatement' && statement._tag !== 'FailStatement') return
      if (boundaries.has(statement.syntax)) return
      const point = entries.length + boundaries.size
      boundaries.set(statement.syntax, point)
      terminalSpans.set(point, statement.expression.syntax.span)
    },
  })
  const pointCount = entries.length + boundaries.size
  const allPoints = Array.from({ length: pointCount }, (_, ordinal) => ordinal)
  const parents = new Map<SyntaxTree.Node, SyntaxTree.Node>()
  const spans = new Map(entries.map(([node, point]) => [point, node.span]))
  for (const [point, span] of terminalSpans) spans.set(point, span)
  for (const [node] of entries)
    for (const child of node.children) if (SyntaxTree.isNode(child)) parents.set(child, node)
  const regions = new Map<string, Region>()
  const origins = new Map<string, Origin>()
  const constraints = new Map(body.constraints)
  const patternRoots = new Map<string, Elaboration.BorrowRootFact>()
  const canonicalRoot = (root: Elaboration.BorrowRootFact): Elaboration.BorrowRootFact => {
    const alias =
      root._tag === 'PatternRoot' ? patternRoots.get(Ownership.siteKey(rootSite(root))) : undefined
    return alias === undefined ? root : { ...alias, path: [...alias.path, ...root.path] }
  }
  const variantBranches: Array<{
    readonly root: Elaboration.BorrowRootFact
    readonly variant: number
    readonly syntax: SyntaxTree.Node
  }> = []
  const expressionUses = new Map<string, Elaboration.ExpressionFact>()
  const replacements: Array<{
    readonly root: Ownership.BindingSite
    readonly path: ReadonlyArray<Elaboration.BorrowSelectorFact>
    readonly lifetimes: ReadonlyArray<Lifetime.Lifetime>
    readonly syntax: SyntaxTree.Node
  }> = []
  const invalidations: Array<{
    readonly root: Ownership.BindingSite
    readonly path: ReadonlyArray<Elaboration.BorrowSelectorFact>
    readonly expression: Elaboration.ExpressionFact
    readonly after?: SyntaxTree.Node
  }> = []
  const scopeOf = (syntax: SyntaxTree.Node): SyntaxTree.Node => {
    let current = parents.get(syntax)
    while (current !== undefined) {
      if (current.kind === 'Block' || current.kind === 'MatchArm') return current
      current = parents.get(current)
    }
    return root
  }
  const ensure = (lifetime: Lifetime.Lifetime): Region => {
    const key = Lifetime.key(lifetime)
    const previous = regions.get(key)
    if (previous !== undefined) return previous
    const region: Region = { lifetime, available: new Set(allPoints), required: new Set() }
    regions.set(key, region)
    return region
  }
  const constrain = (longer: Lifetime.Lifetime, shorter: Lifetime.Lifetime): void => {
    ensure(longer)
    ensure(shorter)
    const bound = { longer, shorter }
    constraints.set(Lifetime.assumptions([bound]).key, bound)
  }
  const requireType = (type: Type.Type, point: number): void => {
    for (const lifetime of Type.storageLifetimes(type)) ensure(lifetime).required.add(point)
  }
  const restrict = (
    lifetime: Lifetime.Lifetime,
    available: Iterable<number>,
    origin: Origin,
  ): void => {
    const region = ensure(lifetime)
    const allowed = new Set(available)
    for (const point of region.available) if (!allowed.has(point)) region.available.delete(point)
    origins.set(Lifetime.key(lifetime), Object.freeze(origin))
  }
  const anchor = (
    lifetime: Lifetime.Lifetime,
    source: Elaboration.BorrowRootFact,
    statement: SyntaxTree.Node,
    span: SourceSpan.SourceSpan,
    referent = false,
  ): void => {
    const alias =
      source._tag === 'PatternRoot'
        ? patternRoots.get(Ownership.siteKey(rootSite(source)))
        : undefined
    if (alias !== undefined) {
      anchor(lifetime, { ...alias, path: [...alias.path, ...source.path] }, statement, span, true)
      return
    }
    // Indexing a stored slice borrows its backing allocation, whose validity is
    // independent of the receiver used to retrieve the slice descriptor.
    const sliceIndex = source.path.findLast((selector) => selector._tag === 'SliceIndex')
    if (sliceIndex?._tag === 'SliceIndex') {
      constrain(sliceIndex.slice.lifetime, lifetime)
      origins.set(Lifetime.key(lifetime), {
        lifetime, root: rootSite(source), path: source.path,
        parent: sliceIndex.slice.lifetime, span,
      })
      return
    }
    let rootType: Type.Type | undefined
    if (source._tag === 'ParameterRoot' && source.parameter.declaredType._tag === 'Resolved')
      rootType = source.parameter.declaredType.type
    else if (source._tag === 'BindingRoot' && source.binding.inferredType._tag === 'Available')
      rootType = source.binding.inferredType.type
    else if (source._tag === 'PatternRoot' && source.binding.type._tag === 'Available')
      rootType = source.binding.type.type
    if (
      (source.path.length > 0 || referent) &&
      rootType !== undefined &&
      (Type.isReference(rootType) || Type.isSlice(rootType))
    ) {
      constrain(rootType.lifetime, lifetime)
      origins.set(Lifetime.key(lifetime), {
        lifetime,
        root: rootSite(source),
        path: source.path,
        parent: rootType.lifetime,
        span,
      })
      return
    }
    let syntax = root
    if (source._tag === 'BindingRoot' || source._tag === 'PatternRoot')
      syntax = source.binding.syntax
    else if (source._tag === 'TemporaryRoot') syntax = statement
    const scope = source._tag === 'TemporaryRoot' ? statement : scopeOf(syntax)
    const available = entries
      .filter(
        ([node]) =>
          encloses(scope.span, node.span) &&
          (source._tag !== 'BindingRoot' || node.span.start >= syntax.span.start),
      )
      .map(([, point]) => point)
    restrict(lifetime, available, { lifetime, root: rootSite(source), path: source.path, span })
  }
  const borrowedCapture = (
    expression: Elaboration.ExpressionFact,
    statement: SyntaxTree.Node,
  ): void => {
    const source = expressionRoot(expression)
    const lifetime = BodyLifetime.region(body, expression.syntax, 'Borrow')
    if (source === undefined || lifetime === undefined) return
    const type = expression.type._tag === 'Available' ? expression.type.type : undefined
    if (type !== undefined && (Type.isReference(type) || Type.isSlice(type))) {
      constrain(type.lifetime, lifetime)
      origins.set(Lifetime.key(lifetime), {
        lifetime,
        root: rootSite(source),
        path: source.path,
        parent: type.lifetime,
        span: expression.syntax.span,
      })
    } else anchor(lifetime, source, statement, expression.syntax.span)
  }
  const bindPatterns = (
    bindings: ReadonlyArray<Elaboration.PatternBindingFact>,
    source: Elaboration.ExpressionFact,
    access: Elaboration.PatternSelectionFact['access'],
  ): void => {
    if (access === 'Move' || access === 'Copy') return
    const root = expressionRoot(source, true)
    if (root !== undefined)
      for (const binding of bindings)
        patternRoots.set(Ownership.siteKey({ _tag: 'Pattern', binding: binding.id }), {
          ...root,
          path: [
            ...root.path,
            ...binding.path.map((field): Elaboration.BorrowSelectorFact => ({
              _tag: 'Field',
              field,
              span: binding.syntax.span,
            })),
          ],
        })
  }
  const visitExpression = (
    expression: Elaboration.ExpressionFact,
    statement: SyntaxTree.Node,
    place = false,
  ): void => {
    expressionUses.set(
      `${expression.syntax.span.sourceId}:${expression.syntax.span.start}:${expression.syntax.span.end}`,
      expression,
    )
    const point = body.points.get(expression.syntax)
    if (expression.type._tag === 'Available') {
      for (const nominal of Type.nominals(expression.type.type)) {
        const failures = TypeOutlives.application(nominal, outlivesScope, (longer, shorter) => {
          if (Lifetime.outlives(outlivesScope.assumptions, longer, shorter)) return true
          if (longer._tag === 'PlaceholderLifetime' || shorter._tag === 'PlaceholderLifetime')
            return false
          if (longer._tag !== 'LocalLifetime' && shorter._tag !== 'LocalLifetime') return false
          constrain(longer, shorter)
          return true
        })
        for (const failure of failures) {
          const diagnostic = Diagnostic.unsatisfiedLifetimeBound(
            Type.encodeGenericArgument(failure.argument),
            Lifetime.display(failure.required),
            expression.syntax.span,
          )
          applicationDiagnostics.set(
            `${Type.key(nominal)}:${failure.ordinal}:${Lifetime.key(failure.required)}:${expression.syntax.span.start}`,
            diagnostic,
          )
        }
      }
    }
    if (point !== undefined && expression.type._tag === 'Available' && !place)
      requireType(expression.type.type, point)
    if (expression._tag === 'Borrow') {
      const formation = expression.formation
      const type = expression.type._tag === 'Available' ? expression.type.type : undefined
      if (
        formation._tag !== 'Unavailable' &&
        type !== undefined &&
        (Type.isReference(type) || Type.isSlice(type))
      ) {
        if (formation._tag === 'ValueReborrow' || formation._tag === 'SliceReborrow') {
          constrain(formation.parent.lifetime, type.lifetime)
          origins.set(Lifetime.key(type.lifetime), {
            lifetime: type.lifetime,
            root: rootSite(formation.root),
            path: formation.root.path,
            parent: formation.parent.lifetime,
            span: expression.syntax.span,
          })
        } else anchor(type.lifetime, formation.root, statement, expression.syntax.span)
      }
      visitExpression(expression.subject, statement, true)
      return
    }
    if (expression._tag === 'PlaceReplace') {
      const source = expressionRoot(expression.destination)
      if (source !== undefined)
        invalidations.push({ root: rootSite(source), path: source.path, expression })
    }
    if (expression._tag === 'Move') {
      const source = expressionRoot(expression.subject)
      if (source !== undefined)
        invalidations.push({ root: rootSite(source), path: source.path, expression })
    }
    if (expression._tag === 'FieldProjection' || expression._tag === 'IndexProjection') {
      const subject = expression.subject.type
      if (
        point !== undefined &&
        subject._tag === 'Available' &&
        (Type.isReference(subject.type) || Type.isSlice(subject.type))
      )
        ensure(subject.type.lifetime).required.add(point)
      visitExpression(expression.subject, statement, true)
      if (expression._tag === 'IndexProjection') visitExpression(expression.index, statement)
      return
    }
    if (expression._tag === 'ReferentProjection') {
      const subject = expression.subject.type
      if (point !== undefined && subject._tag === 'Available' && Type.isReference(subject.type))
        ensure(subject.type.lifetime).required.add(point)
      visitExpression(expression.subject, statement, true)
      return
    }
    if (expression._tag === 'Match') {
      visitExpression(expression.scrutinee, statement, expression.access === 'Place')
      for (const arm of expression.arms) {
        if (!arm.reachable) continue
        bindPatterns(arm.bindings, expression.scrutinee, expression.access)
        const selectedRoot = expressionRoot(expression.scrutinee)
        if (
          expression.access === 'Place' &&
          selectedRoot !== undefined &&
          arm.pattern._tag === 'UnionVariantPattern' &&
          arm.pattern.coverage?._tag === 'NominalUnionVariant'
        )
          variantBranches.push({
            root: canonicalRoot(selectedRoot),
            variant: arm.pattern.coverage.variantOrdinal,
            syntax: arm.syntax,
          })
        if (arm.guard !== undefined) visitExpression(arm.guard, statement)
        if (arm.body._tag === 'Expression') visitExpression(arm.body.expression, statement)
        else visitStatements(arm.body.statements)
      }
      return
    }
    if (expression._tag === 'CallableSection') {
      for (const capture of expression.captures)
        if (capture.access === 'Shared' || capture.access === 'Exclusive')
          borrowedCapture(capture.expression, statement)
    }
    if (expression._tag === 'EffectBlock') {
      for (const capture of expression.captures)
        if (capture.expression !== undefined) {
          visitExpression(capture.expression, statement)
          if (capture.access === 'Shared' || capture.access === 'Exclusive')
            borrowedCapture(capture.expression, statement)
        }
      visitStatements(expression.statements)
      return
    }
    for (const child of Elaboration.expressionChildren(expression))
      visitExpression(child, statement)
  }
  const visitStatements = (statements: ReadonlyArray<Elaboration.StatementFact>): void => {
    for (const statement of statements) {
      const syntax =
        statement._tag === 'BindStatement' ? statement.binding.syntax : statement.syntax
      if (statement._tag === 'PatternBindStatement' || statement._tag === 'IfLetStatement')
        bindPatterns(
          statement.selection.bindings,
          statement.selection.source,
          statement.selection.access,
        )
      for (const expression of HirLowering.directStatementExpressions(statement))
        visitExpression(
          expression,
          syntax,
          statement._tag === 'WriteStatement' && expression === statement.destination,
        )
      if (statement._tag === 'WriteStatement') {
        const destination = expressionRoot(statement.destination)
        const source = destination === undefined ? undefined : canonicalRoot(destination)
        if (source !== undefined) {
          if (statement.compatible && statement.destination.type._tag === 'Available') {
            const lifetimes = Type.storageLifetimes(statement.destination.type.type)
            if (lifetimes.length > 0)
              replacements.push({
                root: rootSite(source),
                path: source.path,
                lifetimes,
                syntax: statement.syntax,
              })
          }
          invalidations.push({
            root: rootSite(source),
            path: source.path,
            expression: statement.value,
            after: statement.syntax,
          })
        }
      }
      if (statement._tag === 'ReturnStatement' || statement._tag === 'FailStatement') {
        const boundary = boundaries.get(statement.syntax)
        if (statement.expression.type._tag === 'Available' && boundary !== undefined)
          requireType(statement.expression.type.type, boundary)
      } else if (statement._tag === 'DropStatement') {
        const source = expressionRoot(statement.expression)
        if (source !== undefined)
          invalidations.push({
            root: rootSite(source),
            path: source.path,
            expression: statement.expression,
          })
      }
      if (statement._tag === 'UnsafeStatement') visitStatements(statement.statements)
      else if (statement._tag === 'IfStatement' || statement._tag === 'IfLetStatement') {
        visitStatements(statement.taken)
        visitStatements(statement.otherwise)
      } else if (statement._tag === 'WhileStatement') visitStatements(statement.body)
    }
  }
  visitStatements(statements)
  for (const bound of body.constraints.values()) constrain(bound.longer, bound.shorter)
  for (const bound of DeclarationFacts.executableLifetimes(declaration).lifetimeBounds ?? [])
    constrain(bound.longer, bound.shorter)
  // Reachability includes loop backedges and respects branch exits. Re-entering the borrow's
  // creation stops an old loan: the next iteration creates a distinct dynamic loan instance.
  for (const origin of origins.values()) {
    if (origin.root === undefined || origin.parent !== undefined) continue
    const created = BodyControlFlow.at(controlFlow, origin.span)
    if (created === undefined) continue
    const region = ensure(origin.lifetime)
    for (const event of invalidations) {
      if (
        Ownership.siteKey(event.root) !== Ownership.siteKey(origin.root) ||
        !pathsOverlap(origin.path ?? [], event.path)
      )
        continue
      const invalidated = BodyControlFlow.at(
        controlFlow,
        (event.after ?? event.expression.syntax).span,
      )
      if (
        invalidated === undefined ||
        !BodyControlFlow.reaches(controlFlow, created.after, invalidated.after, created.before)
      )
        continue
      for (const [node, point] of entries) {
        const use = BodyControlFlow.at(controlFlow, node.span)
        if (
          use !== undefined &&
          BodyControlFlow.reaches(controlFlow, invalidated.after, use.after, created.before)
        )
          region.available.delete(point)
      }
    }
  }
  const retiredUses = new Map<string, ReadonlySet<number>>()
  const storageDependencies = Lifetime.assumptions([
    ...constraints.values(),
    ...body.activatedConstraints.map(({ bound }) => bound),
  ])
  const retirementWork = { comparisons: 0, comparisonCacheHits: 0 }
  const comparedStorage = new Map<string, boolean>()
  const storageOutlives = (longer: Lifetime.Lifetime, shorter: Lifetime.Lifetime): boolean => {
    retirementWork.comparisons += 1
    const identity = `${Lifetime.key(longer)}:${Lifetime.key(shorter)}`
    const cached = comparedStorage.get(identity)
    if (cached !== undefined) {
      retirementWork.comparisonCacheHits += 1
      return cached
    }
    const result = Lifetime.outlives(storageDependencies, longer, shorter)
    comparedStorage.set(identity, result)
    return result
  }
  // Decompose only requested aggregate carriers. References are leaves; recursive pointee shapes
  // and unmentioned array elements are never materialized by replacement liveness.
  const storedPaths = (
    type: Type.Type,
    lifetime: Lifetime.Lifetime,
    span: SourceSpan.SourceSpan,
    seen: ReadonlySet<string> = new Set(),
  ): ReadonlyArray<{
    readonly path: ReadonlyArray<Elaboration.BorrowSelectorFact>
    readonly type: Type.Type
  }> => {
    if (!Type.storageLifetimes(type).some((stored) => storageOutlives(lifetime, stored))) return []
    if (Type.isNominal(type) && !seen.has(Type.key(type))) {
      const declaration = DeclarationFacts.byCanonical(index, {
        _tag: 'CanonicalDeclarationId',
        module: type.module,
        name: type.name,
      })
      if (declaration?._tag === 'StructDeclaration' || declaration?._tag === 'UnionDeclaration') {
        const substitution = TypeInference.substitution(
          declaration.typeParameters.map((parameter) => parameter.type),
          type.arguments,
        )
        if (substitution !== undefined)
          return (
            declaration._tag === 'StructDeclaration'
              ? declaration.fields
              : declaration.variants.flatMap((variant) => variant.fields)
          ).flatMap((field) =>
            field.declaredType._tag !== 'Resolved'
              ? [{ path: [], type }]
              : storedPaths(
                  Type.substitute(field.declaredType.type, substitution),
                  lifetime,
                  span,
                  new Set(seen).add(Type.key(type)),
                ).map((entry) => ({
                  path: [{ _tag: 'Field', field: field.id, span }, ...entry.path],
                  type: entry.type,
                })),
          )
      }
    }
    return [{ path: [], type }]
  }
  if (replacements.length > 0) {
    for (const [identity, origin] of origins) {
      const created = BodyControlFlow.at(controlFlow, origin.span)
      if (created === undefined || origin.parent !== undefined) continue
      const retired = new Set<number>()
      for (const [point, span] of spans) {
        const expression = expressionUses.get(`${span.sourceId}:${span.start}:${span.end}`)
        const value = expression === undefined ? undefined : carrierExpression(expression)
        const selected = value === undefined ? undefined : expressionRoot(value)
        const carrier = selected === undefined ? undefined : canonicalRoot(selected)
        const use = BodyControlFlow.at(controlFlow, span)
        if (carrier === undefined || use === undefined || value?.type._tag !== 'Available') continue
        const prefix = (
          left: ReadonlyArray<Elaboration.BorrowSelectorFact>,
          right: ReadonlyArray<Elaboration.BorrowSelectorFact>,
        ): boolean =>
          left.length <= right.length &&
          left.every((selector, ordinal) => {
            const selected = right.at(ordinal)
            if (selector._tag === 'Field')
              return (
                selected?._tag === 'Field' &&
                DeclarationFacts.sameFieldId(selector.field, selected.field)
              )
            return (
              selector._tag === 'Index' &&
              selected?._tag === 'Index' &&
              selector.bounds._tag === 'Proven' &&
              selected.bounds._tag === 'Proven' &&
              selector.bounds.index === selected.bounds.index
            )
          })
        const relevant = replacements.filter(
          (replacement) =>
            !encloses(replacement.syntax.span, origin.span) &&
            Ownership.siteKey(replacement.root) === Ownership.siteKey(rootSite(carrier)) &&
            replacement.lifetimes.some((lifetime) => storageOutlives(origin.lifetime, lifetime)),
        )
        const retiredPath = (
          path: ReadonlyArray<Elaboration.BorrowSelectorFact>,
          type: Type.Type,
        ): boolean => {
          const barriers = relevant.flatMap((replacement) => {
            if (!prefix(replacement.path, path)) return []
            const installed = BodyControlFlow.at(controlFlow, replacement.syntax.span)
            return installed === undefined ? [] : [installed.after]
          })
          for (const branch of variantBranches) {
            if (
              Ownership.siteKey(rootSite(branch.root)) !== Ownership.siteKey(rootSite(carrier)) ||
              !prefix(branch.root.path, path)
            )
              continue
            const field = path.at(branch.root.path.length)
            if (
              field?._tag !== 'Field' ||
              field.field.owner._tag !== 'UnionVariantFieldOwnerId' ||
              field.field.owner.variant.ordinal === branch.variant
            )
              continue
            const entered = BodyControlFlow.at(controlFlow, branch.syntax.span)
            if (entered !== undefined) barriers.push(entered.before)
          }
          if (
            barriers.length > 0 &&
            !BodyControlFlow.reaches(controlFlow, created.after, use.after, [
              created.before,
              ...barriers,
            ])
          )
            return true
          if (!Type.isFixedArray(type)) return false
          const selectedIndices = new Map<number, Elaboration.BorrowSelectorFact>()
          for (const replacement of relevant) {
            if (!prefix(path, replacement.path)) continue
            const selector = replacement.path.at(path.length)
            if (
              selector?._tag === 'Index' &&
              selector.bounds._tag === 'Proven' &&
              selector.bounds.index >= 0 &&
              selector.bounds.index < type.length
            )
              selectedIndices.set(selector.bounds.index, selector)
          }
          // Completion follows the touched, proven indices. Never allocate a node per array element.
          return (
            selectedIndices.size === type.length &&
            [...selectedIndices.values()].every((selector) =>
              storedPaths(type.element, origin.lifetime, span).every((entry) =>
                retiredPath([...path, selector, ...entry.path], entry.type),
              ),
            )
          )
        }
        const paths = storedPaths(value.type.type, origin.lifetime, span)
        const retiredEveryPath =
          paths.length > 0 &&
          paths.every((entry) => retiredPath([...carrier.path, ...entry.path], entry.type))
        if (retiredEveryPath) {
          retired.add(point)
          ensure(origin.lifetime).available.add(point)
        }
      }
      retiredUses.set(identity, retired)
    }
  }
  const universalAssumptions = Lifetime.assumptions([
    ...outlivesScope.assumptions.bounds,
    ...(DeclarationFacts.executableLifetimes(declaration).lifetimeBounds ?? []),
  ])
  const universalDiagnostics = new Map<string, Diagnostic.Diagnostic>()
  const incoming = new Map<string, Array<Lifetime.Lifetime>>()
  for (const bound of constraints.values()) {
    const predecessors = incoming.get(Lifetime.key(bound.shorter)) ?? []
    predecessors.push(bound.longer)
    incoming.set(Lifetime.key(bound.shorter), predecessors)
  }
  const outgoing = new Map<string, Array<Lifetime.Lifetime>>()
  for (const bound of constraints.values()) {
    const successors = outgoing.get(Lifetime.key(bound.longer)) ?? []
    successors.push(bound.shorter)
    outgoing.set(Lifetime.key(bound.longer), successors)
  }
  for (const { lifetime, parameter } of body.genericStorage.values()) {
    const pending = [...(outgoing.get(Lifetime.key(lifetime)) ?? [])]
    const visited = new Set<string>()
    let publicObligations = 0
    let allProven = true
    while (pending.length > 0) {
      const required = pending.pop()
      if (required === undefined || visited.has(Lifetime.key(required))) continue
      visited.add(Lifetime.key(required))
      if (required._tag === 'BoundLifetime' || required._tag === 'StaticLifetime') {
        publicObligations += 1
        const bounds = (body.parameterBounds.get(Type.key(parameter)) ?? []).map((region) => ({
          type: parameter,
          lifetime: region,
        }))
        if (
          !Type.satisfiesOutlives(parameter, required, bounds, (longer, shorter) =>
            Lifetime.outlives(universalAssumptions, longer, shorter),
          )
        ) {
          allProven = false
          const diagnostic = Diagnostic.unsatisfiedLifetimeBound(
            Type.encode(parameter),
            Lifetime.display(required),
            root.span,
          )
          universalDiagnostics.set(`${Type.key(parameter)}:${Lifetime.key(required)}`, diagnostic)
        }
      }
      pending.push(...(outgoing.get(Lifetime.key(required)) ?? []))
    }
    const available = entries.map(([, point]) => point)
    if (allProven && publicObligations > 0) available.push(...boundaries.values())
    restrict(lifetime, available, { lifetime, span: root.span })
  }
  for (const target of regions.values()) {
    if (target.lifetime._tag !== 'BoundLifetime' && target.lifetime._tag !== 'StaticLifetime')
      continue
    const pending = [...(incoming.get(Lifetime.key(target.lifetime)) ?? [])]
    const visited = new Set<string>()
    while (pending.length > 0) {
      const source = pending.pop()
      if (source === undefined || visited.has(Lifetime.key(source))) continue
      visited.add(Lifetime.key(source))
      const origin = origins.get(Lifetime.key(source))
      // A universal contract extends beyond this body's finite use points. Inference regions and
      // reborrows can reach it through their parents, but owned local storage cannot, even if the
      // constrained value is never subsequently used.
      const finiteStorage =
        source._tag === 'LocalLifetime' && origin?.root !== undefined && origin.parent === undefined
      if (
        finiteStorage ||
        (source._tag === 'BoundLifetime' &&
          !Lifetime.outlives(universalAssumptions, source, target.lifetime))
      ) {
        const diagnostic = Diagnostic.unsatisfiedLifetimeBound(
          Lifetime.display(source),
          Lifetime.display(target.lifetime),
          finiteStorage ? origin.span : root.span,
        )
        universalDiagnostics.set(
          `${Lifetime.key(source)}:${Lifetime.key(target.lifetime)}`,
          diagnostic,
        )
      }
      pending.push(...(incoming.get(Lifetime.key(source)) ?? []))
    }
  }
  const activatedConstraints = body.activatedConstraints.flatMap(({ bound, installed, owner }) => {
    ensure(bound.longer)
    ensure(bound.shorter)
    const installation = BodyControlFlow.at(controlFlow, installed.span)
    if (installation === undefined) return []
    const ownerBoundary =
      owner === undefined ? undefined : BodyControlFlow.at(controlFlow, owner.span)
    const barriers = [
      installation.before,
      ...(ownerBoundary === undefined ? [] : [ownerBoundary.before]),
    ]
    const points = new Set<number>()
    for (const [point, span] of spans) {
      const use = BodyControlFlow.at(controlFlow, span)
      if (
        use !== undefined &&
        BodyControlFlow.reaches(controlFlow, installation.after, use.after, barriers)
      )
        points.add(point)
    }
    return [{ ...bound, points }]
  })
  const input: Lifetime.Input = Object.freeze({
    pointCount,
    regions: Object.freeze([...regions.values()]),
    constraints: Object.freeze([...constraints.values()]),
    activatedConstraints,
  })
  const solution = Lifetime.solve(input)
  const diagnostics = Object.freeze([
    ...applicationDiagnostics.values(),
    ...universalDiagnostics.values(),
    ...diagnosticsOf(solution, origins, spans, root.span),
  ])
  return Object.freeze({
    controlFlow,
    retiredUses,
    retirementWork,
    syntaxPointCount: entries.length,
    input,
    solution,
    origins,
    spans,
    diagnostics,
  })
}

/** Tests concrete loan liveness at an access using the solved holder uses and source CFG. */
export const liveAt = (
  self: LifetimeFlow,
  start: SourceSpan.SourceSpan,
  access: SourceSpan.SourceSpan,
  end: SourceSpan.SourceSpan,
  write = false,
): boolean | undefined => {
  if (self.solution._tag !== 'Solved') return undefined
  const origins = [...self.origins.entries()].filter(
    ([, origin]) =>
      origin.span.sourceId === start.sourceId &&
      origin.span.start === start.start &&
      origin.span.end === start.end,
  )
  if (origins.length === 0) return undefined
  const created = BodyControlFlow.at(self.controlFlow, start)
  const accessed = BodyControlFlow.at(self.controlFlow, access)
  if (created === undefined || accessed === undefined) return undefined
  const at = write
    ? (BodyControlFlow.writeAt(self.controlFlow, access) ?? accessed.after)
    : accessed.before
  if (!BodyControlFlow.reaches(self.controlFlow, created.after, at, created.before)) return false
  let observedHolderUse = false
  for (const [key] of origins)
    for (const point of self.solution.required.get(key) ?? []) {
      const span = self.spans.get(point)
      if (span !== undefined && span.start >= start.end) observedHolderUse = true
      if (self.retiredUses.get(key)?.has(point)) continue
      const use = span === undefined ? undefined : BodyControlFlow.at(self.controlFlow, span)
      if (
        use !== undefined &&
        BodyControlFlow.reaches(self.controlFlow, at, use.after, created.before)
      )
        return true
    }
  const retainedEnd = BodyControlFlow.at(self.controlFlow, end)
  if (
    !observedHolderUse &&
    retainedEnd !== undefined &&
    BodyControlFlow.reaches(self.controlFlow, at, retainedEnd.after, created.before)
  )
    return true

  return false
}

const diagnosticsOf = (
  solution: Lifetime.Solution,
  origins: ReadonlyMap<string, Origin>,
  spans: ReadonlyMap<number, SourceSpan.SourceSpan>,
  fallback: SourceSpan.SourceSpan,
): ReadonlyArray<Diagnostic.Diagnostic> => {
  if (solution._tag !== 'Solved')
    return [
      Diagnostic.invalidLifetimeBinder(
        `Invalid finite lifetime domain: ${solution.dimension}`,
        fallback,
      ),
    ]
  const diagnostics = new Map<string, Diagnostic.Diagnostic>()
  for (const violation of solution.violations) {
    const origin = origins.get(Lifetime.key(violation.lifetime))
    const span = spans.get(violation.point) ?? fallback
    const diagnostic = Diagnostic.expiredLifetime(
      Lifetime.display(violation.lifetime),
      span,
      origin?.span,
    )
    diagnostics.set(`${Lifetime.key(violation.lifetime)}:${span.start}:${span.end}`, diagnostic)
  }
  return Object.freeze([...diagnostics.values()])
}

/** Retains only cleanup hooks which can observe initialized borrowed components. */
const cleanupLifetimes = (
  cleanup: CleanupPlan.CleanupPlan,
  state: MovePath.State,
): ReadonlyArray<Lifetime.Lifetime> => {
  if (state.initialization === 'Missing' && state.children.length === 0) return []
  if (cleanup._tag === 'HookCleanup' || cleanup._tag === 'ParameterCleanup')
    return Type.storageLifetimes(cleanup.type)
  const child = (selector: MovePath.Selector): MovePath.State =>
    state.children.find(
      (entry) =>
        entry.selector._tag === selector._tag &&
        (entry.selector._tag === 'ConstantIndex' && selector._tag === 'ConstantIndex'
          ? entry.selector.index === selector.index
          : entry.selector._tag !== 'ConstantIndex' &&
            selector._tag !== 'ConstantIndex' &&
            entry.selector.ordinal === selector.ordinal),
    )?.state ?? { initialization: state.initialization, children: [] }
  if (cleanup._tag === 'StructCleanup')
    return cleanup.fields.flatMap((field) =>
      cleanupLifetimes(field.cleanup, child({ _tag: 'Field', ordinal: field.field.ordinal })),
    )
  if (cleanup._tag === 'NominalUnionCleanup')
    return cleanup.variants.flatMap((variant) =>
      state.activeVariant !== undefined && state.activeVariant !== variant.ordinal
        ? []
        : variant.fields.flatMap((field) => {
            const variantState = child({ _tag: 'Variant', ordinal: variant.ordinal })
            const fieldState = variantState.children.find(
              (entry) =>
                entry.selector._tag === 'Field' && entry.selector.ordinal === field.field.ordinal,
            )?.state ?? { initialization: variantState.initialization, children: [] }
            return cleanupLifetimes(field.cleanup, fieldState)
          }),
    )
  if (cleanup._tag === 'ArrayCleanup') {
    const elements = state.children.filter((entry) => entry.selector._tag === 'ConstantIndex')
    const retained = elements.flatMap((entry) => cleanupLifetimes(cleanup.element, entry.state))
    // One inherited-state representative covers every unmentioned index, regardless of length.
    if (elements.length < cleanup.length)
      retained.push(
        ...cleanupLifetimes(cleanup.element, {
          initialization: state.initialization,
          children: [],
        }),
      )
    return retained
  }
  if (cleanup._tag === 'UnionCleanup')
    return cleanup.cases.flatMap((entry) => cleanupLifetimes(entry.cleanup, state))
  if (cleanup._tag === 'CallableCleanup' || cleanup._tag === 'EffectCleanup')
    return cleanup.slots.flatMap((slot) => cleanupLifetimes(slot.cleanup, state))
  if (cleanup._tag === 'EffectCompositeCleanup')
    return cleanup.alternatives.flatMap((alternative) => cleanupLifetimes(alternative, state))
  if (
    cleanup._tag === 'RepresentedCallableCleanup' ||
    cleanup._tag === 'RepresentedEffectCleanup' ||
    cleanup._tag === 'LocalSharedCoreCleanup'
  )
    return Type.storageLifetimes(cleanup.type)
  return []
}

const orderedSourcePoints = new WeakMap<
  LifetimeFlow,
  ReadonlyArray<readonly [number, SourceSpan.SourceSpan]>
>()

const cleanupSourcePoint = (
  self: LifetimeFlow,
  exit: SourceSpan.SourceSpan,
): number | undefined => {
  let points = orderedSourcePoints.get(self)
  if (points === undefined) {
    points = [...self.spans]
      .filter(([point]) => point < self.syntaxPointCount)
      .sort((left, right) => left[1].end - right[1].end || left[1].start - right[1].start)
    orderedSourcePoints.set(self, points)
  }
  let low = 0
  let high = points.length
  while (low < high) {
    const middle = Math.floor((low + high) / 2)
    const entry = points.at(middle)
    if (entry !== undefined && entry[1].end <= exit.end) low = middle + 1
    else high = middle
  }
  const selected = points.at(low - 1)
  return low > 0 && selected?.[1].sourceId === exit.sourceId ? selected[0] : undefined
}

/** Adds destructor uses from initialized remainders before checking conflicting place access. */
export const withCleanupUses = (
  self: LifetimeFlow,
  exits: ReadonlyArray<Ownership.ExitPlan>,
): LifetimeFlow => {
  const regions = new Map(
    self.input.regions.map((region) => [
      Lifetime.key(region.lifetime),
      {
        ...region,
        required: new Set(region.required),
      },
    ]),
  )
  let changed = false
  for (const exit of exits) {
    const required = exit.releases.flatMap((release) =>
      cleanupLifetimes(release.cleanup, release.initialization),
    )
    if (required.length === 0) continue
    // The final source point before the ordered release is a CFG point, so loan reachability
    // includes the cleanup without fabricating source statements or expanding partial states.
    const selected = cleanupSourcePoint(self, exit.span)
    if (selected === undefined) continue
    for (const lifetime of required) {
      const region = regions.get(Lifetime.key(lifetime))
      if (region !== undefined && !region.required.has(selected)) {
        region.required.add(selected)
        changed = true
      }
    }
  }
  if (!changed) return self
  const input = { ...self.input, regions: [...regions.values()] }
  return Object.freeze({ ...self, input, solution: Lifetime.solve(input) })
}

/** Checks actual branch-specific destruction after ownership has produced ordered releases. */
export const validateCleanup = (
  self: LifetimeFlow,
  ownership: Ownership.FunctionOwnership,
): {
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly work?: Lifetime.Work
} => {
  const regions = new Map(
    self.input.regions.map((region) => [
      Lifetime.key(region.lifetime),
      { ...region, available: new Set(region.available), required: new Set(region.required) },
    ]),
  )
  const spans = new Map(self.spans)
  let pointCount = self.input.pointCount
  for (const exit of ownership.exits) {
    const sourcePoint = cleanupSourcePoint(self, exit.span)
    const released = new Set<string>()
    for (const release of exit.releases) {
      const point = pointCount++
      spans.set(point, exit.span)
      for (const region of regions.values()) {
        const origin = self.origins.get(Lifetime.key(region.lifetime))
        if (origin?.root === undefined || !released.has(Ownership.siteKey(origin.root))) {
          if (
            region.lifetime._tag === 'StaticLifetime' ||
            (sourcePoint !== undefined && region.available.has(sourcePoint))
          )
            region.available.add(point)
        }
      }
      for (const lifetime of cleanupLifetimes(release.cleanup, release.initialization))
        regions.get(Lifetime.key(lifetime))?.required.add(point)
      released.add(Ownership.siteKey(release.binding.site))
    }
  }
  const solution = Lifetime.solve({ ...self.input, pointCount, regions: [...regions.values()] })
  return {
    diagnostics: diagnosticsOf(solution, self.origins, spans, ownership.declaration.syntax.span),
    ...(solution._tag === 'Solved' ? { work: solution.work } : {}),
  }
}

const sourceCache = new WeakMap<LifetimeFlow, Map<string, ReadonlyArray<Origin>>>()

/** Resolves every concrete loan contributing to a result through already checked lifetime edges. */
export const sources = (self: LifetimeFlow, type: Type.Type): ReadonlyArray<Origin> => {
  let cache = sourceCache.get(self)
  if (cache === undefined) {
    cache = new Map()
    sourceCache.set(self, cache)
  }
  const identity = Type.key(type)
  const cached = cache.get(identity)
  if (cached !== undefined) return cached
  const parents = new Map<string, Array<string>>()
  for (const bound of self.input.constraints) {
    const shorter = Lifetime.key(bound.shorter)
    const entries = parents.get(shorter) ?? []
    entries.push(Lifetime.key(bound.longer))
    parents.set(shorter, entries)
  }
  const pending = Type.storageLifetimes(type).map(Lifetime.key)
  const visited = new Set(pending)
  const result: Array<Origin> = []
  for (let cursor = 0; cursor < pending.length; cursor += 1) {
    const key = pending.at(cursor)
    if (key === undefined) continue
    const origin = self.origins.get(key)
    if (origin !== undefined) result.push(origin)
    for (const parent of parents.get(key) ?? [])
      if (!visited.has(parent)) {
        visited.add(parent)
        pending.push(parent)
      }
  }
  const ordered = Object.freeze(
    result.sort((left, right) => {
      const leftKey = Lifetime.key(left.lifetime)
      const rightKey = Lifetime.key(right.lifetime)
      if (leftKey < rightKey) return -1
      if (leftKey > rightKey) return 1
      return 0
    }),
  )
  cache.set(identity, ordered)
  return ordered
}
