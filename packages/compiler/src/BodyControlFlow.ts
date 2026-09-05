import * as Elaboration from './Elaboration.js'
import type * as Hir from './Hir.js'
import type * as SourceSpan from './SourceSpan.js'
import type * as SyntaxTree from './SyntaxTree.js'
import * as Type from './Type.js'

/** Evaluation boundaries for one retained source node. */
export interface Boundary {
  readonly before: number
  readonly after: number
}

/** Finite semantic control flow, independent of source offsets and backend lowering. */
export interface BodyControlFlow {
  readonly boundaries: ReadonlyMap<SyntaxTree.Node, Boundary>
  readonly spans: ReadonlyMap<string, Boundary>
  readonly writes: ReadonlyMap<string, number>
  readonly edges: ReadonlyArray<ReadonlyArray<number>>
  readonly queries: Map<string, ReadonlySet<number>>
  readonly work: { queries: number; cacheHits: number; visitedEdges: number }
}

const spanKey = (span: SourceSpan.SourceSpan): string =>
  `${span.sourceId}:${span.start}:${span.end}`
const loopKey = (loop: Hir.LoopId): string =>
  `${loop.function.sourceId}:${loop.function.ordinal}:${loop.ordinal}`

/** Builds structured branch exits and loop backedges once for one source body. */
export const make = (
  statements: ReadonlyArray<Elaboration.StatementFact>,
  root: SyntaxTree.Node,
): BodyControlFlow => {
  const edges: Array<Array<number>> = []
  const boundaries = new Map<SyntaxTree.Node, Boundary>()
  const spans = new Map<string, Boundary>()
  const writes = new Map<string, number>()
  const point = (): number => {
    edges.push([])
    return edges.length - 1
  }
  const edge = (from: number, to: number): void => {
    edges.at(from)?.push(to)
  }
  const boundary = (syntax: SyntaxTree.Node): Boundary => {
    const found = boundaries.get(syntax)
    if (found !== undefined) return found
    const value = { before: point(), after: point() }
    boundaries.set(syntax, value)
    spans.set(spanKey(syntax.span), value)
    return value
  }
  type Loops = ReadonlyMap<string, { readonly exit: number; readonly repeat: number }>
  const expression = (value: Elaboration.ExpressionFact, next: number, loops: Loops): number => {
    const own = boundary(value.syntax)
    if (value.type._tag !== 'Available' || !Type.isNever(value.type.type)) edge(own.after, next)
    if (value._tag === 'Match') {
      const dispatch = point()
      let fallback = own.after
      for (const arm of [...value.arms].reverse()) {
        if (!arm.reachable) continue
        const armBody =
          arm.body._tag === 'Expression'
            ? expression(arm.body.expression, own.after, loops)
            : sequence(arm.body.statements, own.after, loops)
        let selected = armBody
        if (arm.guard !== undefined) {
          const choice = point()
          edge(choice, armBody)
          edge(choice, fallback)
          selected = expression(arm.guard, choice, loops)
        }
        const entered = boundary(arm.syntax)
        edge(entered.before, selected)
        edge(dispatch, entered.before)
        fallback = entered.before
      }
      edge(own.before, expression(value.scrutinee, dispatch, loops))
    } else if (value._tag === 'ShortCircuit') {
      const first = value.arguments.at(0)?.expression
      const second = value.arguments.at(1)?.expression
      const choice = point()
      edge(choice, own.after)
      if (second !== undefined) edge(choice, expression(second, own.after, loops))
      edge(own.before, first === undefined ? choice : expression(first, choice, loops))
    } else if (value._tag === 'EffectBlock') {
      // Deferred bodies have their own entry; creation does not execute their statements.
      sequence(value.statements, point(), new Map())
      let start = own.after
      for (const capture of [...value.captures].reverse())
        if (capture.expression !== undefined) start = expression(capture.expression, start, loops)
      edge(own.before, start)
    } else {
      let start = own.after
      for (const child of [...Elaboration.expressionChildren(value)].reverse())
        start = expression(child, start, loops)
      edge(own.before, start)
    }
    return own.before
  }
  const sequence = (
    values: ReadonlyArray<Elaboration.StatementFact>,
    next: number,
    loops: Loops,
  ): number => {
    let start = next
    for (const statement of [...values].reverse()) {
      const syntax =
        statement._tag === 'BindStatement' ? statement.binding.syntax : statement.syntax
      const own = boundary(syntax)
      const previous = start
      start = own.before
      if (statement._tag === 'ReturnStatement' || statement._tag === 'FailStatement') {
        edge(own.before, expression(statement.expression, own.after, loops))
      } else if (statement._tag === 'BreakStatement' || statement._tag === 'ContinueStatement') {
        const target =
          statement.target === undefined ? undefined : loops.get(loopKey(statement.target))
        edge(own.before, own.after)
        if (target !== undefined)
          edge(own.after, statement._tag === 'BreakStatement' ? target.exit : target.repeat)
      } else if (statement._tag === 'WhileStatement') {
        const choice = point()
        const condition = expression(statement.condition, choice, loops)
        const nested = new Map(loops).set(loopKey(statement.loop), {
          exit: own.after,
          repeat: condition,
        })
        const body = sequence(statement.body, condition, nested)
        if (statement.condition._tag !== 'Boolean' || statement.condition.value) edge(choice, body)
        if (statement.condition._tag !== 'Boolean' || !statement.condition.value)
          edge(choice, own.after)
        edge(own.before, condition)
        edge(own.after, previous)
      } else if (statement._tag === 'IfStatement' || statement._tag === 'IfLetStatement') {
        const choice = point()
        const test =
          statement._tag === 'IfStatement' ? statement.condition : statement.selection.source
        const taken = sequence(statement.taken, own.after, loops)
        const otherwise = sequence(statement.otherwise, own.after, loops)
        if (test._tag !== 'Boolean' || test.value) edge(choice, taken)
        if (test._tag !== 'Boolean' || !test.value) edge(choice, otherwise)
        edge(own.before, expression(test, choice, loops))
        edge(own.after, previous)
      } else if (statement._tag === 'UnsafeStatement') {
        edge(own.before, sequence(statement.statements, own.after, loops))
        edge(own.after, previous)
      } else {
        let evaluated = own.after
        let values: ReadonlyArray<Elaboration.ExpressionFact>
        if (statement._tag === 'BindStatement') values = [statement.binding.initializer]
        else if (statement._tag === 'WriteStatement')
          values = [statement.destination, statement.value]
        else if (statement._tag === 'PatternBindStatement') values = [statement.selection.source]
        else values = [statement.expression]
        for (const value of [...values].reverse()) evaluated = expression(value, evaluated, loops)
        if (statement._tag === 'WriteStatement')
          writes.set(spanKey(statement.destination.syntax.span), own.after)
        edge(own.before, evaluated)
        edge(own.after, previous)
      }
    }
    return start
  }
  const body = boundary(root)
  edge(body.before, sequence(statements, body.after, new Map()))
  return {
    boundaries,
    spans,
    writes,
    edges,
    queries: new Map(),
    work: { queries: 0, cacheHits: 0, visitedEdges: 0 },
  }
}

/** Finds an exact semantic boundary; unexecuted annotation syntax has none. */
export const at = (self: BodyControlFlow, span: SourceSpan.SourceSpan): Boundary | undefined =>
  self.spans.get(spanKey(span))

/** Resolves the installation point of a writable place after its RHS has evaluated. */
export const writeAt = (self: BodyControlFlow, span: SourceSpan.SourceSpan): number | undefined =>
  self.writes.get(spanKey(span))

/** Lazily reuses reachability from requested starts; barriers stop re-creation of a loan. */
export const reaches = (
  self: BodyControlFlow,
  from: number,
  to: number,
  barrier?: number | ReadonlyArray<number>,
): boolean => {
  self.work.queries += 1
  const barriers = new Set(typeof barrier === 'number' ? [barrier] : (barrier ?? []))
  const key = `${from}:${[...barriers].sort((left, right) => left - right).join(',')}`
  const cached = self.queries.get(key)
  if (cached !== undefined) {
    self.work.cacheHits += 1
    return cached.has(to)
  }
  const pending = [from]
  const visited = new Set<number>()
  while (pending.length > 0) {
    const current = pending.pop()
    if (current === undefined || barriers.has(current) || visited.has(current)) continue
    visited.add(current)
    for (const next of self.edges.at(current) ?? []) {
      self.work.visitedEdges += 1
      pending.push(next)
    }
  }
  self.queries.set(key, visited)
  return visited.has(to)
}
