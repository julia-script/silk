import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as Elaboration from './Elaboration.js'
import * as Canonical from './internal/Canonical.js'
import * as SyntaxTree from './SyntaxTree.js'
import * as Type from './Type.js'

/** One private runtime field retained by a concrete opaque callable or Effect construction. */
export interface Capture {
  readonly _tag: 'OpaqueCapture'
  readonly ordinal: number
  readonly type: Type.Type
  readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
}

/**
 * Compiler-private realization data for one declaration-owned opaque family.
 *
 * This record never participates in public source equality or module surfaces. Importing modules
 * may specialize it through `definitionOf` while tooling continues to expose only the family and
 * its public bound.
 */
export interface Definition {
  readonly _tag: 'OpaqueRealizationDefinition'
  readonly family: Type.OpaqueFamilyKey
  readonly instance: Type.OpaqueRepresentationArgument
  readonly parameters: ReadonlyArray<Type.Parameter>
  readonly realization: Type.RepresentationArgument
  readonly target:
    | Type.CallableIdentityArgument
    | Type.EffectIdentityArgument
    | Type.RepresentationParameterArgument
  readonly arguments: ReadonlyArray<Type.GenericArgument>
  readonly captures: ReadonlyArray<Capture>
  readonly access: 'Shared' | 'Exclusive' | 'Take'
  readonly cleanup: 'Trivial' | 'Required'
  readonly suspendable: boolean
  readonly bodyFingerprint: string
  readonly targetFingerprint: string
  readonly layoutFingerprint: string
}

/** Complete private opaque-definition catalog for one frontend revision. */
export interface Catalog {
  readonly _tag: 'OpaqueRealizationCatalog'
  readonly definitions: ReadonlyMap<string, Definition>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

/** Non-public object key used to carry private realization catalogs between compiler phases. */
export const catalogSymbol: unique symbol = Symbol.for(
  '@silk-effect/compiler/OpaqueRealizationCatalog',
)

/** A compiler phase artifact carrying private opaque definitions under the non-public symbol. */
export interface HasCatalog {
  readonly [catalogSymbol]: Catalog
}

/** Reads the private catalog inside compiler code and compiler-internal tests. */
export const catalogOf = (self: HasCatalog): Catalog => self[catalogSymbol]

interface Evidence {
  readonly argument: Type.RepresentationArgument
  readonly expression: Elaboration.ExpressionFact
}

interface Producer {
  readonly function: Elaboration.FunctionFact
  readonly instance: Type.OpaqueRepresentationArgument
  readonly evidence: ReadonlyArray<Evidence>
  readonly bodyFingerprint: string
}

const familyKey = (family: Type.OpaqueFamilyKey): string => Type.opaqueFamilyKey(family)

const returnExpressions = (
  statements: ReadonlyArray<Elaboration.StatementFact>,
): ReadonlyArray<Elaboration.ExpressionFact> => {
  const found: Array<Elaboration.ExpressionFact> = []
  const visitExpression = (expression: Elaboration.ExpressionFact): void => {
    if (expression._tag !== 'Match') {
      found.push(expression)
      return
    }
    for (const arm of expression.arms) if (arm.reachable) visitExpression(arm.result)
  }
  const visitStatements = (body: ReadonlyArray<Elaboration.StatementFact>): void => {
    for (const statement of body) {
      if (statement._tag === 'ReturnStatement') visitExpression(statement.expression)
      else if (statement._tag === 'UnsafeStatement') visitStatements(statement.statements)
      else if (statement._tag === 'IfStatement') {
        visitStatements(statement.taken)
        visitStatements(statement.otherwise)
      } else if (statement._tag === 'WhileStatement') visitStatements(statement.body)
    }
  }
  visitStatements(statements)
  return Object.freeze(found)
}

const evidenceOf = (
  expression: Elaboration.ExpressionFact,
  expected: Type.Type,
  family: Type.OpaqueFamilyKey,
): ReadonlyArray<Evidence> => {
  const structural =
    expression.type._tag === 'Available'
      ? Type.opaqueRepresentationEvidence(expression.type.type, expected, family)
      : Object.freeze([])
  if (structural.length > 0)
    return Object.freeze(structural.map((argument) => Object.freeze({ argument, expression })))
  const expectedArgument = Type.isRepresented(expected)
    ? expected.representation.argument
    : undefined
  if (
    expectedArgument === undefined ||
    !Type.isOpaqueRepresentationArgument(expectedArgument) ||
    !Type.equalsOpaqueFamily(expectedArgument.family, family)
  )
    return Object.freeze([])
  const argument = Elaboration.representationOfExpression(expression)
  return argument === undefined
    ? Object.freeze([])
    : Object.freeze([Object.freeze({ argument, expression })])
}

const sourceBodyFingerprint = (
  result: Elaboration.Result,
  declaration: DeclarationIndex.DeclarationFact,
): string => {
  const body = SyntaxTree.directNode(declaration.syntax, 'Block')
  const span = body?.span ?? declaration.syntax.span
  return Canonical.record('OpaqueBody', [
    declaration.id.sourceId,
    Array.from(result.syntax.source.bytes.slice(span.start, span.end)).join(','),
  ])
}

const producers = (results: ReadonlyMap<string, Elaboration.Result>): ReadonlyArray<Producer> =>
  Object.freeze(
    [...results.values()].flatMap((result) =>
      result.functions.flatMap((function_): ReadonlyArray<Producer> => {
        const opaque = function_.declaration.opaqueResult
        const expected = function_.declaration.returnType
        if (opaque === undefined || expected._tag !== 'Resolved') return []
        const instance = Type.opaqueRepresentationArguments(expected.type).find((argument) =>
          Type.equalsOpaqueFamily(argument.family, opaque.family),
        )
        if (instance === undefined) return []
        return [
          Object.freeze({
            function: function_,
            instance,
            evidence: Object.freeze(
              returnExpressions(function_.statements).flatMap((expression) =>
                evidenceOf(expression, expected.type, opaque.family),
              ),
            ),
            bodyFingerprint: sourceBodyFingerprint(result, function_.declaration),
          }),
        ]
      }),
    ),
  )

const constructionExpression = (
  expression: Elaboration.ExpressionFact,
): Elaboration.ExpressionFact => {
  if (expression._tag === 'Grouped') return constructionExpression(expression.expression)
  if (expression._tag === 'Move') return constructionExpression(expression.subject)
  if (expression._tag === 'Identifier' && expression.reference._tag === 'ResolvedBinding')
    return constructionExpression(expression.reference.binding.initializer)
  return expression
}

const captureType = (
  reference: Elaboration.BindingDeclarationFact | DeclarationIndex.ParameterFact,
): Type.Type | undefined => {
  if (reference._tag === 'BindingFact')
    return reference.inferredType._tag === 'Available' ? reference.inferredType.type : undefined
  return reference.declaredType._tag === 'Resolved' ? reference.declaredType.type : undefined
}

const capturesOf = (expression: Elaboration.ExpressionFact): ReadonlyArray<Capture> => {
  const construction = constructionExpression(expression)
  if (construction._tag === 'CallableSection')
    return Object.freeze(
      construction.captures.flatMap((capture): ReadonlyArray<Capture> => {
        if (capture.expression.type._tag !== 'Available') return []
        return [
          Object.freeze({
            _tag: 'OpaqueCapture',
            ordinal: capture.ordinal,
            type: capture.expression.type.type,
            access: capture.access,
          }),
        ]
      }),
    )
  if (construction._tag === 'EffectBlock')
    return Object.freeze(
      construction.captures.flatMap((capture, ordinal): ReadonlyArray<Capture> => {
        const type = captureType(capture.reference)
        return type === undefined
          ? []
          : [
              Object.freeze({
                _tag: 'OpaqueCapture',
                ordinal,
                type,
                access: capture.access,
              }),
            ]
      }),
    )
  return Object.freeze([])
}

const expressionSuspends = (expression: Elaboration.ExpressionFact): boolean => {
  if (expression._tag === 'Run') return true
  if (expression._tag === 'Grouped') return expressionSuspends(expression.expression)
  if (expression._tag === 'Move' || expression._tag === 'Borrow')
    return expressionSuspends(expression.subject)
  if (expression._tag === 'Match')
    return (
      expressionSuspends(expression.scrutinee) ||
      expression.arms.some(
        (arm) =>
          (arm.guard !== undefined && expressionSuspends(arm.guard)) ||
          expressionSuspends(arm.result),
      )
    )
  if (
    expression._tag === 'Call' ||
    expression._tag === 'Operator' ||
    expression._tag === 'ShortCircuit'
  )
    return expression.arguments.some((argument) => expressionSuspends(argument.expression))
  if (expression._tag === 'CallableApply')
    return (
      expressionSuspends(expression.callee) ||
      expression.arguments.some((argument) => expressionSuspends(argument.expression))
    )
  if (expression._tag === 'EffectBlock') return statementsSuspend(expression.statements)
  return false
}

const statementsSuspend = (statements: ReadonlyArray<Elaboration.StatementFact>): boolean =>
  statements.some((statement) => {
    if (statement._tag === 'UnsafeStatement') return statementsSuspend(statement.statements)
    if (statement._tag === 'IfStatement')
      return (
        expressionSuspends(statement.condition) ||
        statementsSuspend(statement.taken) ||
        statementsSuspend(statement.otherwise)
      )
    if (statement._tag === 'WhileStatement')
      return expressionSuspends(statement.condition) || statementsSuspend(statement.body)
    if (statement._tag === 'BindStatement') return expressionSuspends(statement.binding.initializer)
    if (statement._tag === 'ExpressionStatement') return expressionSuspends(statement.expression)
    if (statement._tag === 'WriteStatement')
      return expressionSuspends(statement.destination) || expressionSuspends(statement.value)
    if (
      statement._tag === 'ReturnStatement' ||
      statement._tag === 'FailStatement' ||
      statement._tag === 'DropStatement'
    )
      return expressionSuspends(statement.expression)
    return false
  })

const accessOf = (argument: Type.RepresentationArgument): Definition['access'] => {
  const contract =
    argument._tag === 'RepresentationParameterArgument'
      ? argument.parameter.representationBound
      : argument.contract
  return contract === undefined
    ? 'Shared'
    : Type.isCallable(contract)
      ? contract.mode
      : contract.access
}

const targetOf = (argument: Type.RepresentationArgument): Definition['target'] => {
  if (argument._tag === 'ExactRepresentationArgument') return argument.identity
  if (argument._tag === 'RepresentationParameterArgument') return argument
  throw new RangeError('An opaque realization cannot target another unresolved opaque family')
}

const argumentsOf = (argument: Type.RepresentationArgument): ReadonlyArray<Type.GenericArgument> =>
  argument._tag === 'ExactRepresentationArgument' &&
  Type.isCallableIdentityArgument(argument.identity)
    ? argument.identity.typeArguments
    : argument._tag === 'OpaqueRepresentationArgument'
      ? argument.arguments
      : Object.freeze([])

const definition = (
  producer: Producer,
  realization: Type.RepresentationArgument,
  source: Elaboration.ExpressionFact | undefined,
  inherited: Definition | undefined,
): Definition => {
  const captures =
    source === undefined ? (inherited?.captures ?? Object.freeze([])) : capturesOf(source)
  const access = accessOf(realization)
  const cleanup = captures.some((capture) => capture.access === 'Take') ? 'Required' : 'Trivial'
  const suspendable =
    source === undefined ? (inherited?.suspendable ?? false) : expressionSuspends(source)
  const targetFingerprint = Canonical.record('OpaqueTarget', [Type.genericArgumentKey(realization)])
  const layoutFingerprint = Canonical.record('OpaqueLayout', [
    access,
    cleanup,
    String(suspendable),
    Canonical.array(
      captures.map((capture) =>
        Canonical.record('Capture', [
          String(capture.ordinal),
          Type.key(capture.type),
          capture.access,
        ]),
      ),
    ),
  ])
  return Object.freeze({
    _tag: 'OpaqueRealizationDefinition',
    family: producer.instance.family,
    instance: producer.instance,
    parameters: Object.freeze(
      producer.function.declaration.typeParameters.map((parameter) => parameter.type),
    ),
    realization,
    target: targetOf(realization),
    arguments: argumentsOf(realization),
    captures,
    access,
    cleanup,
    suspendable,
    bodyFingerprint: producer.bodyFingerprint,
    targetFingerprint,
    layoutFingerprint,
  })
}

/** Builds all private definitions and diagnoses non-finite or divergent opaque families. */
export const analyze = (results: ReadonlyMap<string, Elaboration.Result>): Catalog => {
  const pending = producers(results)
  const resolved = new Map<string, ReadonlyMap<string, Type.RepresentationArgument>>()
  for (const producer of pending) {
    const leaves = new Map<string, Type.RepresentationArgument>()
    for (const evidence of producer.evidence) {
      if (evidence.argument._tag === 'OpaqueRepresentationArgument') continue
      leaves.set(Type.genericArgumentKey(evidence.argument), evidence.argument)
    }
    resolved.set(familyKey(producer.instance.family), leaves)
  }

  let changed = true
  while (changed) {
    changed = false
    for (const producer of pending) {
      const key = familyKey(producer.instance.family)
      const current = new Map(resolved.get(key) ?? [])
      for (const evidence of producer.evidence) {
        if (evidence.argument._tag !== 'OpaqueRepresentationArgument') continue
        for (const [identity, argument] of resolved.get(familyKey(evidence.argument.family)) ?? [])
          current.set(identity, argument)
      }
      if (current.size !== (resolved.get(key)?.size ?? 0)) {
        resolved.set(key, current)
        changed = true
      }
    }
  }

  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const definitions = new Map<string, Definition>()
  const invalid = new Set<string>()
  for (const producer of pending) {
    const key = familyKey(producer.instance.family)
    const alternatives = [...(resolved.get(key)?.entries() ?? [])].sort(([left], [right]) =>
      left < right ? -1 : left > right ? 1 : 0,
    )
    if (alternatives.length > 1) {
      invalid.add(key)
      diagnostics.push(
        Diagnostic.divergentOpaqueRealization(
          key,
          alternatives.map(([identity]) => identity),
          producer.evidence.map((evidence) => evidence.expression.syntax.span),
          producer.function.declaration.opaqueResult?.syntax.span ??
            producer.function.declaration.syntax.span,
        ),
      )
    } else if (alternatives.length === 0) {
      invalid.add(key)
      const dependencies = producer.evidence.flatMap((evidence) =>
        evidence.argument._tag === 'OpaqueRepresentationArgument'
          ? [familyKey(evidence.argument.family)]
          : [],
      )
      diagnostics.push(
        Diagnostic.opaqueRealizationCycle(
          Object.freeze([key, ...dependencies]),
          producer.function.declaration.opaqueResult?.syntax.span ??
            producer.function.declaration.syntax.span,
        ),
      )
    }
  }

  let progress = true
  while (progress) {
    progress = false
    for (const producer of pending) {
      const key = familyKey(producer.instance.family)
      if (invalid.has(key) || definitions.has(key)) continue
      const realization = [...(resolved.get(key)?.values() ?? [])].at(0)
      if (realization === undefined) continue
      const direct = producer.evidence.find(
        (evidence) =>
          evidence.argument._tag !== 'OpaqueRepresentationArgument' &&
          Type.equalsGenericArgument(evidence.argument, realization),
      )
      const dependency = producer.evidence
        .filter(
          (
            evidence,
          ): evidence is Evidence & { readonly argument: Type.OpaqueRepresentationArgument } =>
            evidence.argument._tag === 'OpaqueRepresentationArgument',
        )
        .map((evidence) => definitions.get(familyKey(evidence.argument.family)))
        .find((candidate) => candidate !== undefined)
      if (direct === undefined && dependency === undefined) continue
      const built = definition(producer, realization, direct?.expression, dependency)
      if (
        built.captures.some((capture) =>
          Type.opaqueRepresentationArguments(capture.type).some((argument) =>
            Type.equalsOpaqueFamily(argument.family, built.family),
          ),
        )
      ) {
        invalid.add(key)
        diagnostics.push(
          Diagnostic.inlineOpaqueLayoutCycle(
            key,
            direct?.expression.syntax.span ?? producer.function.declaration.syntax.span,
          ),
        )
        continue
      }
      definitions.set(key, built)
      progress = true
    }
  }

  return Object.freeze({
    _tag: 'OpaqueRealizationCatalog',
    definitions: new Map([...definitions].sort(([left], [right]) => (left < right ? -1 : 1))),
    diagnostics: Diagnostic.merge(diagnostics),
  })
}

/** Looks up and specializes one compiler-private definition for an opaque family instance. */
export const definitionOf = (
  self: Catalog,
  instance: Type.OpaqueRepresentationArgument,
): Definition | undefined => {
  const found = self.definitions.get(familyKey(instance.family))
  if (found === undefined) return undefined
  const substitution = Type.substitution(found.parameters, instance.arguments)
  if (substitution === undefined) return found
  const realization = Type.substituteGenericArgument(found.realization, substitution)
  if (!Type.isRepresentationArgument(realization)) return undefined
  const captures = Object.freeze(
    found.captures.map((capture) =>
      Object.freeze({ ...capture, type: Type.substitute(capture.type, substitution) }),
    ),
  )
  const access = accessOf(realization)
  const cleanup = captures.some((capture) => capture.access === 'Take') ? 'Required' : 'Trivial'
  return Object.freeze({
    ...found,
    instance,
    realization,
    target: targetOf(realization),
    arguments: argumentsOf(realization),
    captures,
    access,
    cleanup,
    targetFingerprint: Canonical.record('OpaqueTarget', [Type.genericArgumentKey(realization)]),
    layoutFingerprint: Canonical.record('OpaqueLayout', [
      access,
      cleanup,
      String(found.suspendable),
      Canonical.array(
        captures.map((capture) =>
          Canonical.record('Capture', [
            String(capture.ordinal),
            Type.key(capture.type),
            capture.access,
          ]),
        ),
      ),
    ]),
  })
}

/** Stable source family identity used by incremental dependency maps and test fixtures. */
export const key = (self: Definition): string => familyKey(self.family)

/** One privacy-preserving dependency summary that intentionally omits target and capture details. */
export const publicOrigin = (
  self: Definition,
): {
  readonly family: string
  readonly bound: string
} => Object.freeze({ family: key(self), bound: Type.key(self.instance.contract) })
