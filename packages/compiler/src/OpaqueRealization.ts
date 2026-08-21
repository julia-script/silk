import { stronglyConnected } from './internal/Graph.js'
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

/** The actual producer specialization and executable site that constructed one realization. */
export interface Construction {
  readonly _tag: 'OpaqueConstruction'
  readonly producer: { readonly module: string; readonly name: string }
  readonly arguments: ReadonlyArray<Type.GenericArgument>
  readonly site?: string
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
  readonly construction: Construction
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

const catalogs = new WeakMap<object, Catalog>()

/** Associates a compiler phase artifact with its private opaque definitions. */
export const withCatalog = <A extends object>(self: A, catalog: Catalog): A => {
  catalogs.set(self, catalog)
  return self
}

/** Reads the private catalog inside compiler code and compiler-internal tests. */
export const catalogOf = (self: object): Catalog => {
  const catalog = catalogs.get(self)
  if (catalog === undefined)
    throw new RangeError('Compiler artifact has no opaque realization catalog')
  return catalog
}

/** Reads an associated private catalog when an artifact has retained its compiler identity. */
export const catalogOption = (self: object): Catalog | undefined => catalogs.get(self)

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

const reachableResults = (
  expression: Elaboration.ExpressionFact,
): ReadonlyArray<Elaboration.ExpressionFact> => {
  if (expression._tag === 'Grouped') return reachableResults(expression.expression)
  if (expression._tag !== 'Match') return Object.freeze([expression])
  return Object.freeze(
    expression.arms.flatMap((arm) => (arm.reachable ? reachableResults(arm.result) : [])),
  )
}

const returnExpressions = (
  statements: ReadonlyArray<Elaboration.StatementFact>,
): ReadonlyArray<Elaboration.ExpressionFact> => {
  const found: Array<Elaboration.ExpressionFact> = []
  Elaboration.visitStatementFacts(statements, {
    descendExpressions: false,
    statement: (statement) => {
      if (statement._tag === 'ReturnStatement')
        found.push(...reachableResults(statement.expression))
    },
  })
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
  const nestedFamily = Type.opaqueRepresentationArguments(expected).some((argument) =>
    Type.equalsOpaqueFamily(argument.family, family),
  )
  if (nestedFamily) {
    const argument = Elaboration.representationOfExpression(expression)
    if (argument !== undefined) return Object.freeze([Object.freeze({ argument, expression })])
  }
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
  let suspendable = false
  Elaboration.visitExpressionFacts(expression, {
    expression: (current) => {
      if (current._tag === 'Run') suspendable = true
    },
  })
  return suspendable
}

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

const constructionSite = (argument: Type.RepresentationArgument): string | undefined => {
  if (argument._tag !== 'ExactRepresentationArgument') return undefined
  return Type.isCallableIdentityArgument(argument.identity)
    ? argument.identity.environment === undefined
      ? undefined
      : Type.callableEnvironmentKey(argument.identity.environment)
    : argument.identity.identity
}

const constructionOf = (
  producer: Producer,
  realization: Type.RepresentationArgument,
  inherited: Definition | undefined,
): Construction => {
  if (inherited !== undefined) return inherited.construction
  const site = constructionSite(realization)
  return Object.freeze({
    _tag: 'OpaqueConstruction',
    producer: Object.freeze({ ...producer.instance.family.producer }),
    arguments: Object.freeze([...producer.instance.arguments]),
    ...(site === undefined ? {} : { site }),
  })
}

const fingerprints = (
  realization: Type.RepresentationArgument,
  captures: ReadonlyArray<Capture>,
  access: Definition['access'],
  cleanup: Definition['cleanup'],
  suspendable: boolean,
): Pick<Definition, 'targetFingerprint' | 'layoutFingerprint'> =>
  Object.freeze({
    targetFingerprint: Canonical.record('OpaqueTarget', [Type.genericArgumentKey(realization)]),
    layoutFingerprint: Canonical.record('OpaqueLayout', [
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
    ]),
  })

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
  const computedFingerprints = fingerprints(realization, captures, access, cleanup, suspendable)
  return Object.freeze({
    _tag: 'OpaqueRealizationDefinition',
    family: producer.instance.family,
    instance: producer.instance,
    parameters: Object.freeze(
      producer.function.declaration.typeParameters.map((parameter) => parameter.type),
    ),
    realization,
    construction: constructionOf(producer, realization, inherited),
    target: targetOf(realization),
    arguments: argumentsOf(realization),
    captures,
    access,
    cleanup,
    suspendable,
    bodyFingerprint: producer.bodyFingerprint,
    ...computedFingerprints,
  })
}

const specializeRealization = (
  producer: Producer,
  instance: Type.OpaqueRepresentationArgument,
  realization: Type.RepresentationArgument,
): Type.RepresentationArgument | undefined => {
  const substitution = Type.substitution(
    producer.function.declaration.typeParameters.map((parameter) => parameter.type),
    instance.arguments,
  )
  if (substitution === undefined) return undefined
  const specialized = Type.substituteGenericArgument(realization, substitution)
  return Type.isRepresentationArgument(specialized) ? specialized : undefined
}

const specializeDefinition = (
  found: Definition,
  instance: Type.OpaqueRepresentationArgument,
): Definition | undefined => {
  const substitution = Type.substitution(found.parameters, instance.arguments)
  if (substitution === undefined) return undefined
  const realization = Type.substituteGenericArgument(found.realization, substitution)
  if (!Type.isRepresentationArgument(realization)) return undefined
  const captures = Object.freeze(
    found.captures.map((capture) =>
      Object.freeze({ ...capture, type: Type.substitute(capture.type, substitution) }),
    ),
  )
  const constructionArguments = Object.freeze(
    found.construction.arguments.map((argument) =>
      Type.substituteGenericArgument(argument, substitution),
    ),
  )
  const site = constructionSite(realization)
  const access = accessOf(realization)
  const cleanup = captures.some((capture) => capture.access === 'Take') ? 'Required' : 'Trivial'
  return Object.freeze({
    ...found,
    instance,
    realization,
    construction: Object.freeze({
      _tag: 'OpaqueConstruction',
      producer: found.construction.producer,
      arguments: constructionArguments,
      ...(site === undefined ? {} : { site }),
    }),
    target: targetOf(realization),
    arguments: argumentsOf(realization),
    captures,
    access,
    cleanup,
    ...fingerprints(realization, captures, access, cleanup, found.suspendable),
  })
}


const inlineLayoutCycles = (
  definitions: ReadonlyMap<string, Definition>,
): ReadonlyArray<ReadonlyArray<string>> =>
  stronglyConnected(definitions.keys(), (key) =>
    (definitions.get(key)?.captures ?? []).flatMap((capture) =>
      Type.opaqueRepresentationArguments(capture.type).map((argument) =>
        familyKey(argument.family),
      ),
    ),
  )

const unresolvedRealizationCycles = (
  pending: ReadonlyArray<Producer>,
): ReadonlyArray<ReadonlyArray<string>> => {
  const byFamily = new Map(
    pending.map((producer) => [familyKey(producer.instance.family), producer]),
  )
  return stronglyConnected(byFamily.keys(), (key) =>
    (byFamily.get(key)?.evidence ?? []).flatMap((evidence) =>
      evidence.argument._tag === 'OpaqueRepresentationArgument'
        ? [familyKey(evidence.argument.family)]
        : [],
    ),
  )
}

/** Builds all private definitions and diagnoses non-finite or divergent opaque families. */
export const analyze = (results: ReadonlyMap<string, Elaboration.Result>): Catalog => {
  const pending = producers(results)
  const producersByFamily = new Map(
    pending.map((producer) => [familyKey(producer.instance.family), producer]),
  )
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
        const dependencyKey = familyKey(evidence.argument.family)
        const dependency = producersByFamily.get(dependencyKey)
        if (dependency === undefined) continue
        for (const argument of resolved.get(dependencyKey)?.values() ?? []) {
          const specialized = specializeRealization(dependency, evidence.argument, argument)
          if (specialized !== undefined)
            current.set(Type.genericArgumentKey(specialized), specialized)
        }
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
  const realizationCycles = unresolvedRealizationCycles(
    pending.filter(
      (producer) => (resolved.get(familyKey(producer.instance.family))?.size ?? 0) === 0,
    ),
  )
  const cyclic = new Set(realizationCycles.flat())
  for (const cycle of realizationCycles) {
    for (const key of cycle) invalid.add(key)
    const producer = producersByFamily.get(cycle.at(0) ?? '')
    if (producer === undefined) continue
    diagnostics.push(
      Diagnostic.opaqueRealizationCycle(
        cycle,
        producer.function.declaration.opaqueResult?.syntax.span ??
          producer.function.declaration.syntax.span,
      ),
    )
  }
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
    } else if (alternatives.length === 0 && !cyclic.has(key)) {
      invalid.add(key)
      diagnostics.push(
        Diagnostic.missingOpaqueRealization(
          key,
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
        .map((evidence) => {
          const found = definitions.get(familyKey(evidence.argument.family))
          return found === undefined ? undefined : specializeDefinition(found, evidence.argument)
        })
        .filter(
          (candidate): candidate is Definition =>
            candidate !== undefined &&
            Type.equalsGenericArgument(candidate.realization, realization),
        )
        .find((candidate) => candidate !== undefined)
      if (direct === undefined && dependency === undefined) continue
      const built = definition(producer, realization, direct?.expression, dependency)
      definitions.set(key, built)
      progress = true
    }
  }

  for (const cycle of inlineLayoutCycles(definitions)) {
    for (const key of cycle) {
      definitions.delete(key)
      invalid.add(key)
    }
    const producer = producersByFamily.get(cycle.at(0) ?? '')
    if (producer === undefined) continue
    diagnostics.push(
      Diagnostic.inlineOpaqueLayoutCycle(
        cycle,
        producer?.function.declaration.opaqueResult?.syntax.span ??
          producer.function.declaration.syntax.span,
      ),
    )
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
  return found === undefined ? undefined : specializeDefinition(found, instance)
}

/** Stable source family identity used by incremental dependency maps and test fixtures. */
export const key = (self: Definition): string => familyKey(self.family)
