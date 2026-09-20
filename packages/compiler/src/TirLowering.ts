import * as Lifetime from './Lifetime.js'
import { callableSectionOf, genericArgumentOfTypeArgument } from './CallResolution.js'
import type * as AuthoredHir from './AuthoredHir.js'
import * as Constraint from './Constraint.js'
import * as Diagnostic from './Diagnostic.js'
import type * as Location from './Location.js'
import type {
  ArgumentFact,
  AssignmentRootFact,
  CallReferenceFact,
  DeclarationId,
  ExpressionFact,
  ExpressionTypeFact,
  ParameterReferenceFact,
  PatternSelectionFact,
  SemanticType,
  StatementFact,
} from './Elaboration.js'
import {
  assignmentRootAccess,
  contextualIntegerCompatible,
  retainedResultArguments,
  retainsLifetimes,
} from './Elaboration.js'
import { representationOfExpression } from './ExpressionAnalysis.js'
import * as Tir from './Tir.js'
import * as Intrinsic from './Intrinsic.js'
import * as TypeInference from './internal/TypeInference.js'
import * as SemanticContext from './SemanticContext.js'
import * as Match from './Match.js'
import * as Scalar from './Scalar.js'
import { executableStatements } from './StatementAnalysis.js'
import type * as StaticText from './StaticText.js'
import type * as StaticValue from './StaticValue.js'
import * as Type from './Type.js'
import * as TypeCompatibility from './TypeCompatibility.js'
import * as BodyBuilder from './BodyBuilder.js'

const publishedCause = (
  builder: BodyBuilder.BodyBuilder | undefined,
  cause: Diagnostic.Identity<Location.Location>,
): { readonly cause: Tir.CauseRef } => {
  if (builder === undefined) throw new RangeError('TIR cause requires its body builder')
  return { cause: BodyBuilder.cause(builder, cause) }
}

const publishedEvidence = (
  builder: BodyBuilder.BodyBuilder | undefined,
  evidence: ReadonlyArray<Constraint.ConstraintEvidence>,
): Tir.EvidenceRef => {
  if (builder === undefined) throw new RangeError('TIR evidence requires its body builder')
  return BodyBuilder.selectedEvidence(builder, evidence)
}

export const tirReference = (
  reference: ParameterReferenceFact,
  type: ExpressionTypeFact,
  anchor: AuthoredHir.Anchor,
  context: SemanticContext.SemanticContext,
  builder?: BodyBuilder.BodyBuilder,
): Tir.Expression => {
  const span = context.spanOf(anchor)
  const origin = Tir.authored(anchor)
  if (reference._tag === 'Resolved' && type._tag === 'Available') {
    return Object.freeze({
      _tag: 'ParameterReference',
      parameter: reference.parameter.id,
      type: type.type,
      span,
      origin,
    })
  }
  if (reference._tag === 'ResolvedBinding' && type._tag === 'Available') {
    return Object.freeze({
      _tag: 'BindingReference',
      binding: reference.binding.id,
      type: type.type,
      span,
      origin,
    })
  }
  if (reference._tag === 'ResolvedPattern' && type._tag === 'Available') {
    return Object.freeze({
      _tag: 'PatternBindingReference',
      binding: reference.binding.id,
      type: type.type,
      span,
      origin,
    })
  }
  return Object.freeze({
    _tag: 'Unavailable',
    span,
    origin,
    ...(reference._tag === 'Missing' && reference.cause !== undefined
      ? publishedCause(builder, reference.cause)
      : {}),
  })
}

/** The literal nodes of a value the evaluator produced for the authored position `anchor`. */
const staticValueExpression = (
  value: StaticValue.Value,
  type: SemanticType,
  anchor: AuthoredHir.Anchor,
  context: SemanticContext.SemanticContext,
): Tir.Expression => {
  const span = context.spanOf(anchor)
  const origin = Tir.synthetic(anchor, 'static-result')
  switch (value._tag) {
    case 'UnitValue':
      return Object.freeze({ _tag: 'UnitLiteral', type: Type.unit, span, origin })
    case 'BooleanValue':
      return Object.freeze({
        _tag: 'BooleanLiteral',
        value: value.value,
        type: 'bool',
        span,
        origin,
      })
    case 'CharacterValue':
      return Object.freeze({
        _tag: 'CharacterLiteral',
        value: value.value,
        type: 'char',
        span,
        origin,
      })
    case 'IntegerValue':
      return Object.freeze({
        _tag: 'IntegerLiteral',
        value: value.value,
        type: value.type,
        span,
        origin,
      })
    case 'FloatValue':
      return Object.freeze({
        _tag: 'FloatingLiteral',
        bits: value.bits,
        spelling: `${value.type}(bits=0x${value.bits.toString(16)})`,
        type: value.type,
        span,
        origin,
      })
    case 'TextValue': {
      const data: StaticText.Data = Object.freeze({
        _tag: 'StaticData',
        id: `text:${value.bytes.map((byte) => byte.toString(16).padStart(2, '0')).join('')}`,
        kind: 'Text',
        bytes: value.bytes,
        utf8: true,
      })
      return Object.freeze({
        _tag: 'StaticStringLiteral',
        data,
        type: Type.string(Lifetime.staticLifetime),
        span,
        origin,
      })
    }
    case 'EnumValue':
      return Type.isNominal(type)
        ? Object.freeze({
            _tag: 'EnumMember',
            enum: value.type,
            member: Object.freeze({
              _tag: 'CanonicalEnumMemberId',
              enum: value.type,
              name: value.member,
            }),
            discriminant: value.discriminant,
            type,
            span,
            origin,
          })
        : Object.freeze({ _tag: 'Unavailable', span, origin })
    case 'AggregateValue': {
      if (value.identity._tag === 'ArrayAggregateIdentity' && Type.isFixedArray(type)) {
        const elements = value.fields.map((field) =>
          staticValueExpression(field.value, type.element, anchor, context),
        )
        return elements.some((element) => element._tag === 'Unavailable')
          ? Object.freeze({ _tag: 'Unavailable', span, origin })
          : Object.freeze({ _tag: 'ArrayConstruct', elements, type, span, origin })
      }
      if (
        value.identity._tag !== 'NominalAggregateIdentity' ||
        !Type.isNominal(type) ||
        value.runtimeFields === undefined ||
        value.runtimeFields.length !== value.fields.length
      )
        return Object.freeze({ _tag: 'Unavailable', span, origin })
      const fields = value.fields.flatMap((field) => {
        const runtime = value.runtimeFields?.find(
          (candidate) => candidate.id.ordinal === field.ordinal,
        )
        if (runtime === undefined) return []
        const expression = staticValueExpression(field.value, runtime.type, anchor, context)
        return expression._tag === 'Unavailable'
          ? []
          : [Object.freeze({ field: runtime.id, value: expression })]
      })
      if (fields.length !== value.fields.length)
        return Object.freeze({ _tag: 'Unavailable', span, origin })
      const evaluationOrder = Object.freeze(fields.map((field) => field.field))
      const variant = value.identity.variant
      return variant === undefined
        ? Object.freeze({
            _tag: 'Construct',
            nominal: type,
            evaluationOrder,
            fields: Object.freeze(fields),
            type,
            span,
            origin,
          })
        : Object.freeze({
            _tag: 'ConstructUnionVariant',
            nominal: type,
            variant: Object.freeze({
              _tag: 'CanonicalUnionVariantId',
              union: value.identity.declaration,
              name: variant.name,
            }),
            variantOrdinal: variant.ordinal,
            evaluationOrder,
            fields: Object.freeze(fields),
            type,
            span,
            origin,
          })
    }
    case 'TypeDescriptorValue':
    case 'FieldDescriptorValue':
    case 'FieldCollectionValue':
    case 'StaticSequenceValue':
      return Object.freeze({ _tag: 'Unavailable', span, origin })
  }
}

export const tirPatternSelection = (
  selection: PatternSelectionFact,
  options: LowerStatementOptions,
): Tir.PatternSelection => {
  let member: Match.CoverageIdentity | undefined
  if (selection.pattern._tag === 'EnumMemberPattern') {
    member = selection.pattern.coverage
  } else if (selection.pattern._tag === 'UnionVariantPattern') {
    member = selection.pattern.coverage
  } else if (
    (selection.pattern._tag === 'NominalPattern' || selection.pattern._tag === 'TypePattern') &&
    selection.pattern.member !== undefined
  ) {
    member = Match.structuralMember(selection.pattern.member)
  }
  const subject = tirExpression(selection.subject, options)
  return Object.freeze({
    id: selection.id,
    tests: selection.tests,
    arm: selection.arm,
    access: selection.access,
    source: tirExpression(selection.source, options),
    subject:
      selection.access === 'Move' && (subject._tag === 'Project' || subject._tag === 'IndexPlace')
        ? Object.freeze({ ...subject, access: 'ConsumeRequested' as const })
        : subject,
    members: selection.members,
    ...(member === undefined ? {} : { member }),
    universal: selection.pattern._tag === 'UniversalPattern',
    bindings: Object.freeze(
      selection.bindings.flatMap((binding): ReadonlyArray<Tir.PatternBinding> =>
        binding.type._tag === 'Available'
          ? [
              Object.freeze({
                id: binding.id,
                ...(binding.name._tag === 'Present' ? { name: binding.name.spelling } : {}),
                ...(binding.field === undefined ? {} : { field: binding.field.id }),
                path: binding.path,
                type: binding.type.type,
                access: binding.access,
                span: options.context.spanOf(binding.anchor),
                origin: Tir.authored(binding.anchor),
              }),
            ]
          : [],
      ),
    ),
    cleanup: selection.pattern.omitted,
    irrefutable: selection.irrefutable,
    loanEnd: selection.loanEnd,
    loanEndAt: selection.loanEndAt,
    span: options.context.spanOf(selection.anchor),
    origin: Tir.authored(selection.anchor),
  })
}

export interface LowerStatementOptions {
  /** The artifact-local publisher used by direct construction. */
  readonly builder?: BodyBuilder.BodyBuilder
  /** Spans of the authored module being lowered; TIR retains spans for diagnostics only. */
  readonly context: SemanticContext.SemanticContext
  /**
   * Keeps static structure for the evaluator: static calls, compile errors, static intrinsics and
   * references to static locals stay nodes instead of becoming the values they selected.
   *
   * The map remembers the node of every lowered expression. The evaluator keeps provenance per
   * node, so a subexpression must lower to the same node each time its parent is evaluated.
   */
  readonly static?: WeakMap<object, Tir.Expression>
  readonly lifetimeAssumptions?: Lifetime.Assumptions
  readonly lifetimeCompatibility?: TypeCompatibility.Context
  readonly resultType?: SemanticType
  /** The declaration-owned opaque family constructed at this function's return boundary. */
  readonly opaqueResultFamily?: Type.OpaqueFamilyKey
  /** A composite Effect representation every return site packs into (EFF-013). */
  readonly resultRepresentation?: SemanticType
  readonly functionId?: DeclarationId
  readonly eraseIntrinsicSections?: boolean
}

export const lowerStatements = (
  facts: ReadonlyArray<StatementFact>,
  options: LowerStatementOptions,
): ReadonlyArray<Tir.Statement> =>
  Object.freeze(
    (options.eraseIntrinsicSections ? executableStatements(facts) : facts)
      .filter(
        (statement) =>
          (options.static !== undefined ||
            statement._tag !== 'BindStatement' ||
            statement.binding.phase === 'Runtime') &&
          (!options.eraseIntrinsicSections ||
            !(
              (statement._tag === 'BindStatement' &&
                callableSectionOf(statement.binding.initializer)?.reference._tag ===
                  'ResolvedIntrinsicContract') ||
              (statement._tag === 'DropStatement' &&
                callableSectionOf(statement.expression)?.reference._tag ===
                  'ResolvedIntrinsicContract')
            )),
      )
      .map((statement): Tir.Statement => {
        if (statement._tag === 'UnsafeStatement')
          return Object.freeze({
            _tag: 'Unsafe',
            statements: lowerStatements(statement.statements, options),
            region: statement.region,
            span: options.context.spanOf(statement.anchor),
            origin: Tir.authored(statement.anchor),
          })
        if (statement._tag === 'BindStatement') {
          const binding = statement.binding
          const initializer = (): Tir.Expression => {
            // A declared union is the binding's type: the initializer injects at the boundary.
            if (
              binding.declaredType?._tag === 'Resolved' &&
              Type.isUnion(binding.declaredType.type)
            )
              return tirExpectedExpression(
                binding.initializer,
                binding.declaredType.type,
                'Binding',
                binding.anchor,
                options,
              )
            return tirExpression(binding.initializer, options)
          }
          return Object.freeze({
            _tag: 'Bind',
            binding: binding.id,
            name: binding.name._tag === 'Present' ? binding.name.spelling : undefined,
            mutability: binding.mutability,
            initializer: initializer(),
            region: statement.region,
            span: options.context.spanOf(binding.anchor),
            origin: Tir.authored(binding.anchor),
          })
        }
        if (statement._tag === 'PatternBindStatement')
          return Object.freeze({
            _tag: 'PatternBind',
            selection: tirPatternSelection(statement.selection, options),
            region: statement.region,
            span: options.context.spanOf(statement.anchor),
            origin: Tir.authored(statement.anchor),
          })
        if (statement._tag === 'ExpressionStatement')
          return Object.freeze({
            _tag: 'Evaluate',
            expression: tirExpression(statement.expression, options),
            region: statement.region,
            span: options.context.spanOf(statement.anchor),
            origin: Tir.authored(statement.anchor),
          })
        if (statement._tag === 'IfStatement')
          return Object.freeze({
            _tag: 'If',
            condition: tirExpression(statement.condition, options),
            taken: lowerStatements(statement.taken, options),
            otherwise: lowerStatements(statement.otherwise, options),
            region: statement.region,
            span: options.context.spanOf(statement.anchor),
            origin: Tir.authored(statement.anchor),
          })
        if (statement._tag === 'IfLetStatement')
          return Object.freeze({
            _tag: 'IfLet',
            selection: tirPatternSelection(statement.selection, options),
            taken: lowerStatements(statement.taken, options),
            otherwise: lowerStatements(statement.otherwise, options),
            region: statement.region,
            span: options.context.spanOf(statement.anchor),
            origin: Tir.authored(statement.anchor),
          })
        if (statement._tag === 'WriteStatement') {
          const place =
            statement.root === undefined
              ? undefined
              : tirAssignmentWritePlace(statement.destination, statement.root, options)
          if (place === undefined || !statement.compatible)
            return Object.freeze({
              _tag: 'UnavailableStatement',
              write: Object.freeze({
                destination: tirExpression(statement.destination, options),
                value: tirExpression(statement.value, options),
              }),
              region: statement.region,
              span: options.context.spanOf(statement.anchor),
              origin: Tir.authored(statement.anchor),
            })
          let valueOptions = options
          if (statement.lifetimeProof.length > 0) {
            const lifetimeAssumptions = Lifetime.assumptions([
              ...(options.lifetimeCompatibility?.assumptions.bounds ??
                options.lifetimeAssumptions?.bounds ??
                []),
              ...statement.lifetimeProof,
            ])
            // These are accepted conversion proofs for this RHS, not region edges active in
            // surrounding statements. The lifetime solver retains their installation points.
            valueOptions = {
              ...options,
              lifetimeAssumptions,
              lifetimeCompatibility: TypeCompatibility.context({
                ...options.lifetimeCompatibility,
                assumptions: lifetimeAssumptions,
              }),
            }
          }
          return Object.freeze({
            _tag: 'Write',
            destination: tirExpression(statement.destination, options),
            place,
            value: tirExpectedExpression(
              statement.value,
              place.type,
              'Assignment',
              place.origin.anchor,
              valueOptions,
            ),
            region: statement.region,
            span: options.context.spanOf(statement.anchor),
            origin: Tir.authored(statement.anchor),
          })
        }
        if (statement._tag === 'WhileStatement')
          return Object.freeze({
            _tag: 'While',
            loop: statement.loop,
            ...(statement.parent === undefined ? {} : { parent: statement.parent }),
            condition: tirExpression(statement.condition, options),
            body: lowerStatements(statement.body, options),
            region: statement.region,
            span: options.context.spanOf(statement.anchor),
            origin: Tir.authored(statement.anchor),
          })
        if (statement._tag === 'BreakStatement' || statement._tag === 'ContinueStatement')
          return statement.target === undefined
            ? Object.freeze({
                _tag: 'UnavailableStatement',
                region: statement.region,
                span: options.context.spanOf(statement.anchor),
                origin: Tir.authored(statement.anchor),
              })
            : Object.freeze({
                _tag: statement._tag === 'BreakStatement' ? 'Break' : 'Continue',
                target: statement.target,
                region: statement.region,
                span: options.context.spanOf(statement.anchor),
                origin: Tir.authored(statement.anchor),
              })
        if (statement._tag === 'ReturnStatement')
          return Object.freeze({
            _tag: 'Return',
            expression: effectJoinConvert(
              options.resultType === undefined
                ? tirExpression(statement.expression, options)
                : tirExpectedExpression(
                    statement.expression,
                    options.resultType,
                    'Return',
                    statement.anchor,
                    options,
                  ),
              options.resultRepresentation,
              statement.anchor,
              options.context,
            ),
            region: statement.region,
            span: options.context.spanOf(statement.expression.anchor),
            origin: Tir.authored(statement.expression.anchor),
          })
        if (statement._tag === 'DropStatement')
          return Object.freeze({
            _tag: 'Drop',
            expression: tirExpression(statement.expression, options),
            region: statement.region,
            span: options.context.spanOf(statement.anchor),
            origin: Tir.authored(statement.anchor),
          })
        if (statement.failure === undefined)
          return Object.freeze({
            _tag: 'UnavailableStatement',
            region: statement.region,
            span: options.context.spanOf(statement.anchor),
            origin: Tir.authored(statement.anchor),
          })
        return Object.freeze({
          _tag: 'Fail',
          expression:
            statement.transfer === 'Move'
              ? Object.freeze({
                  _tag: 'Move',
                  subject: tirExpression(statement.expression, options),
                  type:
                    statement.expression.type._tag === 'Available'
                      ? statement.expression.type.type
                      : statement.failure,
                  span: options.context.spanOf(statement.expression.anchor),
                  origin: Tir.authored(statement.expression.anchor),
                })
              : tirExpression(statement.expression, options),
          failure: statement.failure,
          transfer: statement.transfer,
          region: statement.region,
          span: options.context.spanOf(statement.anchor),
          origin: Tir.authored(statement.anchor),
        })
      }),
  )

export const tirCallableTarget = (reference: CallReferenceFact): Tir.CallableTarget | undefined => {
  if (reference._tag === 'ResolvedBuiltin')
    return Object.freeze({
      _tag: 'BuiltinCallableTarget',
      actor: reference.actor,
      operation: reference.operation,
      intrinsic: reference.intrinsic,
    })
  if (reference._tag === 'Resolved' && reference.declaration.canonical._tag === 'Canonical')
    return Object.freeze({
      _tag: 'DeclarationCallableTarget',
      declaration: reference.declaration.canonical.id,
    })
  return undefined
}

export const argumentBorrowId = (
  argument: ArgumentFact,
  ordinal: number,
): Tir.BorrowId | undefined => {
  const expression = argument.expression
  return expression._tag === 'Borrow' && expression.formation._tag !== 'Unavailable'
    ? Object.freeze({
        _tag: 'BorrowId',
        function: argument.id.function,
        callSpan: argument.id.callSpan,
        ...(argument.id.call === undefined ? {} : { call: argument.id.call }),
        ordinal,
      })
    : undefined
}

export const loanEndsOf = (
  arguments_: ReadonlyArray<ArgumentFact>,
  retained: (ordinal: number) => boolean = () => true,
): ReadonlyArray<Tir.BorrowId> =>
  Object.freeze(
    arguments_.flatMap((argument, ordinal) => {
      const borrow = argumentBorrowId(argument, ordinal)
      return borrow === undefined || !retained(ordinal) ? [] : [borrow]
    }),
  )

const isRepresentationIdenticalGenericForwarding = (
  declared: SemanticType,
  actual: SemanticType,
): boolean => {
  if (!Type.someSubterm(declared, Type.isParameter) || !Type.someSubterm(actual, Type.isParameter))
    return false
  const inferred = new Map<string, Type.GenericArgument>()
  const inference = TypeInference.inferOpenGenericArguments(declared, actual, inferred)
  return (
    inference.matches &&
    inference.conflicts.length === 0 &&
    Type.equals(Type.substitute(declared, inferred), actual)
  )
}

/** The nodes only a body that keeps static structure holds. */
const staticStructure = (
  fact: ExpressionFact,
  options: LowerStatementOptions,
): Tir.Expression | undefined => {
  const span = options.context.spanOf(fact.anchor)
  const origin = Tir.authored(fact.anchor)
  const unavailable = (): Tir.Expression => Object.freeze({ _tag: 'Unavailable', span, origin })
  if (fact._tag === 'CompileError')
    return Object.freeze({
      _tag: 'CompileError',
      message: tirExpression(fact.message, options),
      type: fact.type._tag === 'Available' ? fact.type.type : 'never',
      span,
      origin,
    })
  if (fact._tag === 'StaticText' && fact.data?.kind === 'Text' && fact.literal !== undefined)
    // Provenance names the literal itself, which can be narrower than the expression wrapping it.
    return Object.freeze({
      _tag: 'StaticStringLiteral',
      data: fact.data,
      type: Type.string(Lifetime.staticLifetime),
      span: options.context.spanOf(fact.literal),
      origin: Tir.authored(fact.literal),
    })
  if (fact._tag === 'Constant' && fact.value === undefined)
    return fact.declaration.canonical._tag === 'Canonical' && fact.type._tag === 'Available'
      ? Object.freeze({
          _tag: 'ConstantReference',
          declaration: fact.declaration.canonical.id,
          type: fact.type.type,
          span,
          origin,
        })
      : unavailable()
  if (fact._tag !== 'Call') return undefined
  const typeArguments = fact.contract._tag === 'Compatible' ? fact.contract.typeArguments : []
  const arguments_ = Object.freeze(
    fact.arguments.map((argument) => tirExpression(argument.expression, options)),
  )
  if (
    fact.reference._tag === 'ResolvedIntrinsicContract' &&
    fact.reference.intrinsic.id.actor === 'Intrinsic'
  )
    return Object.freeze({
      _tag: 'StaticIntrinsic',
      operation: fact.reference.intrinsic.id.name,
      typeArguments,
      arguments: arguments_,
      type: fact.type._tag === 'Available' ? fact.type.type : 'never',
      span,
      origin,
    })
  if (
    fact.reference._tag === 'Resolved' &&
    fact.reference.declaration.phase === 'Static' &&
    fact.reference.declaration.canonical._tag === 'Canonical'
  )
    return Object.freeze({
      _tag: 'StaticCall',
      target: fact.reference.declaration.canonical.id,
      typeArguments,
      evidence: Object.freeze(
        fact.contract._tag === 'Compatible'
          ? fact.contract.evidence.map(Constraint.evidenceKey)
          : [],
      ),
      arguments: arguments_,
      ...(fact.staticFailure === undefined ? {} : { failure: fact.staticFailure }),
      ...(fact.staticTextSpan === undefined ? {} : { text: fact.staticTextSpan }),
      ...(fact.staticTextOrigin === undefined ? {} : { textOrigin: fact.staticTextOrigin }),
      type: fact.type._tag === 'Available' ? fact.type.type : 'never',
      span,
      origin,
    })
  return undefined
}

export const tirExpression = (
  fact: ExpressionFact,
  options: LowerStatementOptions,
  borrow?: Tir.BorrowId,
): Tir.Expression => {
  // A call supplies the argument borrow identity only after the argument itself has been checked.
  // Replace the provisional context-free node with that call-owned node instead of reusing it.
  const published = borrow === undefined ? options.builder?.expressions.get(fact) : undefined
  if (published !== undefined) return published
  const known = options.static?.get(fact)
  if (known !== undefined) return known
  const lowered =
    (options.static === undefined ? undefined : staticStructure(fact, options)) ??
    residualExpression(fact, options, borrow)
  const node =
    options.builder === undefined || lowered.id !== undefined
      ? lowered
      : BodyBuilder.node(options.builder, lowered)
  options.static?.set(fact, node)
  options.builder?.expressions.set(fact, node)
  return node
}

const residualExpression = (
  fact: ExpressionFact,
  options: LowerStatementOptions,
  borrow?: Tir.BorrowId,
): Tir.Expression => {
  if (fact._tag === 'CompileError')
    return Object.freeze({
      _tag: 'Unavailable',
      span: options.context.spanOf(fact.anchor),
      origin: Tir.authored(fact.anchor),
    })
  if (fact._tag === 'ShortCircuit') {
    const left = fact.arguments.at(0)
    const right = fact.arguments.at(1)
    if (left === undefined || right === undefined || fact.type._tag !== 'Available') {
      return Object.freeze({
        _tag: 'Unavailable',
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
      })
    }
    const loweredLeft = tirExpression(left.expression, options)
    const loweredRight = tirExpression(right.expression, options)
    return loweredLeft._tag === 'Unavailable' || loweredRight._tag === 'Unavailable'
      ? Object.freeze({
          _tag: 'Unavailable',
          span: options.context.spanOf(fact.anchor),
          origin: Tir.authored(fact.anchor),
        })
      : Object.freeze({
          _tag: 'ShortCircuit',
          operator: fact.operator,
          left: loweredLeft,
          right: loweredRight,
          type: fact.type.type,
          span: options.context.spanOf(fact.anchor),
          origin: Tir.authored(fact.anchor),
        })
  }
  if (fact._tag === 'Integer') {
    return fact.integer._tag === 'Available'
      ? Object.freeze({
          _tag: 'IntegerLiteral',
          value: fact.integer.value,
          type: fact.integer.type,
          span: options.context.spanOf(fact.anchor),
          origin: Tir.authored(fact.anchor),
        })
      : Object.freeze({
          _tag: 'Unavailable',
          span: options.context.spanOf(fact.anchor),
          origin: Tir.authored(fact.anchor),
        })
  }
  if (fact._tag === 'Duration') {
    return fact.value !== undefined && fact.type._tag === 'Available'
      ? Object.freeze({
          _tag: 'IntegerLiteral',
          value: fact.value,
          type: 'u64',
          span: options.context.spanOf(fact.anchor),
          origin: Tir.authored(fact.anchor),
        })
      : Object.freeze({
          _tag: 'Unavailable',
          span: options.context.spanOf(fact.anchor),
          origin: Tir.authored(fact.anchor),
        })
  }
  if (fact._tag === 'Floating') {
    return fact.floating._tag === 'Available'
      ? Object.freeze({
          _tag: 'FloatingLiteral',
          bits: fact.floating.bits,
          spelling: fact.floating.spelling,
          type: fact.floating.type,
          span: options.context.spanOf(fact.anchor),
          origin: Tir.authored(fact.anchor),
        })
      : Object.freeze({
          _tag: 'Unavailable',
          span: options.context.spanOf(fact.anchor),
          origin: Tir.authored(fact.anchor),
        })
  }
  if (fact._tag === 'StaticText') {
    if (fact.data === undefined) {
      return Object.freeze({
        _tag: 'Unavailable',
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
      })
    }
    if (fact.data.kind === 'Text') {
      return Object.freeze({
        _tag: 'StaticStringLiteral',
        data: fact.data,
        type: Type.string(Lifetime.staticLifetime),
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
      })
    }
    return Object.freeze({
      _tag: 'StaticByteViewLiteral',
      data: fact.data,
      type: Type.slice('Shared', 'u8', Lifetime.staticLifetime),
      span: options.context.spanOf(fact.anchor),
      origin: Tir.authored(fact.anchor),
    })
  }
  if (fact._tag === 'Unit') {
    return Object.freeze({
      _tag: 'UnitLiteral',
      type: Type.unit,
      span: options.context.spanOf(fact.anchor),
      origin: Tir.authored(fact.anchor),
    })
  }
  if (fact._tag === 'Boolean') {
    return fact.type._tag === 'Available'
      ? Object.freeze({
          _tag: 'BooleanLiteral',
          value: fact.value,
          type: fact.type.type,
          span: options.context.spanOf(fact.anchor),
          origin: Tir.authored(fact.anchor),
        })
      : Object.freeze({
          _tag: 'Unavailable',
          span: options.context.spanOf(fact.anchor),
          origin: Tir.authored(fact.anchor),
        })
  }
  if (fact._tag === 'Character') {
    return fact.type._tag === 'Available' && fact.value !== undefined
      ? Object.freeze({
          _tag: 'CharacterLiteral',
          value: fact.value,
          type: fact.type.type,
          span: options.context.spanOf(fact.anchor),
          origin: Tir.authored(fact.anchor),
        })
      : Object.freeze({
          _tag: 'Unavailable',
          span: options.context.spanOf(fact.anchor),
          origin: Tir.authored(fact.anchor),
        })
  }
  if (fact._tag === 'Constant') {
    if (fact.value?._tag === 'Character')
      return Object.freeze({
        _tag: 'CharacterLiteral',
        value: fact.value.value,
        type: 'char',
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
      })
    if (fact.value?._tag === 'Boolean')
      return Object.freeze({
        _tag: 'BooleanLiteral',
        value: fact.value.value,
        type: 'bool',
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
      })
    if (fact.value?._tag === 'Integer')
      return Object.freeze({
        _tag: 'IntegerLiteral',
        value: fact.value.value,
        type: fact.value.type,
        ...(fact.declaration.canonical._tag === 'Canonical'
          ? { constant: fact.declaration.canonical.id }
          : {}),
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
      })
    if (fact.value?._tag === 'Floating')
      return Object.freeze({
        _tag: 'FloatingLiteral',
        bits: fact.value.bits,
        spelling: fact.value.spelling,
        type: fact.value.type,
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
      })
    if (fact.value?._tag === 'String')
      return Object.freeze({
        _tag: 'StaticStringLiteral',
        data: fact.value.data,
        type: Type.string(Lifetime.staticLifetime),
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
      })
    return Object.freeze({
      _tag: 'Unavailable',
      span: options.context.spanOf(fact.anchor),
      origin: Tir.authored(fact.anchor),
    })
  }
  if (fact._tag === 'ForeignStatic') {
    return fact.type._tag === 'Available' && fact.declaration.canonical._tag === 'Canonical'
      ? Object.freeze({
          _tag: 'ForeignStaticLoad',
          declaration: fact.declaration.canonical.id,
          direction: fact.declaration.direction,
          symbol: fact.declaration.foreign.symbol,
          type: fact.type.type,
          span: options.context.spanOf(fact.anchor),
          origin: Tir.authored(fact.anchor),
        })
      : Object.freeze({
          _tag: 'Unavailable',
          span: options.context.spanOf(fact.anchor),
          origin: Tir.authored(fact.anchor),
        })
  }
  if (fact._tag === 'EnumMember') {
    if (
      fact.enum.canonical._tag !== 'Canonical' ||
      fact.member?.canonical._tag !== 'Canonical' ||
      fact.member.discriminant._tag !== 'Available' ||
      fact.type._tag !== 'Available' ||
      !Type.isNominal(fact.type.type)
    )
      return Object.freeze({
        _tag: 'Unavailable',
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
        ...(fact.cause === undefined ? {} : publishedCause(options.builder, fact.cause)),
      })
    return Object.freeze({
      _tag: 'EnumMember',
      enum: fact.enum.canonical.id,
      member: fact.member.canonical.id,
      discriminant: fact.member.discriminant.value,
      type: fact.type.type,
      span: options.context.spanOf(fact.anchor),
      origin: Tir.authored(fact.anchor),
    })
  }
  if (fact._tag === 'EnumValue') {
    const value = tirExpression(fact.argument, options)
    return fact.type._tag !== 'Available' || value._tag === 'Unavailable'
      ? Object.freeze({
          _tag: 'Unavailable',
          span: options.context.spanOf(fact.anchor),
          origin: Tir.authored(fact.anchor),
        })
      : Object.freeze({
          _tag: 'EnumValue',
          enum: fact.operation.enum,
          value,
          intrinsic: fact.operation.intrinsic,
          type: fact.operation.result.spelling,
          span: options.context.spanOf(fact.anchor),
          origin: Tir.authored(fact.anchor),
        })
  }
  if (fact._tag === 'Identifier') {
    if (
      options.static === undefined &&
      fact.staticValue !== undefined &&
      fact.type._tag === 'Available'
    )
      return staticValueExpression(fact.staticValue, fact.type.type, fact.anchor, options.context)
    if (
      options.static === undefined &&
      fact.reference._tag === 'ResolvedBinding' &&
      fact.reference.binding.staticValue !== undefined &&
      fact.type._tag === 'Available'
    )
      return staticValueExpression(
        fact.reference.binding.staticValue,
        fact.type.type,
        fact.anchor,
        options.context,
      )
    return tirReference(fact.reference, fact.type, fact.anchor, options.context, options.builder)
  }
  if (fact._tag === 'Move') {
    const subject = tirExpression(fact.subject, options)
    if (subject._tag === 'Unavailable' || fact.type._tag !== 'Available') {
      return subject._tag === 'Unavailable'
        ? subject
        : Object.freeze({
            _tag: 'Unavailable',
            span: options.context.spanOf(fact.anchor),
            origin: Tir.authored(fact.anchor),
          })
    }
    return Object.freeze({
      _tag: 'Move',
      subject:
        subject._tag === 'Project' || subject._tag === 'IndexPlace'
          ? Object.freeze({ ...subject, access: 'ConsumeRequested' as const })
          : subject,
      type: fact.type.type,
      span: options.context.spanOf(fact.anchor),
      origin: Tir.authored(fact.anchor),
    })
  }
  if (fact._tag === 'PlaceReplace') {
    const place =
      fact.root === undefined
        ? undefined
        : tirAssignmentWritePlace(fact.destination, fact.root, options)
    if (place === undefined || !fact.compatible || fact.type._tag !== 'Available') {
      return Object.freeze({
        _tag: 'Unavailable',
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
      })
    }
    return Object.freeze({
      _tag: 'Replace',
      place,
      value: tirExpectedExpression(
        fact.value,
        place.type,
        'Assignment',
        place.origin.anchor,
        options,
      ),
      type: fact.type.type,
      span: options.context.spanOf(fact.anchor),
      origin: Tir.authored(fact.anchor),
    })
  }
  if (fact._tag === 'EffectBlock') {
    if (fact.type._tag !== 'Available' || !Type.isEffect(fact.type.type))
      return Object.freeze({
        _tag: 'Unavailable',
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
      })
    return Object.freeze({
      _tag: 'EffectBlock',
      site: fact.site,
      statements: lowerStatements(fact.statements, {
        // Deferred bodies retain the enclosing borrow context but own their return boundary.
        context: options.context,
        ...(options.functionId === undefined ? {} : { functionId: options.functionId }),
        ...(options.lifetimeAssumptions === undefined
          ? {}
          : { lifetimeAssumptions: options.lifetimeAssumptions }),
        ...(options.lifetimeCompatibility === undefined
          ? {}
          : { lifetimeCompatibility: options.lifetimeCompatibility }),
        ...(options.eraseIntrinsicSections === undefined
          ? {}
          : { eraseIntrinsicSections: options.eraseIntrinsicSections }),
        resultType: fact.type.type.success,
      }),
      captures: Object.freeze(
        fact.captures.map((capture) =>
          Object.freeze({
            ...(capture.reference._tag === 'BindingFact' ? { binding: capture.reference.id } : {}),
            ...(capture.reference._tag === 'PatternBinding'
              ? { pattern: capture.reference.id }
              : {}),
            ...(capture.reference._tag === 'ParameterDeclaration'
              ? { parameter: capture.reference.id }
              : {}),
            access: capture.access,
            span: capture.span,
            at: capture.anchor,
            ...(capture.expression === undefined ? {} : { use: capture.expression.anchor }),
          }),
        ),
      ),
      type: fact.type.type,
      span: options.context.spanOf(fact.anchor),
      origin: Tir.authored(fact.anchor),
    })
  }
  if (fact._tag === 'Run') {
    const subject = tirExpression(fact.subject, options)
    if (subject._tag === 'Unavailable' || fact.type._tag !== 'Available')
      return Object.freeze({
        _tag: 'Unavailable',
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
      })
    return Object.freeze({
      _tag: 'Run',
      subject,
      type: fact.type.type,
      span: options.context.spanOf(fact.anchor),
      origin: Tir.authored(fact.anchor),
    })
  }
  if (fact._tag === 'EffectCatch') {
    const protected_ = tirExpression(fact.protected, options)
    const handler = tirExpression(fact.handler, options)
    if (
      protected_._tag === 'Unavailable' ||
      handler._tag === 'Unavailable' ||
      fact.reference._tag !== 'ResolvedIntrinsicReference' ||
      fact.selected === undefined ||
      fact.type._tag !== 'Available' ||
      !Type.isEffect(fact.type.type)
    )
      return Object.freeze({
        _tag: 'Unavailable',
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
      })
    return Object.freeze({
      _tag: 'EffectCatch',
      intrinsic: fact.reference.operation.id,
      protected: protected_,
      handler,
      selected: fact.selected,
      protectedRow: fact.protectedRow,
      handlerRow: fact.handlerRow,
      residualRow: fact.residualRow,
      evidence: publishedEvidence(options.builder, fact.evidence),
      type: fact.type.type,
      span: options.context.spanOf(fact.anchor),
      origin: Tir.authored(fact.anchor),
    })
  }
  if (fact._tag === 'EffectBindRequirement') {
    const protected_ = tirExpression(fact.protected, options)
    if (
      protected_._tag === 'Unavailable' ||
      fact.provider === undefined ||
      fact.type._tag !== 'Available' ||
      !Type.isEffect(fact.type.type)
    )
      return Object.freeze({
        _tag: 'Unavailable',
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
      })
    return Object.freeze({
      _tag: 'EffectBindRequirement',
      protected: protected_,
      provider: Object.freeze({
        ...(fact.provider.reference._tag === 'BindingFact'
          ? { binding: fact.provider.reference.id }
          : { parameter: fact.provider.reference.id }),
        selected: fact.provider.selected,
        evidence: publishedEvidence(options.builder, fact.provider.evidence),
        ...(fact.provider.capability === undefined ? {} : { capability: fact.provider.capability }),
        providerType: fact.provider.providerType,
        ...(fact.provider.witness === undefined ? {} : { witness: fact.provider.witness }),
        ...(fact.provider.role === undefined ? {} : { role: fact.provider.role }),
        selectionAccess: fact.provider.selectionAccess,
        captureAccess: fact.provider.captureAccess,
        span: fact.provider.span,
        at: fact.provider.at,
      }),
      type: fact.type.type,
      span: options.context.spanOf(fact.anchor),
      origin: Tir.authored(fact.anchor),
    })
  }
  if (fact._tag === 'Match') {
    const loweredScrutinee = tirExpression(fact.scrutinee, options)
    const scrutinee =
      fact.access === 'Move' &&
      (loweredScrutinee._tag === 'Project' || loweredScrutinee._tag === 'IndexPlace')
        ? Object.freeze({ ...loweredScrutinee, access: 'ConsumeRequested' as const })
        : loweredScrutinee
    if (scrutinee._tag === 'Unavailable' || fact.type._tag !== 'Available') {
      return Object.freeze({
        _tag: 'Unavailable',
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
      })
    }
    const target = fact.type.type
    return Object.freeze({
      _tag: 'Match',
      match: fact.id,
      access: fact.access,
      scrutinee,
      members: fact.members,
      arms: Object.freeze(
        fact.arms.map((arm) => {
          let member: Match.CoverageIdentity | undefined
          if (arm.pattern._tag === 'EnumMemberPattern') {
            member = arm.pattern.coverage
          } else if (arm.pattern._tag === 'UnionVariantPattern') {
            member = arm.pattern.coverage
          } else if (
            (arm.pattern._tag === 'NominalPattern' || arm.pattern._tag === 'TypePattern') &&
            arm.pattern.member !== undefined
          ) {
            member = Match.structuralMember(arm.pattern.member)
          }
          return Object.freeze({
            id: arm.id,
            tests: arm.tests,
            ...(member === undefined ? {} : { member }),
            ...(arm.pattern._tag === 'IntegerPattern' && arm.pattern.value !== undefined
              ? { integer: arm.pattern.value }
              : {}),
            universal: arm.pattern._tag === 'UniversalPattern',
            bindings: Object.freeze(
              arm.bindings.flatMap((binding) =>
                binding.type._tag === 'Available'
                  ? [
                      Object.freeze({
                        id: binding.id,
                        ...(binding.name._tag === 'Present' ? { name: binding.name.spelling } : {}),
                        ...(binding.field === undefined ? {} : { field: binding.field.id }),
                        path: binding.path,
                        type: binding.type.type,
                        access: binding.access,
                        span: options.context.spanOf(binding.anchor),
                        origin: Tir.authored(binding.anchor),
                      }),
                    ]
                  : [],
              ),
            ),
            cleanup: arm.pattern.omitted,
            ...(arm.guard === undefined ? {} : { guard: tirExpression(arm.guard, options) }),
            body:
              arm.body._tag === 'Expression'
                ? Object.freeze({
                    _tag: 'Expression' as const,
                    expression: Type.isUnion(target)
                      ? tirExpectedExpression(
                          arm.body.expression,
                          target,
                          'MatchArm',
                          arm.anchor,
                          options,
                        )
                      : tirExpression(arm.body.expression, options),
                    type:
                      arm.body.type._tag === 'Available' && !Type.isUnion(target)
                        ? arm.body.type.type
                        : target,
                    span: options.context.spanOf(arm.body.anchor),
                    origin: Tir.authored(arm.body.anchor),
                  })
                : Object.freeze({
                    _tag: 'Block' as const,
                    statements: lowerStatements(arm.body.statements, options),
                    completion: arm.body.completion,
                    type: arm.body.type._tag === 'Available' ? arm.body.type.type : target,
                    span: options.context.spanOf(arm.body.anchor),
                    origin: Tir.authored(arm.body.anchor),
                  }),
            before: arm.before,
            after: arm.after,
            reachable: arm.reachable,
            span: options.context.spanOf(arm.anchor),
            at: arm.anchor,
            origin: Tir.authored(arm.anchor),
          })
        }),
      ),
      type: target,
      span: options.context.spanOf(fact.anchor),
      origin: Tir.authored(fact.anchor),
    })
  }
  if (fact._tag === 'StructLiteral') {
    if (
      fact.target._tag !== 'Resolved' ||
      fact.type._tag !== 'Available' ||
      fact.fields.length !== fact.target.struct.fields.length
    ) {
      return Object.freeze({
        _tag: 'Unavailable',
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
        ...(fact.target._tag === 'Unavailable' && fact.target.cause !== undefined
          ? publishedCause(options.builder, fact.target.cause)
          : {}),
      })
    }
    const substitution =
      TypeInference.substitution(
        fact.target.struct.typeParameters.map((parameter) => parameter.type),
        fact.target.type.arguments,
      ) ?? new Map()
    return Object.freeze({
      _tag: 'Construct',
      nominal: fact.target.type,
      evaluationOrder: Object.freeze(
        fact.initializers.flatMap((initializer) =>
          initializer.state._tag === 'Resolved' ? [initializer.state.field.id] : [],
        ),
      ),
      fields: Object.freeze(
        fact.fields.map(({ field, initializer }) => {
          const value =
            field.declaredType._tag === 'Resolved'
              ? tirExpectedExpression(
                  initializer.expression,
                  Type.substitute(field.declaredType.type, substitution),
                  'StructField',
                  field.anchor,
                  options,
                )
              : tirExpression(initializer.expression, options)
          return Object.freeze({ field: field.id, value })
        }),
      ),
      type: fact.type.type,
      span: options.context.spanOf(fact.anchor),
      origin: Tir.authored(fact.anchor),
    })
  }
  if (fact._tag === 'UnionVariant') {
    if (
      fact.target._tag !== 'Resolved' ||
      fact.target.variant.canonical._tag !== 'Canonical' ||
      fact.type._tag !== 'Available' ||
      fact.fields.length !== fact.target.variant.fields.length
    ) {
      return Object.freeze({
        _tag: 'Unavailable',
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
        ...(fact.target._tag === 'Unavailable' && fact.target.cause !== undefined
          ? publishedCause(options.builder, fact.target.cause)
          : {}),
      })
    }
    const substitution =
      TypeInference.substitution(
        fact.target.union.typeParameters.map((parameter) => parameter.type),
        fact.target.type.arguments,
      ) ?? new Map()
    return Object.freeze({
      _tag: 'ConstructUnionVariant',
      nominal: fact.target.type,
      variant: fact.target.variant.canonical.id,
      variantOrdinal: fact.target.variant.id.ordinal,
      evaluationOrder: Object.freeze(
        fact.initializers.flatMap((initializer) =>
          initializer.state._tag === 'Resolved' ? [initializer.state.field.id] : [],
        ),
      ),
      fields: Object.freeze(
        fact.fields.map(({ field, initializer }) => {
          const value =
            field.declaredType._tag === 'Resolved'
              ? tirExpectedExpression(
                  initializer.expression,
                  Type.substitute(field.declaredType.type, substitution),
                  'StructField',
                  field.anchor,
                  options,
                )
              : tirExpression(initializer.expression, options)
          return Object.freeze({ field: field.id, value })
        }),
      ),
      type: fact.type.type,
      span: options.context.spanOf(fact.anchor),
      origin: Tir.authored(fact.anchor),
    })
  }
  if (fact._tag === 'ArrayLiteral') {
    if (fact.state._tag !== 'Complete' || fact.type._tag !== 'Available') {
      return Object.freeze({
        _tag: 'Unavailable',
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
      })
    }
    return Object.freeze({
      _tag: 'ArrayConstruct',
      elements: Object.freeze(
        fact.elements.map((element) =>
          element.expected === undefined
            ? tirExpression(element.expression, options)
            : tirExpectedExpression(
                element.expression,
                element.expected,
                'ArrayElement',
                element.anchor,
                options,
              ),
        ),
      ),
      type: fact.state.type,
      span: options.context.spanOf(fact.anchor),
      origin: Tir.authored(fact.anchor),
    })
  }
  if (fact._tag === 'FieldProjection') {
    if (
      options.static === undefined &&
      fact.staticValue !== undefined &&
      fact.type._tag === 'Available'
    )
      return staticValueExpression(fact.staticValue, fact.type.type, fact.anchor, options.context)
    if (fact.state._tag === 'SliceLength' && fact.type._tag === 'Available') {
      const slice = tirExpression(fact.subject, options)
      return slice._tag === 'Unavailable'
        ? slice
        : Object.freeze({
            _tag: 'SliceLength',
            slice,
            type: 'usize',
            span: options.context.spanOf(fact.anchor),
            origin: Tir.authored(fact.anchor),
          })
    }
    if (
      fact.nominal === undefined ||
      fact.state._tag !== 'Resolved' ||
      fact.type._tag !== 'Available'
    ) {
      return Object.freeze({
        _tag: 'Unavailable',
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
        ...(fact.state._tag === 'Unavailable' && fact.state.cause !== undefined
          ? publishedCause(options.builder, fact.state.cause)
          : {}),
      })
    }
    return Object.freeze({
      _tag: 'Project',
      subject: tirExpression(fact.subject, options),
      nominal: fact.nominal,
      field: fact.state.field.id,
      access: 'CopyRead',
      ...(fact.borrowAccess === undefined ? {} : { borrowAccess: fact.borrowAccess }),
      type: fact.type.type,
      span: options.context.spanOf(fact.anchor),
      origin: Tir.authored(fact.anchor),
    })
  }
  if (fact._tag === 'IndexProjection') {
    if (
      fact.slice !== undefined &&
      fact.type._tag === 'Available' &&
      fact.bounds._tag === 'RuntimeSlice'
    ) {
      const slice = tirExpression(fact.subject, options)
      const index = tirExpression(fact.index, options)
      if (slice._tag === 'Unavailable' || index._tag === 'Unavailable') {
        return slice._tag === 'Unavailable' ? slice : index
      }
      return Object.freeze({
        _tag: 'SliceIndexPlace',
        slice,
        index,
        access: fact.slice.access,
        sourceType: fact.slice,
        type: fact.type.type,
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
      })
    }
    if (
      fact.array === undefined ||
      fact.type._tag !== 'Available' ||
      (fact.bounds._tag !== 'Proven' && fact.bounds._tag !== 'Runtime')
    ) {
      return Object.freeze({
        _tag: 'Unavailable',
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
        ...(fact.bounds._tag === 'Invalid'
          ? publishedCause(options.builder, fact.bounds.cause)
          : {}),
      })
    }
    const subject = tirExpression(fact.subject, options)
    const index = tirExpression(fact.index, options)
    if (subject._tag === 'Unavailable' || index._tag === 'Unavailable') {
      return subject._tag === 'Unavailable' ? subject : index
    }
    return Object.freeze({
      _tag: 'IndexPlace',
      subject,
      index,
      array: fact.array,
      access: fact.access,
      bounds: fact.bounds,
      type: fact.type.type,
      span: options.context.spanOf(fact.anchor),
      origin: Tir.authored(fact.anchor),
    })
  }
  if (fact._tag === 'ReferentProjection') {
    if (
      fact.state._tag !== 'Resolved' ||
      fact.reference === undefined ||
      fact.borrowAccess === undefined ||
      fact.type._tag !== 'Available'
    ) {
      return Object.freeze({
        _tag: 'Unavailable',
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
        ...(fact.state._tag === 'Unavailable' && fact.state.cause !== undefined
          ? publishedCause(options.builder, fact.state.cause)
          : {}),
      })
    }
    return Object.freeze({
      _tag: 'ReferentPlace',
      subject: tirExpression(fact.subject, options),
      reference: fact.reference,
      access: 'CopyRead',
      borrowAccess: fact.borrowAccess,
      type: fact.type.type,
      span: options.context.spanOf(fact.anchor),
      origin: Tir.authored(fact.anchor),
    })
  }
  if (fact._tag === 'Borrow') {
    // Direct arguments retain their call-owned identity. Nested storage and assignment borrows
    // use the standalone identity already published by source ownership analysis.
    borrow ??=
      options.functionId === undefined
        ? undefined
        : Object.freeze({
            _tag: 'BorrowId',
            function: options.functionId,
            callSpan: options.context.spanOf(fact.anchor),
            call: fact.anchor,
            ordinal: 0,
          })
    if (
      borrow === undefined ||
      fact.formation._tag === 'Unavailable' ||
      fact.type._tag !== 'Available' ||
      (!Type.isSlice(fact.type.type) && !Type.isReference(fact.type.type))
    ) {
      return Object.freeze({
        _tag: 'Unavailable',
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
        ...(fact.formation._tag === 'Unavailable' && fact.formation.cause !== undefined
          ? publishedCause(options.builder, fact.formation.cause)
          : {}),
      })
    }
    let root: Tir.SliceRoot
    switch (fact.formation.root._tag) {
      case 'BindingRoot':
        root = Object.freeze({ _tag: 'BindingSliceRoot', binding: fact.formation.root.binding.id })
        break
      case 'ParameterRoot':
        root = Object.freeze({
          _tag: 'ParameterSliceRoot',
          parameter: fact.formation.root.parameter.id,
        })
        break
      case 'PatternRoot':
        root = Object.freeze({ _tag: 'PatternSliceRoot', binding: fact.formation.root.binding.id })
        break
      case 'TemporaryRoot':
        root = Object.freeze({
          _tag: 'TemporarySliceRoot',
          owner: fact.formation.root.owner,
          value: tirExpression(fact.formation.root.value, options),
        })
        break
    }
    const selectors: Array<Tir.BorrowSelector> = []
    for (const selector of fact.formation.root.path) {
      if (selector._tag === 'Field') {
        selectors.push(
          Object.freeze({
            _tag: 'Field',
            field: selector.field,
            span: selector.span,
            ...(selector.at === undefined ? {} : { at: selector.at }),
            ...(selector.at === undefined ? {} : { at: selector.at }),
          }),
        )
        continue
      }
      if (selector._tag === 'SliceIndex') {
        const index = tirExpression(selector.index, options)
        if (index._tag === 'Unavailable') {
          return Object.freeze({
            _tag: 'Unavailable',
            span: options.context.spanOf(fact.anchor),
            origin: Tir.authored(fact.anchor),
          })
        }
        selectors.push(
          Object.freeze({
            _tag: 'SliceIndex',
            index,
            slice: selector.slice,
            span: selector.span,
            ...(selector.at === undefined ? {} : { at: selector.at }),
            ...(selector.at === undefined ? {} : { at: selector.at }),
          }),
        )
        continue
      }
      const index = tirExpression(selector.index, options)
      if (index._tag === 'Unavailable') {
        return Object.freeze({
          _tag: 'Unavailable',
          span: options.context.spanOf(fact.anchor),
          origin: Tir.authored(fact.anchor),
        })
      }
      selectors.push(
        Object.freeze({
          _tag: 'Index',
          index,
          array: selector.array,
          bounds: selector.bounds,
          span: selector.span,
          ...(selector.at === undefined ? {} : { at: selector.at }),
        }),
      )
    }
    if (
      (fact.formation._tag === 'ValueBorrow' || fact.formation._tag === 'ValueReborrow') &&
      Type.isReference(fact.type.type)
    ) {
      return Object.freeze({
        _tag: 'ValueBorrow',
        place: tirExpression(fact.subject, options),
        borrow,
        root,
        selectors: Object.freeze(selectors),
        source:
          fact.formation._tag === 'ValueBorrow' ? fact.formation.source : fact.formation.parent,
        access: fact.access,
        reborrow: fact.formation._tag === 'ValueReborrow',
        suspendsParent: fact.formation._tag === 'ValueReborrow' && fact.formation.suspendsParent,
        type: fact.type.type,
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
      })
    }
    if (fact.formation._tag === 'ValueBorrow' || fact.formation._tag === 'ValueReborrow')
      return Object.freeze({
        _tag: 'Unavailable',
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
      })
    if (!Type.isSlice(fact.type.type))
      return Object.freeze({
        _tag: 'Unavailable',
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
      })
    return Object.freeze({
      _tag: 'SliceBorrow',
      place: tirExpression(fact.subject, options),
      borrow,
      root,
      selectors: Object.freeze(selectors),
      source:
        fact.formation._tag === 'FixedArrayBorrow' ? fact.formation.array : fact.formation.parent,
      access: fact.access,
      reborrow: fact.formation._tag === 'SliceReborrow',
      suspendsParent: fact.formation._tag === 'SliceReborrow' && fact.formation.suspendsParent,
      type: fact.type.type,
      span: options.context.spanOf(fact.anchor),
      origin: Tir.authored(fact.anchor),
    })
  }
  if (fact._tag === 'FunctionItem') {
    const target = tirCallableTarget(fact.reference)
    if (target === undefined || fact.type._tag !== 'Available')
      return Object.freeze({
        _tag: 'Unavailable',
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
      })
    if (fact.foreignAddress !== undefined && Type.isForeignFunction(fact.type.type))
      return Object.freeze({
        _tag: 'ForeignFunctionAddress',
        target,
        symbol: fact.foreignAddress.symbol,
        type: fact.type.type,
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
      })
    if (!Type.isCallable(fact.type.type))
      return Object.freeze({
        _tag: 'Unavailable',
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
      })
    return Object.freeze({
      _tag: 'FunctionItem',
      target,
      typeArguments: fact.typeArguments,
      type: fact.type.type,
      span: options.context.spanOf(fact.anchor),
      origin: Tir.authored(fact.anchor),
    })
  }
  if (fact._tag === 'CallableSection') {
    const target = tirCallableTarget(fact.reference)
    if (target === undefined || fact.type._tag !== 'Available' || !Type.isCallable(fact.type.type))
      return Object.freeze({
        _tag: 'Unavailable',
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
      })
    return Object.freeze({
      _tag: 'CallableSection',
      site: fact.site,
      target,
      remainingParameters: fact.remainingParameters,
      captures: Object.freeze(
        fact.captures.map((capture) =>
          Object.freeze({
            ordinal: capture.ordinal,
            parameterOrdinal: capture.parameterOrdinal,
            value: tirExpression(
              capture.expression,
              options,
              Object.freeze({
                _tag: 'BorrowId',
                function: fact.site.function,
                callSpan: options.context.spanOf(fact.anchor),
                call: fact.anchor,
                ordinal: capture.ordinal,
              }),
            ),
            access: capture.access,
          }),
        ),
      ),
      typeArguments: fact.typeArguments,
      substitution: fact.substitution,
      retainedDependencies: fact.retainedDependencies,
      mode: fact.mode,
      type: fact.type.type,
      span: options.context.spanOf(fact.anchor),
      origin: Tir.authored(fact.anchor),
    })
  }
  if (fact._tag === 'ForeignApply') {
    if (fact.type._tag !== 'Available')
      return {
        _tag: 'Unavailable',
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
      }
    return {
      _tag: 'ForeignApply',
      evaluation: fact.evaluation,
      callee: tirExpression(fact.callee, options),
      arguments: fact.arguments.map((argument, ordinal) =>
        tirExpression(argument.expression, options, argumentBorrowId(argument, ordinal)),
      ),
      contract: fact.contract,
      loanEnds: loanEndsOf(fact.arguments, () => true),
      type: fact.type.type,
      span: options.context.spanOf(fact.anchor),
      origin: Tir.authored(fact.anchor),
    }
  }
  if (fact._tag === 'CallableApply') {
    if (fact.type._tag !== 'Available')
      return Object.freeze({
        _tag: 'Unavailable',
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
      })
    const resultType = fact.type.type
    const retainedOrdinals = new Set(
      retainedResultArguments(fact, options.lifetimeAssumptions ?? Lifetime.assumptions([])).map(
        (argument) => argument.id.ordinal,
      ),
    )
    const retainedSection = callableSectionOf(fact.callee)
    const retainedCaptures =
      retainedSection?.captures.filter(
        (capture) =>
          capture.expression.type._tag === 'Available' &&
          retainsLifetimes(
            capture.expression.type.type,
            resultType,
            options.lifetimeAssumptions ?? Lifetime.assumptions([]),
          ),
      ) ?? []
    const retainedCaptureLoans: ReadonlyArray<Tir.BorrowId> =
      retainedSection === undefined
        ? []
        : retainedCaptures.map((capture) => ({
            _tag: 'BorrowId',
            function: retainedSection.site.function,
            callSpan: options.context.spanOf(retainedSection.anchor),
            call: retainedSection.anchor,
            ordinal: capture.ordinal,
          }))
    return Object.freeze({
      _tag: 'CallableApply',
      callee: tirExpression(fact.callee, options),
      arguments: Object.freeze(
        fact.arguments.map((argument, ordinal) =>
          tirExpression(argument.expression, options, argumentBorrowId(argument, ordinal)),
        ),
      ),
      // A staged application retains every argument loan inside the new environment.
      loanEnds: loanEndsOf(
        fact.arguments,
        (ordinal) => fact.staged === undefined && !retainedOrdinals.has(ordinal),
      ),
      heldLoans: Object.freeze([
        ...loanEndsOf(
          fact.arguments,
          (ordinal) => fact.staged !== undefined || retainedOrdinals.has(ordinal),
        ),
        ...retainedCaptureLoans,
      ]),
      ...(fact.staged === undefined
        ? {}
        : {
            staged: Object.freeze({
              site: fact.staged.site,
              captures: Object.freeze(
                fact.staged.captures.map((capture) =>
                  Object.freeze({ ordinal: capture.ordinal, access: capture.access }),
                ),
              ),
            }),
          }),
      access: fact.mode,
      substitution: fact.substitution,
      evaluation:
        fact.provenance._tag === 'PipelineCallableApplication'
          ? 'LeftThenCallable'
          : 'CalleeThenArguments',
      realization: fact.callee._tag === 'CallableSection' ? 'DirectErasedSection' : 'Environment',
      type: fact.type.type,
      span: options.context.spanOf(fact.anchor),
      origin: Tir.authored(fact.anchor),
    })
  }
  if (
    fact.reference._tag === 'ResolvedInterfaceOperation' &&
    fact.contract._tag === 'Compatible' &&
    fact.type._tag === 'Available'
  ) {
    const borrowIds = loanEndsOf(fact.arguments)
    return Object.freeze({
      _tag: 'InterfaceOperationCall',
      ...(fact._tag === 'Operator' ? { operator: true as const } : {}),
      capability: fact.reference.capability,
      provider: fact.reference.provider,
      operation: fact.reference.operation,
      contract: fact.reference.interfaceContract,
      ...(fact.witnessEffectSite === undefined
        ? {}
        : { witnessEffectSite: fact.witnessEffectSite }),
      arguments: Object.freeze(
        fact.arguments.map((argument, ordinal) =>
          tirExpression(argument.expression, options, argumentBorrowId(argument, ordinal)),
        ),
      ),
      loanEnds: borrowIds,
      type: fact.type.type,
      span: options.context.spanOf(fact.anchor),
      origin: Tir.authored(fact.anchor),
    })
  }
  if (
    fact._tag === 'Operator' &&
    fact.reference._tag === 'ResolvedEnumEquality' &&
    fact.type._tag === 'Available'
  ) {
    const leftFact = fact.arguments.at(0)
    const rightFact = fact.arguments.at(1)
    const left = leftFact === undefined ? undefined : tirExpression(leftFact.expression, options)
    const right = rightFact === undefined ? undefined : tirExpression(rightFact.expression, options)
    return left === undefined ||
      right === undefined ||
      left._tag === 'Unavailable' ||
      right._tag === 'Unavailable'
      ? Object.freeze({
          _tag: 'Unavailable',
          span: options.context.spanOf(fact.anchor),
          origin: Tir.authored(fact.anchor),
        })
      : Object.freeze({
          _tag: 'EnumEquality',
          enum: fact.reference.enum,
          left,
          right,
          negated: fact.reference.operator === 'NotEquals',
          type: Scalar.boolean.spelling,
          span: options.context.spanOf(fact.anchor),
          origin: Tir.authored(fact.anchor),
        })
  }
  if (
    fact.reference._tag === 'ResolvedBuiltin' &&
    fact.contract._tag === 'Compatible' &&
    fact.type._tag === 'Available'
  ) {
    const retainedOrdinals = new Set(
      retainedResultArguments(fact, options.lifetimeAssumptions ?? Lifetime.assumptions([])).map(
        (argument) => argument.id.ordinal,
      ),
    )
    const directLoanEnds = loanEndsOf(fact.arguments, (ordinal) => !retainedOrdinals.has(ordinal))
    const nestedSlotLoanEnds =
      fact.reference.operation === 'SlotWrite' ||
      fact.reference.operation === 'SlotTake' ||
      fact.reference.operation === 'SlotCopy' ||
      fact.reference.operation === 'SlotDrop'
        ? fact.arguments.flatMap((argument): ReadonlyArray<Tir.BorrowId> => {
            const nested = argument.expression
            if (
              nested._tag !== 'Call' ||
              nested.reference._tag !== 'ResolvedBuiltin' ||
              nested.reference.operation !== 'RawBufferSlot'
            )
              return []
            return loanEndsOf(nested.arguments)
          })
        : []
    const arguments_ = Object.freeze(
      fact.arguments.map((argument, ordinal) => {
        const borrowId = argumentBorrowId(argument, ordinal)
        return tirExpression(argument.expression, options, borrowId)
      }),
    )
    const heldLoans = Object.freeze(
      loanEndsOf(fact.arguments, (ordinal) => retainedOrdinals.has(ordinal)),
    )
    if (fact.reference.operation === 'StringFromUtf8Unchecked') {
      const source = arguments_.at(0)
      return source === undefined || source._tag === 'Unavailable' || !Type.isSlice(source.type)
        ? Object.freeze({
            _tag: 'Unavailable',
            span: options.context.spanOf(fact.anchor),
            origin: Tir.authored(fact.anchor),
          })
        : Object.freeze({
            _tag: 'RuntimeStringView',
            source,
            heldLoans,
            type: Type.string(source.type.lifetime),
            span: options.context.spanOf(fact.anchor),
            origin: Tir.authored(fact.anchor),
          })
    }
    if (fact.reference.operation === 'StringEqualsExact') {
      const left = arguments_.at(0)
      const right = arguments_.at(1)
      return left === undefined ||
        right === undefined ||
        left._tag === 'Unavailable' ||
        right._tag === 'Unavailable'
        ? Object.freeze({
            _tag: 'Unavailable',
            span: options.context.spanOf(fact.anchor),
            origin: Tir.authored(fact.anchor),
          })
        : Object.freeze({
            _tag: 'StringEquality',
            left,
            right,
            negated: fact._tag === 'Operator' && fact.operator === 'NotEquals',
            intrinsic: fact.reference.intrinsic,
            type: Scalar.boolean.spelling,
            span: options.context.spanOf(fact.anchor),
            origin: Tir.authored(fact.anchor),
          })
    }
    return Object.freeze({
      _tag: 'BuiltinCall',
      ...(fact.reference.assembly === undefined ? {} : { assembly: fact.reference.assembly }),
      operation: fact.reference.operation,
      intrinsic: fact.reference.intrinsic,
      ...(fact._tag === 'Operator' && fact.interfaceOperation !== undefined
        ? { interfaceOperation: fact.interfaceOperation }
        : {}),
      ...(fact._tag === 'Operator' && fact.witnessEffectSite !== undefined
        ? { witnessEffectSite: fact.witnessEffectSite }
        : {}),
      typeArguments: Object.freeze(
        fact._tag === 'Call'
          ? (() => {
              const operation = Intrinsic.findOperationById(fact.reference.intrinsic)
              const parameters =
                operation !== undefined && Intrinsic.isBuiltinOperation(operation)
                  ? (operation.rule.typeParameters ?? Object.freeze([]))
                  : Object.freeze<ReadonlyArray<Type.Parameter>>([])
              return fact.typeArguments.flatMap((argument, ordinal) => {
                const parameter = parameters.at(ordinal)
                const converted =
                  parameter === undefined
                    ? argument.type
                    : genericArgumentOfTypeArgument(parameter, argument)
                return converted === undefined ? [] : [converted]
              })
            })()
          : [],
      ),
      arguments: arguments_,
      loanEnds: Object.freeze([...directLoanEnds, ...nestedSlotLoanEnds]),
      heldLoans,
      type: fact.type.type,
      span: options.context.spanOf(fact.anchor),
      origin: Tir.authored(fact.anchor),
    })
  }
  if (
    fact.reference._tag === 'ResolvedServiceOperation' &&
    fact.reference.service.canonical._tag === 'Canonical' &&
    fact.reference.operation.name._tag === 'Present' &&
    fact.contract._tag === 'Compatible' &&
    fact.type._tag === 'Available' &&
    Type.isEffect(fact.type.type)
  ) {
    const serviceArguments = fact.contract.typeArguments.slice(
      0,
      fact.reference.service.typeParameters.length,
    )
    const service = Type.nominal(
      fact.reference.service.canonical.id.module,
      fact.reference.service.canonical.id.name,
      serviceArguments,
    )
    const requirement = Type.requirementMembers(fact.type.type).find((candidate) =>
      Type.equals(candidate.capability, service),
    )
    if (requirement === undefined)
      return Object.freeze({
        _tag: 'Unavailable',
        span: options.context.spanOf(fact.anchor),
        origin: Tir.authored(fact.anchor),
      })
    const substitution = fact.contract.substitution
    const target = fact.reference.operation
    const staticArgumentOrigins = Object.freeze(
      (fact._tag === 'Call' ? (fact.staticArguments ?? []) : []).map(
        (argument) => argument.textOrigin,
      ),
    )
    return Object.freeze({
      _tag: 'ServiceEffectConstruct',
      service,
      operation: fact.reference.operation.name.spelling,
      role: requirement.role,
      access: requirement.access,
      typeArguments: fact.contract.typeArguments,
      staticArguments: Object.freeze(
        (fact._tag === 'Call' ? (fact.staticArguments ?? []) : []).map(
          (argument) => argument.value,
        ),
      ),
      ...(staticArgumentOrigins.some((origin) => origin !== undefined)
        ? { staticArgumentOrigins }
        : {}),
      arguments: Object.freeze(
        fact.arguments.flatMap((argument, ordinal) => {
          const parameter = target.parameters.at(ordinal)
          if (parameter?.phase === 'Static') return []
          const borrowId = argumentBorrowId(argument, ordinal)
          const genericForwarding =
            parameter?.declaredType._tag === 'Resolved' &&
            argument.expression.type._tag === 'Available' &&
            isRepresentationIdenticalGenericForwarding(
              parameter.declaredType.type,
              argument.expression.type.type,
            )
          return [
            parameter?.declaredType._tag === 'Resolved' && !genericForwarding
              ? tirExpectedExpression(
                  argument.expression,
                  Type.substitute(parameter.declaredType.type, substitution),
                  'Argument',
                  parameter.anchor,
                  options,
                  borrowId,
                )
              : tirExpression(argument.expression, options, borrowId),
          ]
        }),
      ),
      loanEnds: loanEndsOf(
        fact.arguments,
        (ordinal) => target.parameters.at(ordinal)?.phase !== 'Static',
      ),
      type: fact.type.type,
      span: options.context.spanOf(fact.anchor),
      origin: Tir.authored(fact.anchor),
    })
  }
  if (
    fact.reference._tag === 'Resolved' &&
    fact.reference.declaration.canonical._tag === 'Canonical' &&
    fact.contract._tag === 'Compatible' &&
    fact.type._tag === 'Available'
  ) {
    if (options.static === undefined && fact._tag === 'Call' && fact.staticValue !== undefined)
      return staticValueExpression(fact.staticValue, fact.type.type, fact.anchor, options.context)
    const target = fact.reference.declaration
    const substitution = fact.contract.substitution
    const retainedOrdinals = new Set(
      retainedResultArguments(fact, options.lifetimeAssumptions ?? Lifetime.assumptions([])).map(
        (argument) => argument.id.ordinal,
      ),
    )
    const staticArgumentOrigins = Object.freeze(
      (fact._tag === 'Call' ? (fact.staticArguments ?? []) : []).map(
        (argument) => argument.textOrigin,
      ),
    )
    const call = {
      target: fact.reference.declaration.canonical.id,
      typeArguments: fact.contract.typeArguments,
      evidence: publishedEvidence(options.builder, fact.contract.evidence),
      symbolicConformances: fact.contract.symbolicConformances ?? Object.freeze([]),
      staticArguments: Object.freeze(
        (fact._tag === 'Call' ? (fact.staticArguments ?? []) : []).map(
          (argument) => argument.value,
        ),
      ),
      ...(staticArgumentOrigins.some((origin) => origin !== undefined)
        ? { staticArgumentOrigins }
        : {}),
      arguments: Object.freeze(
        fact.arguments.flatMap((argument, ordinal) => {
          const parameter = target.parameters.at(ordinal)
          if (parameter?.phase === 'Static') return []
          const borrowId = argumentBorrowId(argument, ordinal)
          const genericForwarding =
            parameter?.declaredType._tag === 'Resolved' &&
            argument.expression.type._tag === 'Available' &&
            isRepresentationIdenticalGenericForwarding(
              parameter.declaredType.type,
              argument.expression.type.type,
            )
          return [
            parameter?.declaredType._tag === 'Resolved' && !genericForwarding
              ? tirExpectedExpression(
                  argument.expression,
                  Type.substitute(parameter.declaredType.type, substitution),
                  'Argument',
                  parameter.anchor,
                  options,
                  borrowId,
                )
              : tirExpression(argument.expression, options, borrowId),
          ]
        }),
      ),
      loanEnds: loanEndsOf(
        fact.arguments,
        (ordinal) =>
          target.parameters.at(ordinal)?.phase !== 'Static' && !retainedOrdinals.has(ordinal),
      ),
      heldLoans: loanEndsOf(
        fact.arguments,
        (ordinal) =>
          target.parameters.at(ordinal)?.phase !== 'Static' && retainedOrdinals.has(ordinal),
      ),
      type: fact.type.type,
      span: options.context.spanOf(fact.anchor),
      origin: Tir.authored(fact.anchor),
    }
    return Type.isEffect(fact.type.type) &&
      fact.reference._tag === 'Resolved' &&
      fact.reference.declaration.functionKind === 'Effect'
      ? Object.freeze({ ...call, _tag: 'EffectConstruct' as const, type: fact.type.type })
      : Object.freeze({ ...call, _tag: 'Call' as const })
  }
  let cause: Diagnostic.Identity<Location.Location> | undefined
  if (fact.reference._tag === 'Missing' || fact.reference._tag === 'Ambiguous') {
    cause = fact.reference.cause
  } else if (fact.contract._tag === 'Unavailable') {
    cause = fact.contract.cause
  }
  return Object.freeze({
    _tag: 'Unavailable',
    span: options.context.spanOf(fact.anchor),
    origin: Tir.authored(fact.anchor),
    ...(cause === undefined ? {} : publishedCause(options.builder, cause)),
  })
}

/** Wraps one return site so it packs its Effect into the function's composite representation. */
const effectJoinConvert = (
  source: Tir.Expression,
  target: SemanticType | undefined,
  expected: AuthoredHir.Anchor,
  context: SemanticContext.SemanticContext,
): Tir.Expression => {
  if (
    target === undefined ||
    source._tag === 'Unavailable' ||
    Type.isNever(source.type) ||
    !Type.isRepresented(target)
  )
    return source
  return Object.freeze({
    _tag: 'UnionConvert',
    source,
    sourceType: source.type,
    target,
    conversion: 'EffectJoin',
    mappings: Object.freeze([]),
    access: 'Owned',
    context: 'Return',
    expectedAt: context.spanOf(expected),
    expected,
    type: target,
    span: source.span,
    origin: source.origin,
  })
}

export const tirExpectedExpression = (
  fact: ExpressionFact,
  target: SemanticType,
  context: Extract<Tir.Expression, { readonly _tag: 'UnionConvert' }>['context'],
  expected: AuthoredHir.Anchor,
  options: LowerStatementOptions,
  borrow?: Tir.BorrowId,
): Tir.Expression => {
  if (
    fact._tag === 'Integer' &&
    fact.integer._tag === 'Available' &&
    contextualIntegerCompatible(fact, target) &&
    typeof target === 'string' &&
    Scalar.isIntegerSpelling(target)
  )
    return Object.freeze({
      _tag: 'IntegerLiteral',
      value: fact.integer.value,
      type: target,
      span: options.context.spanOf(fact.anchor),
      origin: Tir.authored(fact.anchor),
    })
  const loweredSource = tirExpression(fact, options, borrow)
  if (loweredSource._tag === 'Unavailable') return loweredSource
  const unionTarget = Type.isUnion(target) ? target : undefined
  const representation =
    unionTarget === undefined ? undefined : representationOfExpression(options.context, fact)
  const sourceContract = Type.isRepresented(loweredSource.type)
    ? loweredSource.type.contract
    : loweredSource.type
  const representedSource =
    representation !== undefined &&
    (Type.isCallable(sourceContract) || Type.isEffect(sourceContract)) &&
    unionTarget?.members.some(
      (member) =>
        Type.equals(member, sourceContract) ||
        (Type.isRepresented(member) && Type.equals(member.contract, sourceContract)),
    )
      ? Type.represented(sourceContract, sourceContract, representation)
      : undefined
  const source = loweredSource
  if (
    context === 'Return' &&
    options.opaqueResultFamily !== undefined &&
    Type.isRepresented(target) &&
    Type.isOpaqueRepresentationArgument(target.representation.argument) &&
    Type.equalsOpaqueFamily(target.representation.argument.family, options.opaqueResultFamily) &&
    representationOfExpression(options.context, fact) !== undefined &&
    TypeCompatibility.isCompatible(
      TypeCompatibility.check(sourceContract, target.contract, options.lifetimeCompatibility),
    )
  )
    return source
  if (Type.isRepresented(target) && Type.haveSameRepresentationShape(source.type, target))
    return source
  const compatibility = TypeCompatibility.check(
    representedSource ?? source.type,
    target,
    options.lifetimeCompatibility,
  )
  if (compatibility._tag === 'Exact') return source
  if (
    compatibility._tag === 'Lifetime' ||
    compatibility._tag === 'CallableMode' ||
    compatibility._tag === 'EffectAccess' ||
    compatibility._tag === 'ReferenceAccess' ||
    compatibility._tag === 'PointerWeakening'
  )
    return source
  if (compatibility._tag === 'Bottom') return source
  if (compatibility._tag === 'Incompatible') {
    return Object.freeze({
      _tag: 'Unavailable',
      span: options.context.spanOf(fact.anchor),
      origin: Tir.authored(fact.anchor),
    })
  }
  return Object.freeze({
    _tag: 'UnionConvert',
    source,
    sourceType: compatibility.source,
    target: compatibility.target,
    conversion: compatibility._tag,
    mappings: compatibility.mappings,
    access: 'Owned',
    context,
    expectedAt: options.context.spanOf(expected),
    expected,
    type: compatibility.target,
    span: options.context.spanOf(fact.anchor),
    origin: Tir.authored(fact.anchor),
  })
}

export const tirWritePlace = (
  fact: ExpressionFact,
  root: AssignmentRootFact,
  options: LowerStatementOptions,
): Tir.WritePlace | undefined => {
  const selectors: Array<Tir.WriteSelector> = []
  const walk = (current: ExpressionFact): boolean => {
    if (current._tag === 'Identifier') {
      if (root._tag === 'PatternBinding')
        return (
          current.reference._tag === 'ResolvedPattern' && current.reference.binding.id === root.id
        )
      return root._tag === 'ParameterDeclaration'
        ? current.reference._tag === 'Resolved' &&
            current.reference.parameter.id.ordinal === root.id.ordinal
        : current.reference._tag === 'ResolvedBinding' &&
            current.reference.binding.id.ordinal === root.id.ordinal
    }
    if (current._tag === 'FieldProjection') {
      if (
        !walk(current.subject) ||
        current.state._tag !== 'Resolved' ||
        current.type._tag !== 'Available'
      ) {
        return false
      }
      selectors.push(
        Object.freeze({
          _tag: 'Field',
          field: current.state.field.id,
          type: current.type.type,
          span: options.context.spanOf(current.anchor),
          at: current.anchor,
          origin: Tir.authored(current.anchor),
        }),
      )
      return true
    }
    if (current._tag === 'IndexProjection') {
      if (
        !walk(current.subject) ||
        current.array === undefined ||
        current.type._tag !== 'Available' ||
        (current.bounds._tag !== 'Proven' && current.bounds._tag !== 'Runtime')
      ) {
        return false
      }
      const index = tirExpression(current.index, options)
      if (index._tag === 'Unavailable') return false
      selectors.push(
        Object.freeze({
          _tag: 'Index',
          index,
          array: current.array,
          bounds: current.bounds,
          type: current.type.type,
          span: options.context.spanOf(current.anchor),
          at: current.anchor,
          origin: Tir.authored(current.anchor),
        }),
      )
      return true
    }
    return false
  }
  if (!walk(fact) || fact.type._tag !== 'Available') return undefined
  let ownedRoot: Tir.OwnedWriteRoot
  if (root._tag === 'ParameterDeclaration')
    ownedRoot = { _tag: 'ParameterWriteRoot', parameter: root.id }
  else if (root._tag === 'PatternBinding')
    ownedRoot = { _tag: 'PatternWriteRoot', binding: root.id }
  else ownedRoot = { _tag: 'BindingWriteRoot', binding: root.id }
  return Object.freeze({
    _tag: 'WritePlace',
    root: ownedRoot,
    selectors: Object.freeze(selectors),
    type: fact.type.type,
    span: options.context.spanOf(fact.anchor),
    origin: Tir.authored(fact.anchor),
  })
}

export const assignmentRootType = (root: AssignmentRootFact): SemanticType | undefined => {
  if (root._tag === 'PatternBinding')
    return root.type._tag === 'Available' ? root.type.type : undefined
  if (root._tag === 'ParameterDeclaration') {
    return root.declaredType._tag === 'Resolved' ? root.declaredType.type : undefined
  }
  return root.inferredType._tag === 'Available' ? root.inferredType.type : undefined
}

export const tirBorrowedWritePlace = (
  fact: ExpressionFact,
  root: AssignmentRootFact,
  options: LowerStatementOptions,
): Tir.BorrowedWritePlace | undefined => {
  if (root._tag === 'PatternBinding') return undefined
  const rootType = assignmentRootType(root)
  if (rootType === undefined) return undefined
  const selectors: Array<Tir.BorrowedWriteSelector> = []
  const walk = (current: ExpressionFact): boolean => {
    if (current._tag === 'Identifier') {
      return root._tag === 'ParameterDeclaration'
        ? current.reference._tag === 'Resolved' &&
            current.reference.parameter.id.ordinal === root.id.ordinal
        : current.reference._tag === 'ResolvedBinding' &&
            current.reference.binding.id.ordinal === root.id.ordinal
    }
    if (current._tag === 'FieldProjection') {
      if (
        !walk(current.subject) ||
        current.state._tag !== 'Resolved' ||
        current.type._tag !== 'Available'
      ) {
        return false
      }
      selectors.push(
        Object.freeze({
          _tag: 'Field',
          field: current.state.field.id,
          type: current.type.type,
          span: options.context.spanOf(current.anchor),
          at: current.anchor,
          origin: Tir.authored(current.anchor),
        }),
      )
      return true
    }
    if (current._tag === 'ReferentProjection') {
      return (
        walk(current.subject) &&
        current.state._tag === 'Resolved' &&
        current.borrowAccess === 'Exclusive' &&
        current.type._tag === 'Available'
      )
    }
    if (current._tag === 'IndexProjection') {
      if (!walk(current.subject) || current.type._tag !== 'Available') return false
      const index = tirExpression(current.index, options)
      if (index._tag === 'Unavailable') return false
      if (
        current.slice !== undefined &&
        current.slice.access === 'Exclusive' &&
        current.bounds._tag === 'RuntimeSlice'
      ) {
        selectors.push(
          Object.freeze({
            _tag: 'SliceIndex',
            index,
            slice: current.slice,
            type: current.type.type,
            span: options.context.spanOf(current.anchor),
            at: current.anchor,
            origin: Tir.authored(current.anchor),
          }),
        )
        return true
      }
      if (
        current.array === undefined ||
        (current.bounds._tag !== 'Proven' && current.bounds._tag !== 'Runtime')
      ) {
        return false
      }
      selectors.push(
        Object.freeze({
          _tag: 'Index',
          index,
          array: current.array,
          bounds: current.bounds,
          type: current.type.type,
          span: options.context.spanOf(current.anchor),
          at: current.anchor,
          origin: Tir.authored(current.anchor),
        }),
      )
      return true
    }
    return false
  }
  if (!walk(fact) || fact.type._tag !== 'Available') return undefined
  return Object.freeze({
    _tag: 'BorrowedWritePlace',
    root:
      root._tag === 'ParameterDeclaration'
        ? Object.freeze({ _tag: 'ParameterSliceRoot' as const, parameter: root.id })
        : Object.freeze({ _tag: 'BindingSliceRoot' as const, binding: root.id }),
    rootType,
    selectors: Object.freeze(selectors),
    type: fact.type.type,
    span: options.context.spanOf(fact.anchor),
    origin: Tir.authored(fact.anchor),
  })
}

export const tirAssignmentWritePlace = (
  fact: ExpressionFact,
  root: AssignmentRootFact,
  options: LowerStatementOptions,
): Tir.WritePlace | undefined => {
  const access = assignmentRootAccess(root, fact)
  if (access === 'ExclusiveBorrowed') return tirBorrowedWritePlace(fact, root, options)
  if (access === 'MutableOwned') return tirWritePlace(fact, root, options)
  return undefined
}

export const directStatementExpressions = (
  statement: StatementFact,
): ReadonlyArray<ExpressionFact> => {
  switch (statement._tag) {
    case 'BindStatement':
      return Object.freeze([statement.binding.initializer])
    case 'PatternBindStatement':
      return Object.freeze([statement.selection.source])
    case 'ExpressionStatement':
      return Object.freeze([statement.expression])
    case 'ReturnStatement':
    case 'FailStatement':
    case 'DropStatement':
      return Object.freeze([statement.expression])
    case 'IfStatement':
    case 'WhileStatement':
      return Object.freeze([statement.condition])
    case 'IfLetStatement':
      return Object.freeze([statement.selection.source])
    case 'WriteStatement':
      return Object.freeze([statement.destination, statement.value])
    case 'UnsafeStatement':
    case 'BreakStatement':
    case 'ContinueStatement':
      return Object.freeze([])
  }
}

export const directExpressionChildren = (
  expression: ExpressionFact,
): ReadonlyArray<ExpressionFact> => {
  switch (expression._tag) {
    case 'CompileError':
      return Object.freeze([expression.message])
    case 'EnumValue':
      return Object.freeze([expression.argument])
    case 'Move':
    case 'Borrow':
    case 'FieldProjection':
    case 'ReferentProjection':
    case 'Run':
      return Object.freeze([expression.subject])
    case 'PlaceReplace':
      return Object.freeze([expression.destination, expression.value])
    case 'IndexProjection':
      return Object.freeze([expression.subject, expression.index])
    case 'ArrayLiteral':
      return Object.freeze(expression.elements.map((element) => element.expression))
    case 'StructLiteral':
    case 'UnionVariant':
      return Object.freeze(expression.initializers.map((initializer) => initializer.expression))
    case 'EffectBindRequirement':
      return Object.freeze([expression.protected])
    case 'EffectCatch':
      return Object.freeze([expression.protected, expression.handler])
    case 'CallableSection':
      return Object.freeze(expression.captures.map((capture) => capture.expression))
    case 'ForeignApply':
    case 'CallableApply':
      return Object.freeze([
        expression.callee,
        ...expression.arguments.map((argument) => argument.expression),
      ])
    case 'Operator':
    case 'ShortCircuit':
    case 'Call':
      return Object.freeze(expression.arguments.map((argument) => argument.expression))
    case 'EffectBlock':
    case 'Match':
    case 'Integer':
    case 'Duration':
    case 'Floating':
    case 'StaticText':
    case 'Character':
    case 'Unit':
    case 'Boolean':
    case 'Constant':
    case 'ForeignStatic':
    case 'EnumMember':
    case 'Identifier':
    case 'FunctionItem':
      return Object.freeze([])
  }
}

/** Callbacks for one deterministic traversal of elaborated statement and expression facts. */

/**
 * Lowers what construction has analyzed so far into nodes the evaluator can interpret.
 *
 * One lowering is shared by everything evaluated for one body: the evaluator keeps text provenance
 * per node, and a subexpression must be the same node whenever its parent is evaluated again.
 */
export interface StaticLowering {
  readonly expression: (fact: ExpressionFact) => Tir.Expression
  readonly statements: (facts: ReadonlyArray<StatementFact>) => ReadonlyArray<Tir.Statement>
}

export const staticLowering = (
  context: SemanticContext.SemanticContext,
  builder?: BodyBuilder.BodyBuilder,
): StaticLowering => {
  const options: LowerStatementOptions = {
    context,
    static: new WeakMap(),
    ...(builder === undefined ? {} : { builder }),
  }
  return Object.freeze({
    expression: (fact: ExpressionFact) => tirExpression(fact, options),
    statements: (facts: ReadonlyArray<StatementFact>) => lowerStatements(facts, options),
  })
}
