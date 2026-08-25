import { callableSectionOf, genericArgumentOfTypeArgument } from './CallResolution.js'
import type * as Diagnostic from './Diagnostic.js'
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
  returnedBorrowArgument,
} from './Elaboration.js'
import { representationOfExpression } from './ExpressionAnalysis.js'
import type * as Hir from './Hir.js'
import * as Intrinsic from './Intrinsic.js'
import * as TypeInference from './internal/TypeInference.js'
import * as Match from './Match.js'
import * as Scalar from './Scalar.js'
import type * as SourceSpan from './SourceSpan.js'
import { executableStatements } from './StatementAnalysis.js'
import * as Type from './Type.js'
import * as TypeCompatibility from './TypeCompatibility.js'

export const hirReference = (
  reference: ParameterReferenceFact,
  type: ExpressionTypeFact,
  span: SourceSpan.SourceSpan,
): Hir.Expression => {
  if (reference._tag === 'Resolved' && type._tag === 'Available') {
    return Object.freeze({
      _tag: 'ParameterReference',
      parameter: reference.parameter.id,
      type: type.type,
      span,
    })
  }
  if (reference._tag === 'ResolvedBinding' && type._tag === 'Available') {
    return Object.freeze({
      _tag: 'BindingReference',
      binding: reference.binding.id,
      type: type.type,
      span,
    })
  }
  if (reference._tag === 'ResolvedPattern' && type._tag === 'Available') {
    return Object.freeze({
      _tag: 'PatternBindingReference',
      binding: reference.binding.id,
      type: type.type,
      span,
    })
  }
  return Object.freeze({
    _tag: 'Unavailable',
    span,
    ...(reference._tag === 'Missing' && reference.cause !== undefined
      ? { cause: reference.cause }
      : {}),
  })
}

export const hirPatternSelection = (selection: PatternSelectionFact): Hir.PatternSelection => {
  let member: Match.CoverageIdentity | undefined
  if (selection.pattern._tag === 'EnumMemberPattern') {
    member = selection.pattern.coverage
  } else if (
    (selection.pattern._tag === 'NominalPattern' || selection.pattern._tag === 'TypePattern') &&
    selection.pattern.member !== undefined
  ) {
    member = Match.structuralMember(selection.pattern.member)
  }
  return Object.freeze({
    id: selection.id,
    arm: selection.arm,
    access: selection.access,
    subject: hirExpression(selection.subject),
    members: selection.members,
    ...(member === undefined ? {} : { member }),
    universal: selection.pattern._tag === 'UniversalPattern',
    bindings: Object.freeze(
      selection.bindings.flatMap(
        (binding): ReadonlyArray<Hir.PatternBinding> =>
          binding.type._tag === 'Available'
            ? [
                Object.freeze({
                  id: binding.id,
                  ...(binding.name._tag === 'Present' ? { name: binding.name.spelling } : {}),
                  ...(binding.field === undefined ? {} : { field: binding.field.id }),
                  path: binding.path,
                  type: binding.type.type,
                  access: binding.access,
                  span: binding.syntax.span,
                }),
              ]
            : [],
      ),
    ),
    cleanup: selection.pattern.omitted,
    irrefutable: selection.irrefutable,
    span: selection.syntax.span,
  })
}

export interface LowerStatementOptions {
  readonly resultType?: SemanticType
  readonly functionId?: DeclarationId
  readonly eraseIntrinsicSections?: boolean
  readonly borrowBindingInitializers?: boolean
}

export const lowerStatements = (
  facts: ReadonlyArray<StatementFact>,
  options: LowerStatementOptions = {},
): ReadonlyArray<Hir.Statement> =>
  Object.freeze(
    (options.eraseIntrinsicSections ? executableStatements(facts) : facts)
      .filter(
        (statement) =>
          !options.eraseIntrinsicSections ||
          !(
            (statement._tag === 'BindStatement' &&
              callableSectionOf(statement.binding.initializer)?.reference._tag ===
                'ResolvedIntrinsicContract') ||
            (statement._tag === 'DropStatement' &&
              callableSectionOf(statement.expression)?.reference._tag ===
                'ResolvedIntrinsicContract')
          ),
      )
      .map((statement): Hir.Statement => {
        if (statement._tag === 'UnsafeStatement')
          return Object.freeze({
            _tag: 'Unsafe',
            statements: lowerStatements(statement.statements, options),
            region: statement.region,
            span: statement.syntax.span,
          })
        if (statement._tag === 'BindStatement')
          return Object.freeze({
            _tag: 'Bind',
            binding: statement.binding.id,
            name:
              statement.binding.name._tag === 'Present'
                ? statement.binding.name.spelling
                : undefined,
            mutability: statement.binding.mutability,
            initializer:
              options.borrowBindingInitializers &&
              statement.binding.initializer._tag === 'Borrow' &&
              options.functionId !== undefined
                ? hirExpression(
                    statement.binding.initializer,
                    Object.freeze({
                      _tag: 'BorrowId',
                      function: options.functionId,
                      callSpan: statement.binding.initializer.syntax.span,
                      ordinal: 0,
                    }),
                  )
                : hirExpression(statement.binding.initializer),
            region: statement.region,
            span: statement.binding.syntax.span,
          })
        if (statement._tag === 'PatternBindStatement')
          return Object.freeze({
            _tag: 'PatternBind',
            selection: hirPatternSelection(statement.selection),
            region: statement.region,
            span: statement.syntax.span,
          })
        if (statement._tag === 'ExpressionStatement')
          return Object.freeze({
            _tag: 'Evaluate',
            expression: hirExpression(statement.expression),
            region: statement.region,
            span: statement.syntax.span,
          })
        if (statement._tag === 'IfStatement')
          return Object.freeze({
            _tag: 'If',
            condition: hirExpression(statement.condition),
            taken: lowerStatements(statement.taken, options),
            otherwise: lowerStatements(statement.otherwise, options),
            region: statement.region,
            span: statement.syntax.span,
          })
        if (statement._tag === 'IfLetStatement')
          return Object.freeze({
            _tag: 'IfLet',
            selection: hirPatternSelection(statement.selection),
            taken: lowerStatements(statement.taken, options),
            otherwise: lowerStatements(statement.otherwise, options),
            region: statement.region,
            span: statement.syntax.span,
          })
        if (statement._tag === 'WriteStatement') {
          const place =
            statement.root === undefined
              ? undefined
              : hirAssignmentWritePlace(statement.destination, statement.root)
          if (place === undefined || !statement.compatible)
            return Object.freeze({
              _tag: 'UnavailableStatement',
              region: statement.region,
              span: statement.syntax.span,
            })
          return Object.freeze({
            _tag: 'Write',
            place,
            value: hirExpectedExpression(statement.value, place.type, 'Assignment', place.span),
            region: statement.region,
            span: statement.syntax.span,
          })
        }
        if (statement._tag === 'WhileStatement')
          return Object.freeze({
            _tag: 'While',
            loop: statement.loop,
            ...(statement.parent === undefined ? {} : { parent: statement.parent }),
            condition: hirExpression(statement.condition),
            body: lowerStatements(statement.body, options),
            region: statement.region,
            span: statement.syntax.span,
          })
        if (statement._tag === 'BreakStatement' || statement._tag === 'ContinueStatement')
          return statement.target === undefined
            ? Object.freeze({
                _tag: 'UnavailableStatement',
                region: statement.region,
                span: statement.syntax.span,
              })
            : Object.freeze({
                _tag: statement._tag === 'BreakStatement' ? 'Break' : 'Continue',
                target: statement.target,
                region: statement.region,
                span: statement.syntax.span,
              })
        if (statement._tag === 'ReturnStatement')
          return Object.freeze({
            _tag: 'Return',
            expression:
              options.resultType === undefined
                ? hirExpression(statement.expression)
                : hirExpectedExpression(
                    statement.expression,
                    options.resultType,
                    'Return',
                    statement.syntax.span,
                  ),
            region: statement.region,
            span: statement.expression.syntax.span,
          })
        if (statement._tag === 'DropStatement')
          return Object.freeze({
            _tag: 'Drop',
            expression:
              statement.expression.type._tag === 'Available'
                ? Object.freeze({
                    _tag: 'Move' as const,
                    subject: hirExpression(statement.expression),
                    type: statement.expression.type.type,
                    span: statement.expression.syntax.span,
                  })
                : hirExpression(statement.expression),
            region: statement.region,
            span: statement.syntax.span,
          })
        if (statement.failure === undefined)
          return Object.freeze({
            _tag: 'UnavailableStatement',
            region: statement.region,
            span: statement.syntax.span,
          })
        return Object.freeze({
          _tag: 'Fail',
          expression:
            statement.transfer === 'Move'
              ? Object.freeze({
                  _tag: 'Move',
                  subject: hirExpression(statement.expression),
                  type:
                    statement.expression.type._tag === 'Available'
                      ? statement.expression.type.type
                      : statement.failure,
                  span: statement.expression.syntax.span,
                })
              : hirExpression(statement.expression),
          failure: statement.failure,
          transfer: statement.transfer,
          region: statement.region,
          span: statement.syntax.span,
        })
      }),
  )

export const hirCallableTarget = (reference: CallReferenceFact): Hir.CallableTarget | undefined => {
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
): Hir.BorrowId | undefined =>
  argument.expression._tag === 'Borrow' && argument.expression.formation._tag !== 'Unavailable'
    ? Object.freeze({
        _tag: 'BorrowId',
        function: argument.id.function,
        callSpan: argument.id.callSpan,
        ordinal,
      })
    : undefined

export const loanEndsOf = (
  arguments_: ReadonlyArray<ArgumentFact>,
  retained: (ordinal: number) => boolean = () => true,
): ReadonlyArray<Hir.BorrowId> =>
  Object.freeze(
    arguments_.flatMap((argument, ordinal) => {
      const borrow = argumentBorrowId(argument, ordinal)
      return borrow === undefined || !retained(ordinal) ? [] : [borrow]
    }),
  )

export const hirExpression = (fact: ExpressionFact, borrow?: Hir.BorrowId): Hir.Expression => {
  if (fact._tag === 'ShortCircuit') {
    const left = fact.arguments.at(0)
    const right = fact.arguments.at(1)
    if (left === undefined || right === undefined || fact.type._tag !== 'Available') {
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    }
    const loweredLeft = hirExpression(left.expression)
    const loweredRight = hirExpression(right.expression)
    return loweredLeft._tag === 'Unavailable' || loweredRight._tag === 'Unavailable'
      ? Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
      : Object.freeze({
          _tag: 'ShortCircuit',
          operator: fact.operator,
          left: loweredLeft,
          right: loweredRight,
          type: fact.type.type,
          span: fact.syntax.span,
        })
  }
  if (fact._tag === 'Integer') {
    return fact.integer._tag === 'Available'
      ? Object.freeze({
          _tag: 'IntegerLiteral',
          value: fact.integer.value,
          type: fact.integer.type,
          span: fact.syntax.span,
        })
      : Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
  }
  if (fact._tag === 'Floating') {
    return fact.floating._tag === 'Available'
      ? Object.freeze({
          _tag: 'FloatingLiteral',
          bits: fact.floating.bits,
          spelling: fact.floating.spelling,
          type: fact.floating.type,
          span: fact.syntax.span,
        })
      : Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
  }
  if (fact._tag === 'StaticText') {
    if (fact.data === undefined) {
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    }
    if (fact.data.kind === 'Text') {
      return Object.freeze({
        _tag: 'StaticStringLiteral',
        data: fact.data,
        type: Type.string,
        span: fact.syntax.span,
      })
    }
    return Object.freeze({
      _tag: 'StaticByteViewLiteral',
      data: fact.data,
      type: Type.slice('Shared', 'u8'),
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'Unit') {
    return Object.freeze({
      _tag: 'UnitLiteral',
      type: Type.unit,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'Boolean') {
    return fact.type._tag === 'Available'
      ? Object.freeze({
          _tag: 'BooleanLiteral',
          value: fact.value,
          type: fact.type.type,
          span: fact.syntax.span,
        })
      : Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
  }
  if (fact._tag === 'Character') {
    return fact.type._tag === 'Available' && fact.value !== undefined
      ? Object.freeze({
          _tag: 'CharacterLiteral',
          value: fact.value,
          type: fact.type.type,
          span: fact.syntax.span,
        })
      : Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
  }
  if (fact._tag === 'Constant') {
    if (fact.value?._tag === 'Character')
      return Object.freeze({
        _tag: 'CharacterLiteral',
        value: fact.value.value,
        type: 'char',
        span: fact.syntax.span,
      })
    if (fact.value?._tag === 'Boolean')
      return Object.freeze({
        _tag: 'BooleanLiteral',
        value: fact.value.value,
        type: 'bool',
        span: fact.syntax.span,
      })
    if (fact.value?._tag === 'Integer')
      return Object.freeze({
        _tag: 'IntegerLiteral',
        value: fact.value.value,
        type: fact.value.type,
        ...(fact.declaration.canonical._tag === 'Canonical'
          ? { constant: fact.declaration.canonical.id }
          : {}),
        ...(fact.value.target === undefined ? {} : { targetConstant: fact.value.target }),
        span: fact.syntax.span,
      })
    if (fact.value?._tag === 'Floating')
      return Object.freeze({
        _tag: 'FloatingLiteral',
        bits: fact.value.bits,
        spelling: fact.value.spelling,
        type: fact.value.type,
        span: fact.syntax.span,
      })
    if (fact.value?._tag === 'String')
      return Object.freeze({
        _tag: 'StaticStringLiteral',
        data: fact.value.data,
        type: Type.string,
        span: fact.syntax.span,
      })
    return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
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
        span: fact.syntax.span,
        ...(fact.cause === undefined ? {} : { cause: fact.cause }),
      })
    return Object.freeze({
      _tag: 'EnumMember',
      enum: fact.enum.canonical.id,
      member: fact.member.canonical.id,
      discriminant: fact.member.discriminant.value,
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'EnumValue') {
    const value = hirExpression(fact.argument)
    return fact.type._tag !== 'Available' || value._tag === 'Unavailable'
      ? Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
      : Object.freeze({
          _tag: 'EnumValue',
          enum: fact.operation.enum,
          value,
          intrinsic: fact.operation.intrinsic,
          type: fact.operation.result.spelling,
          span: fact.syntax.span,
        })
  }
  if (fact._tag === 'Identifier') {
    return hirReference(fact.reference, fact.type, fact.syntax.span)
  }
  if (fact._tag === 'Move') {
    const subject = hirExpression(fact.subject)
    if (subject._tag === 'Unavailable' || fact.type._tag !== 'Available') {
      return subject._tag === 'Unavailable'
        ? subject
        : Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    }
    return Object.freeze({
      _tag: 'Move',
      subject:
        subject._tag === 'Project' || subject._tag === 'IndexPlace'
          ? Object.freeze({ ...subject, access: 'ConsumeRequested' as const })
          : subject,
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'PlaceReplace') {
    const place =
      fact.root === undefined ? undefined : hirAssignmentWritePlace(fact.destination, fact.root)
    if (place === undefined || !fact.compatible || fact.type._tag !== 'Available') {
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    }
    return Object.freeze({
      _tag: 'Replace',
      place,
      value: hirExpectedExpression(fact.value, place.type, 'Assignment', place.span),
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'EffectBlock') {
    if (fact.type._tag !== 'Available' || !Type.isEffect(fact.type.type))
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    return Object.freeze({
      _tag: 'EffectBlock',
      site: fact.site,
      statements: lowerStatements(fact.statements, { resultType: fact.type.type.success }),
      captures: Object.freeze(
        fact.captures.map((capture) =>
          Object.freeze({
            ...(capture.reference._tag === 'BindingFact'
              ? { binding: capture.reference.id }
              : { parameter: capture.reference.id }),
            access: capture.access,
            span: capture.span,
          }),
        ),
      ),
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'Run') {
    const subject = hirExpression(fact.subject)
    if (subject._tag === 'Unavailable' || fact.type._tag !== 'Available')
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    return Object.freeze({
      _tag: 'Run',
      subject,
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'EffectResult') {
    const protected_ = hirExpression(fact.protected)
    if (
      protected_._tag === 'Unavailable' ||
      fact.type._tag !== 'Available' ||
      !Type.isEffect(fact.type.type)
    )
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    return Object.freeze({
      _tag: 'EffectResult',
      protected: protected_,
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'EffectCatch') {
    const protected_ = hirExpression(fact.protected)
    const handler = hirExpression(fact.handler)
    if (
      protected_._tag === 'Unavailable' ||
      handler._tag === 'Unavailable' ||
      fact.reference._tag !== 'ResolvedIntrinsicReference' ||
      fact.selected === undefined ||
      fact.type._tag !== 'Available' ||
      !Type.isEffect(fact.type.type)
    )
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    return Object.freeze({
      _tag: 'EffectCatch',
      intrinsic: fact.reference.operation.id,
      protected: protected_,
      handler,
      selected: fact.selected,
      protectedRow: fact.protectedRow,
      handlerRow: fact.handlerRow,
      residualRow: fact.residualRow,
      evidence: fact.evidence,
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'EffectBindRequirement') {
    const protected_ = hirExpression(fact.protected)
    if (
      protected_._tag === 'Unavailable' ||
      fact.provider === undefined ||
      fact.type._tag !== 'Available' ||
      !Type.isEffect(fact.type.type)
    )
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    return Object.freeze({
      _tag: 'EffectBindRequirement',
      protected: protected_,
      provider: Object.freeze({
        ...(fact.provider.reference._tag === 'BindingFact'
          ? { binding: fact.provider.reference.id }
          : { parameter: fact.provider.reference.id }),
        selected: fact.provider.selected,
        evidence: fact.provider.evidence,
        ...(fact.provider.capability === undefined ? {} : { capability: fact.provider.capability }),
        providerType: fact.provider.providerType,
        ...(fact.provider.witness === undefined ? {} : { witness: fact.provider.witness }),
        ...(fact.provider.role === undefined ? {} : { role: fact.provider.role }),
        selectionAccess: fact.provider.selectionAccess,
        captureAccess: fact.provider.captureAccess,
        span: fact.provider.span,
      }),
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'Match') {
    const scrutinee = hirExpression(fact.scrutinee)
    if (scrutinee._tag === 'Unavailable' || fact.type._tag !== 'Available') {
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    }
    const target = fact.type.type
    return Object.freeze({
      _tag: 'Match',
      id: fact.id,
      access: fact.access,
      scrutinee,
      members: fact.members,
      arms: Object.freeze(
        fact.arms.map((arm) => {
          let member: Match.CoverageIdentity | undefined
          if (arm.pattern._tag === 'EnumMemberPattern') {
            member = arm.pattern.coverage
          } else if (
            (arm.pattern._tag === 'NominalPattern' || arm.pattern._tag === 'TypePattern') &&
            arm.pattern.member !== undefined
          ) {
            member = Match.structuralMember(arm.pattern.member)
          }
          return Object.freeze({
            id: arm.id,
            ...(member === undefined ? {} : { member }),
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
                        span: binding.syntax.span,
                      }),
                    ]
                  : [],
              ),
            ),
            cleanup: arm.pattern.omitted,
            ...(arm.guard === undefined ? {} : { guard: hirExpression(arm.guard) }),
            result: Type.isUnion(target)
              ? hirExpectedExpression(arm.result, target, 'MatchArm', arm.syntax.span)
              : hirExpression(arm.result),
            before: arm.before,
            after: arm.after,
            reachable: arm.reachable,
            span: arm.syntax.span,
          })
        }),
      ),
      type: target,
      span: fact.syntax.span,
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
        span: fact.syntax.span,
        ...(fact.target._tag === 'Unavailable' && fact.target.cause !== undefined
          ? { cause: fact.target.cause }
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
              ? hirExpectedExpression(
                  initializer.expression,
                  Type.substitute(field.declaredType.type, substitution),
                  'StructField',
                  field.syntax.span,
                )
              : hirExpression(initializer.expression)
          return Object.freeze({ field: field.id, value })
        }),
      ),
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'ArrayLiteral') {
    if (fact.state._tag !== 'Complete' || fact.type._tag !== 'Available') {
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    }
    return Object.freeze({
      _tag: 'ArrayConstruct',
      elements: Object.freeze(
        fact.elements.map((element) =>
          element.expected === undefined
            ? hirExpression(element.expression)
            : hirExpectedExpression(
                element.expression,
                element.expected,
                'ArrayElement',
                element.syntax.span,
              ),
        ),
      ),
      type: fact.state.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'FieldProjection') {
    if (fact.state._tag === 'SliceLength' && fact.type._tag === 'Available') {
      const slice = hirExpression(fact.subject)
      return slice._tag === 'Unavailable'
        ? slice
        : Object.freeze({
            _tag: 'SliceLength',
            slice,
            type: 'usize',
            span: fact.syntax.span,
          })
    }
    if (
      fact.nominal === undefined ||
      fact.state._tag !== 'Resolved' ||
      fact.type._tag !== 'Available'
    ) {
      return Object.freeze({
        _tag: 'Unavailable',
        span: fact.syntax.span,
        ...(fact.state._tag === 'Unavailable' && fact.state.cause !== undefined
          ? { cause: fact.state.cause }
          : {}),
      })
    }
    return Object.freeze({
      _tag: 'Project',
      subject: hirExpression(fact.subject),
      nominal: fact.nominal,
      field: fact.state.field.id,
      access: 'CopyRead',
      ...(fact.borrowAccess === undefined ? {} : { borrowAccess: fact.borrowAccess }),
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'IndexProjection') {
    if (
      fact.slice !== undefined &&
      fact.type._tag === 'Available' &&
      fact.bounds._tag === 'RuntimeSlice'
    ) {
      const slice = hirExpression(fact.subject)
      const index = hirExpression(fact.index)
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
        span: fact.syntax.span,
      })
    }
    if (
      fact.array === undefined ||
      fact.type._tag !== 'Available' ||
      (fact.bounds._tag !== 'Proven' && fact.bounds._tag !== 'Runtime')
    ) {
      return Object.freeze({
        _tag: 'Unavailable',
        span: fact.syntax.span,
        ...(fact.bounds._tag === 'Invalid' ? { cause: fact.bounds.cause } : {}),
      })
    }
    const subject = hirExpression(fact.subject)
    const index = hirExpression(fact.index)
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
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'Borrow') {
    if (
      borrow === undefined ||
      fact.formation._tag === 'Unavailable' ||
      fact.type._tag !== 'Available' ||
      (!Type.isSlice(fact.type.type) && !Type.isReference(fact.type.type))
    ) {
      return Object.freeze({
        _tag: 'Unavailable',
        span: fact.syntax.span,
        ...(fact.formation._tag === 'Unavailable' && fact.formation.cause !== undefined
          ? { cause: fact.formation.cause }
          : {}),
      })
    }
    let root: Hir.SliceRoot
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
          value: hirExpression(fact.formation.root.value),
        })
        break
    }
    const selectors: Array<Hir.BorrowSelector> = []
    for (const selector of fact.formation.root.path) {
      if (selector._tag === 'Field') {
        selectors.push(
          Object.freeze({
            _tag: 'Field',
            field: selector.field,
            span: selector.span,
          }),
        )
        continue
      }
      if (selector._tag === 'SliceIndex') {
        const index = hirExpression(selector.index)
        if (index._tag === 'Unavailable') {
          return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
        }
        selectors.push(
          Object.freeze({
            _tag: 'SliceIndex',
            index,
            slice: selector.slice,
            span: selector.span,
          }),
        )
        continue
      }
      const index = hirExpression(selector.index)
      if (index._tag === 'Unavailable') {
        return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
      }
      selectors.push(
        Object.freeze({
          _tag: 'Index',
          index,
          array: selector.array,
          bounds: selector.bounds,
          span: selector.span,
        }),
      )
    }
    if (fact.formation._tag === 'ValueBorrow' && Type.isReference(fact.type.type)) {
      return Object.freeze({
        _tag: 'ValueBorrow',
        borrow,
        root,
        selectors: Object.freeze(selectors),
        source: fact.formation.source,
        access: fact.access,
        type: fact.type.type,
        span: fact.syntax.span,
      })
    }
    if (fact.formation._tag === 'ValueBorrow')
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    if (!Type.isSlice(fact.type.type))
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    return Object.freeze({
      _tag: 'SliceBorrow',
      borrow,
      root,
      selectors: Object.freeze(selectors),
      source:
        fact.formation._tag === 'FixedArrayBorrow' ? fact.formation.array : fact.formation.parent,
      access: fact.access,
      reborrow: fact.formation._tag === 'SliceReborrow',
      suspendsParent: fact.formation._tag === 'SliceReborrow' && fact.formation.suspendsParent,
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'Grouped') return hirExpression(fact.expression)
  if (fact._tag === 'FunctionItem') {
    const target = hirCallableTarget(fact.reference)
    if (target === undefined || fact.type._tag !== 'Available' || !Type.isCallable(fact.type.type))
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    return Object.freeze({
      _tag: 'FunctionItem',
      target,
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'CallableSection') {
    const target = hirCallableTarget(fact.reference)
    if (target === undefined || fact.type._tag !== 'Available' || !Type.isCallable(fact.type.type))
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
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
            value: hirExpression(
              capture.expression,
              Object.freeze({
                _tag: 'BorrowId',
                function: fact.site.function,
                callSpan: fact.syntax.span,
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
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'CallableApply') {
    if (fact.type._tag !== 'Available')
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    const returnedBorrowOrdinal = returnedBorrowArgument(fact)?.id.ordinal
    const returnedCapture =
      fact.returnedBorrowSource?._tag === 'Capture' ? fact.returnedBorrowSource.capture : undefined
    const returnedSection = callableSectionOf(fact.callee)
    const returnedCaptureLoan =
      returnedCapture === undefined || returnedSection === undefined
        ? undefined
        : Object.freeze({
            _tag: 'BorrowId' as const,
            function: returnedSection.site.function,
            callSpan: returnedSection.syntax.span,
            ordinal: returnedCapture.ordinal,
          })
    return Object.freeze({
      _tag: 'CallableApply',
      callee: hirExpression(fact.callee),
      arguments: Object.freeze(
        fact.arguments.map((argument, ordinal) =>
          hirExpression(argument.expression, argumentBorrowId(argument, ordinal)),
        ),
      ),
      loanEnds: loanEndsOf(fact.arguments, (ordinal) => returnedBorrowOrdinal !== ordinal),
      heldLoans: Object.freeze([
        ...loanEndsOf(fact.arguments, (ordinal) => returnedBorrowOrdinal === ordinal),
        ...(returnedCaptureLoan === undefined ? [] : [returnedCaptureLoan]),
      ]),
      access: fact.mode,
      substitution: fact.substitution,
      evaluation:
        fact.provenance._tag === 'PipelineCallableApplication'
          ? 'LeftThenCallable'
          : 'CalleeThenArguments',
      realization: fact.callee._tag === 'CallableSection' ? 'DirectErasedSection' : 'Environment',
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (
    fact.reference._tag === 'ResolvedBoundOperation' &&
    fact.contract._tag === 'Compatible' &&
    fact.type._tag === 'Available'
  ) {
    const borrowIds = loanEndsOf(fact.arguments)
    return Object.freeze({
      _tag: 'BoundOperationCall',
      capability: fact.reference.capability,
      provider: fact.reference.provider,
      operation: fact.reference.operation,
      contract: fact.reference.interfaceContract,
      ...(fact.witnessEffectSite === undefined
        ? {}
        : { witnessEffectSite: fact.witnessEffectSite }),
      arguments: Object.freeze(
        fact.arguments.map((argument, ordinal) =>
          hirExpression(argument.expression, argumentBorrowId(argument, ordinal)),
        ),
      ),
      loanEnds: borrowIds,
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (
    fact._tag === 'Operator' &&
    fact.reference._tag === 'ResolvedEnumEquality' &&
    fact.type._tag === 'Available'
  ) {
    const leftFact = fact.arguments.at(0)
    const rightFact = fact.arguments.at(1)
    const left = leftFact === undefined ? undefined : hirExpression(leftFact.expression)
    const right = rightFact === undefined ? undefined : hirExpression(rightFact.expression)
    return left === undefined ||
      right === undefined ||
      left._tag === 'Unavailable' ||
      right._tag === 'Unavailable'
      ? Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
      : Object.freeze({
          _tag: 'EnumEquality',
          enum: fact.reference.enum,
          left,
          right,
          negated: fact.reference.operator === 'NotEquals',
          type: Scalar.boolean.spelling,
          span: fact.syntax.span,
        })
  }
  if (
    fact.reference._tag === 'ResolvedBuiltin' &&
    fact.contract._tag === 'Compatible' &&
    fact.type._tag === 'Available'
  ) {
    const returnedBorrowParameter = fact.reference.returnedBorrowParameter
    const directLoanEnds = loanEndsOf(
      fact.arguments,
      (ordinal) => returnedBorrowParameter !== ordinal,
    )
    const nestedSlotLoanEnds =
      fact.reference.operation === 'SlotWrite' ||
      fact.reference.operation === 'SlotTake' ||
      fact.reference.operation === 'SlotCopy' ||
      fact.reference.operation === 'SlotDrop'
        ? fact.arguments.flatMap((argument): ReadonlyArray<Hir.BorrowId> => {
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
        return hirExpression(argument.expression, borrowId)
      }),
    )
    const heldLoans = Object.freeze(
      fact.reference.operation === 'RawBufferSlot'
        ? directLoanEnds
        : loanEndsOf(fact.arguments, (ordinal) => returnedBorrowParameter === ordinal),
    )
    if (fact.reference.operation === 'StringFromUtf8Unchecked') {
      const source = arguments_.at(0)
      return source === undefined || source._tag === 'Unavailable'
        ? Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
        : Object.freeze({
            _tag: 'RuntimeStringView',
            source,
            heldLoans,
            type: Type.string,
            span: fact.syntax.span,
          })
    }
    if (fact.reference.operation === 'StringEqualsExact') {
      const left = arguments_.at(0)
      const right = arguments_.at(1)
      return left === undefined ||
        right === undefined ||
        left._tag === 'Unavailable' ||
        right._tag === 'Unavailable'
        ? Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
        : Object.freeze({
            _tag: 'StringEquality',
            left,
            right,
            negated: fact._tag === 'Operator' && fact.operator === 'NotEquals',
            intrinsic: fact.reference.intrinsic,
            type: Scalar.boolean.spelling,
            span: fact.syntax.span,
          })
    }
    return Object.freeze({
      _tag: 'BuiltinCall',
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
      loanEnds: Object.freeze(
        fact.reference.operation === 'RawBufferSlot'
          ? []
          : [...directLoanEnds, ...nestedSlotLoanEnds],
      ),
      heldLoans,
      type: fact.type.type,
      span: fact.syntax.span,
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
    const serviceArguments = fact.contract.typeArguments
      .slice(0, fact.reference.service.typeParameters.length)
      .filter(Type.isTypeArgument)
    const service = Type.nominal(
      fact.reference.service.canonical.id.module,
      fact.reference.service.canonical.id.name,
      serviceArguments,
    )
    const requirement = Type.requirementMembers(fact.type.type).find((candidate) =>
      Type.equals(candidate.capability, service),
    )
    if (requirement === undefined)
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    const substitution = fact.contract.substitution
    const target = fact.reference.operation
    return Object.freeze({
      _tag: 'ServiceEffectConstruct',
      service,
      operation: fact.reference.operation.name.spelling,
      role: requirement.role,
      access: requirement.access,
      typeArguments: fact.contract.typeArguments,
      arguments: Object.freeze(
        fact.arguments.map((argument, ordinal) => {
          const parameter = target.parameters.at(ordinal)
          const borrowId = argumentBorrowId(argument, ordinal)
          return parameter?.declaredType._tag === 'Resolved'
            ? hirExpectedExpression(
                argument.expression,
                Type.substitute(parameter.declaredType.type, substitution),
                'Argument',
                parameter.syntax.span,
                borrowId,
              )
            : hirExpression(argument.expression, borrowId)
        }),
      ),
      loanEnds: loanEndsOf(fact.arguments),
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (
    fact.reference._tag === 'Resolved' &&
    fact.reference.declaration.canonical._tag === 'Canonical' &&
    fact.contract._tag === 'Compatible' &&
    fact.type._tag === 'Available'
  ) {
    const target = fact.reference.declaration
    const substitution = fact.contract.substitution
    const returnedBorrowOrdinal = returnedBorrowArgument(fact)?.id.ordinal
    const call = {
      target: fact.reference.declaration.canonical.id,
      typeArguments: fact.contract.typeArguments,
      arguments: Object.freeze(
        fact.arguments.map((argument, ordinal) => {
          const parameter = target.parameters.at(ordinal)
          const borrowId = argumentBorrowId(argument, ordinal)
          return parameter?.declaredType._tag === 'Resolved'
            ? hirExpectedExpression(
                argument.expression,
                Type.substitute(parameter.declaredType.type, substitution),
                'Argument',
                parameter.syntax.span,
                borrowId,
              )
            : hirExpression(argument.expression, borrowId)
        }),
      ),
      loanEnds: loanEndsOf(fact.arguments, (ordinal) => returnedBorrowOrdinal !== ordinal),
      heldLoans: loanEndsOf(fact.arguments, (ordinal) => returnedBorrowOrdinal === ordinal),
      type: fact.type.type,
      span: fact.syntax.span,
    }
    return Type.isEffect(fact.type.type) &&
      fact.reference._tag === 'Resolved' &&
      fact.reference.declaration.functionKind === 'Effect'
      ? Object.freeze({ ...call, _tag: 'EffectConstruct' as const, type: fact.type.type })
      : Object.freeze({ ...call, _tag: 'Call' as const })
  }
  let cause: Diagnostic.Identity | undefined
  if (fact.reference._tag === 'Missing' || fact.reference._tag === 'Ambiguous') {
    cause = fact.reference.cause
  } else if (fact.contract._tag === 'Unavailable') {
    cause = fact.contract.cause
  }
  return Object.freeze({
    _tag: 'Unavailable',
    span: fact.syntax.span,
    ...(cause === undefined ? {} : { cause }),
  })
}

export const hirExpectedExpression = (
  fact: ExpressionFact,
  target: SemanticType,
  context: Extract<Hir.Expression, { readonly _tag: 'UnionConvert' }>['context'],
  expectedAt: SourceSpan.SourceSpan,
  borrow?: Hir.BorrowId,
): Hir.Expression => {
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
      span: fact.syntax.span,
    })
  const loweredSource = hirExpression(fact, borrow)
  if (loweredSource._tag === 'Unavailable') return loweredSource
  const unionTarget = Type.isUnion(target) ? target : undefined
  const representation = unionTarget === undefined ? undefined : representationOfExpression(fact)
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
  if (Type.isRepresented(target) && Type.haveSameRepresentationShape(source.type, target))
    return source
  const compatibility = TypeCompatibility.check(representedSource ?? source.type, target)
  if (compatibility._tag === 'Exact') return source
  if (
    compatibility._tag === 'CallableMode' ||
    compatibility._tag === 'EffectAccess' ||
    compatibility._tag === 'ReferenceAccess'
  )
    return source
  if (compatibility._tag === 'Bottom') return source
  if (compatibility._tag === 'Incompatible') {
    return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
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
    expectedAt,
    type: compatibility.target,
    span: fact.syntax.span,
  })
}

export const hirWritePlace = (
  fact: ExpressionFact,
  root: AssignmentRootFact,
): Hir.WritePlace | undefined => {
  const selectors: Array<Hir.WriteSelector> = []
  const walk = (current: ExpressionFact): boolean => {
    if (current._tag === 'Grouped') return walk(current.expression)
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
          span: current.syntax.span,
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
      const index = hirExpression(current.index)
      if (index._tag === 'Unavailable') return false
      selectors.push(
        Object.freeze({
          _tag: 'Index',
          index,
          array: current.array,
          bounds: current.bounds,
          type: current.type.type,
          span: current.syntax.span,
        }),
      )
      return true
    }
    return false
  }
  if (!walk(fact) || fact.type._tag !== 'Available') return undefined
  return Object.freeze({
    _tag: 'WritePlace',
    root:
      root._tag === 'ParameterDeclaration'
        ? Object.freeze({ _tag: 'ParameterWriteRoot' as const, parameter: root.id })
        : Object.freeze({ _tag: 'BindingWriteRoot' as const, binding: root.id }),
    selectors: Object.freeze(selectors),
    type: fact.type.type,
    span: fact.syntax.span,
  })
}

export const assignmentRootType = (root: AssignmentRootFact): SemanticType | undefined => {
  if (root._tag === 'ParameterDeclaration') {
    return root.declaredType._tag === 'Resolved' ? root.declaredType.type : undefined
  }
  return root.inferredType._tag === 'Available' ? root.inferredType.type : undefined
}

export const hirBorrowedWritePlace = (
  fact: ExpressionFact,
  root: AssignmentRootFact,
): Hir.BorrowedWritePlace | undefined => {
  const rootType = assignmentRootType(root)
  if (
    rootType === undefined ||
    !(Type.isSlice(rootType) || Type.isReference(rootType)) ||
    rootType.access !== 'Exclusive'
  ) {
    return undefined
  }
  const selectors: Array<Hir.BorrowedWriteSelector> = []
  const walk = (current: ExpressionFact): boolean => {
    if (current._tag === 'Grouped') return walk(current.expression)
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
        current.type._tag !== 'Available' ||
        current.borrowAccess !== 'Exclusive'
      ) {
        return false
      }
      selectors.push(
        Object.freeze({
          _tag: 'Field',
          field: current.state.field.id,
          type: current.type.type,
          span: current.syntax.span,
        }),
      )
      return true
    }
    if (current._tag === 'IndexProjection') {
      if (
        !walk(current.subject) ||
        current.slice === undefined ||
        current.slice.access !== 'Exclusive' ||
        current.type._tag !== 'Available' ||
        current.bounds._tag !== 'RuntimeSlice'
      ) {
        return false
      }
      const index = hirExpression(current.index)
      if (index._tag === 'Unavailable') return false
      selectors.push(
        Object.freeze({
          _tag: 'SliceIndex',
          index,
          slice: current.slice,
          type: current.type.type,
          span: current.syntax.span,
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
    slice: rootType,
    selectors: Object.freeze(selectors),
    type: fact.type.type,
    span: fact.syntax.span,
  })
}

export const hirAssignmentWritePlace = (
  fact: ExpressionFact,
  root: AssignmentRootFact,
): Hir.WritePlace | undefined => {
  const access = assignmentRootAccess(root)
  if (access === 'ExclusiveBorrowed') return hirBorrowedWritePlace(fact, root)
  if (access === 'MutableOwned') return hirWritePlace(fact, root)
  return undefined
}

export const statementSpan = (statement: StatementFact): SourceSpan.SourceSpan =>
  statement._tag === 'BindStatement' ? statement.binding.syntax.span : statement.syntax.span

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
    case 'EnumValue':
      return Object.freeze([expression.argument])
    case 'Move':
    case 'Borrow':
    case 'FieldProjection':
    case 'Run':
      return Object.freeze([expression.subject])
    case 'PlaceReplace':
      return Object.freeze([expression.destination, expression.value])
    case 'IndexProjection':
      return Object.freeze([expression.subject, expression.index])
    case 'ArrayLiteral':
      return Object.freeze(expression.elements.map((element) => element.expression))
    case 'StructLiteral':
      return Object.freeze(expression.initializers.map((initializer) => initializer.expression))
    case 'Grouped':
      return Object.freeze([expression.expression])
    case 'EffectResult':
      return Object.freeze([expression.protected])
    case 'EffectBindRequirement':
      return Object.freeze([expression.protected])
    case 'EffectCatch':
      return Object.freeze([expression.protected, expression.handler])
    case 'CallableSection':
      return Object.freeze(expression.captures.map((capture) => capture.expression))
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
    case 'Floating':
    case 'StaticText':
    case 'Character':
    case 'Unit':
    case 'Boolean':
    case 'Constant':
    case 'EnumMember':
    case 'Identifier':
    case 'FunctionItem':
      return Object.freeze([])
  }
}

/** Callbacks for one deterministic traversal of elaborated statement and expression facts. */
