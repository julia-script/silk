import * as Option from 'effect/Option'
import * as CallableContract from './CallableContract.js'
import * as ConformanceProof from './ConformanceProof.js'
import * as Constraint from './Constraint.js'
import * as DeclarationCollection from './DeclarationCollection.js'
import * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as DeclarationResolution from './DeclarationResolution.js'
import * as Diagnostic from './Diagnostic.js'
import type {
  ArgumentFact,
  ArgumentsResult,
  ArrayElementFact,
  ArrayLiteralState,
  BindingDeclarationFact,
  BorrowRootFact,
  BoundsFact,
  BuiltinArgumentMappingFact,
  CallableApplyExpressionFact,
  CallableSectionExpressionFact,
  CallReferenceFact,
  ConstantExpressionFact,
  DeclarationFact,
  DeclarationId,
  DeclaredName,
  EffectCaptureFact,
  EffectRequirementBindingFact,
  ExpressionFact,
  ExpressionResult,
  ExpressionTypeFact,
  FloatingExpressionFact,
  FunctionFact,
  IdentifierExpressionFact,
  IdentifierResult,
  IntegerResult,
  InterfaceOperationFact,
  IntrinsicReferenceFact,
  MatchArmFact,
  MatchExpressionFact,
  MoveExpressionFact,
  ParameterFact,
  ParameterReferenceFact,
  PatternBindingFact,
  PatternFact,
  PatternFieldFact,
  PatternFieldState,
  ProjectionState,
  SemanticType,
  StatementFact,
  StructInitializerFact,
  StructInitializerState,
  StructTargetFact,
  StructTypeArgumentFact,
} from './Elaboration.js'
import {
  argumentFact,
  assignmentRoot,
  availableBoolExpressionType,
  availableExpressionType,
  callCallee,
  callReferenceTokens,
  childNode,
  contextualIntegerCompatible,
  directToken,
  isAvailableSyntax,
  isExpressionNode,
  isRecursiveArgumentNode,
  lookupDeclaration,
  lookupParameter,
  pipelineCallable,
  pipelineInput,
  referencePath,
  representationJoinDiagnostic,
  spelling,
  typesCompatible,
  unavailableElement,
  unavailableExpressionType,
  unavailableSyntax,
  unionConversionDiagnostic,
} from './Elaboration.js'
import * as FloatingPoint from './FloatingPoint.js'
import * as Hir from './Hir.js'
import * as Intrinsic from './Intrinsic.js'
import * as DigitSeparator from './internal/DigitSeparator.js'
import * as IntegerLiteral from './internal/IntegerLiteral.js'
import * as TypeInference from './internal/TypeInference.js'
import * as LiteralForm from './LiteralForm.js'
import * as Match from './Match.js'
import * as NameResolution from './NameResolution.js'
import * as Operator from './Operator.js'
import * as RowAlgebra from './RowAlgebra.js'
import * as Scalar from './Scalar.js'
import * as SourceFile from './SourceFile.js'
import * as SourceSpan from './SourceSpan.js'
import { analyzeStatements, unsafeCallDiagnostic } from './StatementAnalysis.js'
import * as StaticText from './StaticText.js'
import * as SyntaxTree from './SyntaxTree.js'
import * as TargetConstant from './TargetConstant.js'
import type * as Token from './Token.js'
import * as Type from './Type.js'

export const analyzeInteger = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  expected?: SemanticType,
): IntegerResult => {
  const token = directToken(node, 'DecimalInteger')
  if (token === undefined) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Unavailable',
        syntax: unavailableSyntax(node, 'DecimalInteger'),
      }),
      diagnostics: Object.freeze([]),
    })
  }

  const minusToken = directToken(node, 'Minus')
  const negative = minusToken !== undefined
  const literalSpan =
    minusToken === undefined
      ? token.span
      : Option.getOrElse(
          SourceSpan.make(source, minusToken.span.start, token.span.end),
          () => token.span,
        )
  const bytes = Option.getOrThrowWith(
    SourceFile.slice(source, token.span),
    () => new RangeError(`Semantic integer span does not belong to source ${source.id}`),
  )
  const selected =
    typeof expected === 'string' && Scalar.isIntegerSpelling(expected)
      ? Scalar.find(expected)
      : Scalar.defaultInteger
  if (selected === undefined || selected.category !== 'Integer')
    throw new RangeError('The scalar catalog lost its default integer')
  const magnitude = IntegerLiteral.magnitude(bytes)
  const value = negative ? -magnitude : magnitude
  // Target-width integers are retained against the widest admitted target here. The concrete
  // target validates its selected 32- or 64-bit range before MIR is committed.
  const range = Scalar.range(selected, 64)
  if (value >= range.minimum && value <= range.maximum) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Available',
        type: selected.spelling,
        value,
        token,
        syntax: node,
      }),
      diagnostics: Object.freeze([]),
    })
  }

  const digits = Array.from(bytes, (byte) => String.fromCharCode(byte)).join('')
  const tokenSpelling = negative ? `-${digits}` : digits
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'OutOfRange',
      type: selected.spelling,
      spelling: tokenSpelling,
      token,
      syntax: node,
    }),
    diagnostics: Object.freeze([
      selected.spelling === 'usize' && negative
        ? Diagnostic.usizeNegative(tokenSpelling, literalSpan)
        : Diagnostic.integerOutOfRange(tokenSpelling, literalSpan),
    ]),
  })
}

export const analyzeFloating = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  expected?: SemanticType,
): {
  readonly fact: FloatingExpressionFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  const unavailable = (
    diagnostics: ReadonlyArray<Diagnostic.Diagnostic>,
  ): { readonly fact: FloatingExpressionFact; readonly diagnostics: typeof diagnostics } =>
    Object.freeze({ fact: Object.freeze({ _tag: 'Unavailable', syntax: node }), diagnostics })
  const token = directToken(node, 'DecimalFloat')
  if (token === undefined) return unavailable(Object.freeze([]))
  const bytes = Option.getOrThrowWith(
    SourceFile.slice(source, token.span),
    () => new RangeError(`Semantic float span does not belong to source ${source.id}`),
  )
  const unsigned = DigitSeparator.strip(bytes)
  const spelling = directToken(node, 'Minus') === undefined ? unsigned : `-${unsigned}`
  const selected = Scalar.isFloatSpelling(expected) ? expected : Scalar.defaultFloat.spelling
  const encoded = FloatingPoint.fromDecimal(spelling, selected === 'f32' ? 32 : 64)
  // A spelling the lexer accepted but no float can represent must never pass silently.
  if (encoded === undefined) {
    return unavailable(Object.freeze([Diagnostic.invalidFloatLiteral(spelling, node.span)]))
  }
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Available',
      type: selected,
      bits: encoded.bits,
      spelling,
      token,
      syntax: node,
    }),
    diagnostics: Object.freeze([]),
  })
}

export const analyzeConstant = (
  declaration: DeclarationFacts.ConstantFact,
  token: Token.Token,
  syntax: SyntaxTree.Node,
  reportDiagnostic: boolean,
): ExpressionResult => {
  const declared = declaration.declaredType
  const literal = declaration.literal
  let value: ConstantExpressionFact['value']
  let type: SemanticType | undefined
  let detail: string | undefined

  if (declared._tag !== 'Resolved' || typeof declared.type !== 'string') {
    detail = 'the declared type must be one primitive scalar or string'
  } else if (literal._tag === 'Malformed') {
    detail = literal.detail
  } else if (literal._tag === 'Unavailable') {
    detail = 'the initializer must be one literal'
  } else if (Type.isString(declared.type) && literal._tag === 'StringLiteral') {
    if (literal.data.kind !== 'Text') detail = 'a byte-string literal does not produce a string'
    else {
      type = Type.string
      value = Object.freeze({ _tag: 'String', data: literal.data })
    }
  } else if (declared.type === 'bool' && literal._tag === 'BooleanLiteral') {
    type = 'bool'
    value = Object.freeze({ _tag: 'Boolean', value: literal.value })
  } else if (declared.type === 'char' && literal._tag === 'CharacterLiteral') {
    type = 'char'
    value = Object.freeze({ _tag: 'Character', value: literal.value })
  } else if (Scalar.isIntegerSpelling(declared.type) && literal._tag === 'IntegerLiteral') {
    const scalar = Scalar.find(declared.type)
    if (scalar === undefined || scalar.category !== 'Integer') {
      detail = `unknown integer type ${declared.type}`
    } else {
      const range = Scalar.range(scalar, 64)
      if (literal.value < range.minimum || literal.value > range.maximum) {
        detail = `${literal.spelling} does not fit ${declared.type}`
      } else {
        type = declared.type
        value = Object.freeze({ _tag: 'Integer', value: literal.value, type })
      }
    }
  } else if (literal._tag === 'TargetConstant') {
    // The pointer width is not known here — elaboration precedes target selection — so the fact is
    // recorded with its widest value and its selector. `Lower` narrows it once the target is fixed.
    const expected = TargetConstant.declaredType(literal.selector)
    if (declared.type !== expected) {
      detail = `${TargetConstant.root}.${literal.selector} is ${expected}, not ${Type.encode(declared.type)}`
    } else {
      type = expected
      value = Object.freeze({
        _tag: 'Integer',
        value: TargetConstant.unselected(literal.selector),
        type,
        target: literal.selector,
      })
    }
  } else if (Scalar.isFloatSpelling(declared.type) && literal._tag === 'FloatingLiteral') {
    const selected = declared.type
    const encoded = FloatingPoint.fromDecimal(literal.spelling, selected === 'f32' ? 32 : 64)
    if (encoded === undefined) detail = `${literal.spelling} is not a valid ${selected} literal`
    else {
      type = selected
      value = Object.freeze({
        _tag: 'Floating',
        bits: encoded.bits,
        spelling: literal.spelling,
        type: selected,
      })
    }
  } else {
    detail = `the literal kind does not match ${declared._tag === 'Resolved' ? Type.encode(declared.type) : 'the declared type'}`
  }

  const diagnostic =
    detail === undefined
      ? undefined
      : Diagnostic.invalidConstant(detail, declaration.initializer.span)
  const expressionType =
    type === undefined ? unavailableExpressionType : availableExpressionType(type)
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Constant',
      declaration,
      token,
      ...(value === undefined ? {} : { value }),
      type: expressionType,
      syntax,
    }),
    diagnostics: Object.freeze(reportDiagnostic && diagnostic !== undefined ? [diagnostic] : []),
    type,
  })
}

/** The value names visible at one body position: parameters plus completed bindings. */
export interface Scope {
  readonly parameters: ReadonlyArray<ParameterFact>
  readonly bindings: ReadonlyArray<BindingDeclarationFact>
  readonly patternBindings: ReadonlyArray<PatternBindingFact>
}

export interface ValueResolution {
  readonly reference: ParameterReferenceFact
  readonly type: ExpressionTypeFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

export const resolveValueName = (
  scope: Scope,
  tokenSpelling: string,
  token: Token.Token,
): ValueResolution => {
  const binding = scope.bindings.findLast(
    (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === tokenSpelling,
  )
  if (binding !== undefined) {
    return Object.freeze({
      reference: Object.freeze({
        _tag: 'ResolvedBinding' as const,
        spelling: tokenSpelling,
        token,
        binding,
      }),
      type: binding.inferredType,
      diagnostics: Object.freeze([]),
    })
  }
  const patternBinding = scope.patternBindings.findLast(
    (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === tokenSpelling,
  )
  if (patternBinding !== undefined) {
    return Object.freeze({
      reference: Object.freeze({
        _tag: 'ResolvedPattern' as const,
        spelling: tokenSpelling,
        token,
        binding: patternBinding,
      }),
      type: patternBinding.type,
      diagnostics: Object.freeze([]),
    })
  }
  const lookup = lookupParameter(scope.parameters, tokenSpelling)
  if (lookup._tag === 'Resolved') {
    return Object.freeze({
      reference: Object.freeze({
        _tag: 'Resolved' as const,
        spelling: tokenSpelling,
        token,
        parameter: lookup.parameter,
      }),
      type:
        lookup.parameter.declaredType._tag === 'Resolved'
          ? availableExpressionType(lookup.parameter.declaredType.type)
          : unavailableExpressionType,
      diagnostics: Object.freeze([]),
    })
  }
  if (lookup._tag === 'Ambiguous') {
    return Object.freeze({
      reference: Object.freeze({
        _tag: 'Ambiguous' as const,
        spelling: tokenSpelling,
        token,
        parameters: lookup.parameters,
      }),
      type: unavailableExpressionType,
      diagnostics: Object.freeze([]),
    })
  }
  const missingDiagnostic = Diagnostic.unknownValueReference(tokenSpelling, token.span)
  return Object.freeze({
    reference: Object.freeze({
      _tag: 'Missing' as const,
      spelling: tokenSpelling,
      token,
      cause: Diagnostic.identity(missingDiagnostic),
    }),
    type: unavailableExpressionType,
    diagnostics: Object.freeze([missingDiagnostic]),
  })
}

export const analyzeIdentifier = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  scope: Scope,
): IdentifierResult => {
  const token = directToken(node, 'Identifier')
  if (token === undefined || !node.children.every(isAvailableSyntax)) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Identifier',
        reference: Object.freeze({
          _tag: 'Unavailable',
          syntax:
            token === undefined
              ? unavailableSyntax(node, 'Identifier')
              : unavailableElement(node.children, node),
        }),
        type: unavailableExpressionType,
        syntax: node,
      }),
      diagnostics: Object.freeze([]),
      type: undefined,
      syntax: node,
    })
  }

  const resolution = resolveValueName(scope, spelling(source, token), token)
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Identifier',
      reference: resolution.reference,
      type: resolution.type,
      syntax: node,
    }),
    diagnostics: resolution.diagnostics,
    type: resolution.type._tag === 'Available' ? resolution.type.type : undefined,
    syntax: node,
  })
}

export const analyzeConstantReference = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  resolution: ResolutionContext,
): ExpressionResult | undefined => {
  const identifiers = SyntaxTree.tokens(node).filter((token) => token.kind === 'Identifier')
  const first = identifiers.at(0)
  const second = identifiers.at(1)
  if (first === undefined || identifiers.length > 2) return undefined
  const lookup =
    second === undefined
      ? NameResolution.lookup(resolution.scope, resolution.index, spelling(source, first))
      : NameResolution.lookupQualified(
          resolution.scope,
          resolution.index,
          spelling(source, first),
          spelling(source, second),
          second,
        )
  return lookup._tag === 'Resolved' && lookup.declaration._tag === 'ConstantDeclaration'
    ? analyzeConstant(lookup.declaration, second ?? first, node, false)
    : undefined
}

export interface MoveResult {
  readonly fact: MoveExpressionFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly type: SemanticType | undefined
}

export const analyzeMove = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): MoveResult => {
  const subjectNode = node.children.find(isExpressionNode)
  const subject =
    subjectNode === undefined
      ? undefined
      : analyzeExpression(source, subjectNode, declarations, declaration, scope, resolution)
  if (subject === undefined) throw new RangeError('Move expression requires a subject expression')
  const invalidMove =
    subject.fact._tag === 'Constant'
      ? Diagnostic.invalidConstant('constants are immediate values and cannot be moved', node.span)
      : undefined
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Move',
      subject: subject.fact,
      type: subject.fact.type,
      syntax: node,
    }),
    diagnostics: Object.freeze(
      invalidMove === undefined ? subject.diagnostics : [...subject.diagnostics, invalidMove],
    ),
    type: invalidMove === undefined ? subject.type : undefined,
  })
}

export const borrowRoot = (subject: ExpressionFact): BorrowRootFact | undefined => {
  if (subject._tag === 'Grouped') return borrowRoot(subject.expression)
  if (subject._tag === 'FieldProjection' && subject.state._tag === 'Resolved') {
    const root = borrowRoot(subject.subject)
    return root === undefined
      ? undefined
      : Object.freeze({
          ...root,
          path: Object.freeze([
            ...root.path,
            Object.freeze({
              _tag: 'Field' as const,
              field: subject.state.field.id,
              span: subject.syntax.span,
            }),
          ]),
        })
  }
  if (
    subject._tag === 'IndexProjection' &&
    subject.array !== undefined &&
    (subject.bounds._tag === 'Proven' || subject.bounds._tag === 'Runtime')
  ) {
    const root = borrowRoot(subject.subject)
    return root === undefined
      ? undefined
      : Object.freeze({
          ...root,
          path: Object.freeze([
            ...root.path,
            Object.freeze({
              _tag: 'Index' as const,
              index: subject.index,
              array: subject.array,
              bounds: subject.bounds,
              span: subject.syntax.span,
            }),
          ]),
        })
  }
  if (
    subject._tag === 'IndexProjection' &&
    subject.slice !== undefined &&
    subject.bounds._tag === 'RuntimeSlice'
  ) {
    const root = borrowRoot(subject.subject)
    return root === undefined
      ? undefined
      : Object.freeze({
          ...root,
          path: Object.freeze([
            ...root.path,
            Object.freeze({
              _tag: 'SliceIndex' as const,
              index: subject.index,
              slice: subject.slice,
              span: subject.syntax.span,
            }),
          ]),
        })
  }
  if (subject._tag !== 'Identifier') return undefined
  if (subject.reference._tag === 'ResolvedBinding') {
    return Object.freeze({
      _tag: 'BindingRoot',
      binding: subject.reference.binding,
      path: Object.freeze([]),
    })
  }
  if (subject.reference._tag === 'Resolved') {
    return Object.freeze({
      _tag: 'ParameterRoot',
      parameter: subject.reference.parameter,
      path: Object.freeze([]),
    })
  }
  if (subject.reference._tag === 'ResolvedPattern') {
    return Object.freeze({
      _tag: 'PatternRoot',
      binding: subject.reference.binding,
      path: Object.freeze([]),
    })
  }
  return undefined
}

export const exclusiveBorrowRoot = (root: BorrowRootFact): boolean =>
  root._tag === 'TemporaryRoot' ||
  (root._tag === 'BindingRoot' && root.binding.mutability === 'Mutable') ||
  (root._tag === 'PatternRoot' && root.binding.access === 'Exclusive') ||
  (root._tag === 'ParameterRoot' &&
    root.path.length > 0 &&
    root.parameter.declaredType._tag === 'Resolved' &&
    Type.isReference(root.parameter.declaredType.type) &&
    root.parameter.declaredType.type.access === 'Exclusive')

export const unavailableBorrow = (
  node: SyntaxTree.Node,
  access: Type.Slice['access'],
  subject: ExpressionFact,
  diagnostics: ReadonlyArray<Diagnostic.Diagnostic>,
  cause?: Diagnostic.Diagnostic,
): ExpressionResult =>
  Object.freeze({
    fact: Object.freeze({
      _tag: 'Borrow',
      access,
      subject,
      formation: Object.freeze({
        _tag: 'Unavailable',
        ...(cause === undefined ? {} : { cause: Diagnostic.identity(cause) }),
      }),
      type: unavailableExpressionType,
      syntax: node,
    }),
    diagnostics: Object.freeze([...diagnostics, ...(cause === undefined ? [] : [cause])]),
    type: undefined,
  })

export const analyzeBorrow = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
  expected: SemanticType | undefined,
  borrowAllowed: boolean,
): ExpressionResult => {
  const access: Type.Slice['access'] =
    directToken(node, 'MutKeyword') === undefined ? 'Shared' : 'Exclusive'
  const subjectNode = node.children.find(isExpressionNode)
  const subjectResult =
    subjectNode === undefined
      ? undefined
      : analyzeExpression(source, subjectNode, declarations, declaration, scope, resolution)
  const subject = subjectResult?.fact ?? unavailableExpression(node)
  const diagnostics = subjectResult?.diagnostics ?? Object.freeze([])
  if (
    !borrowAllowed ||
    (expected !== undefined && !Type.isSlice(expected) && !Type.isReference(expected))
  ) {
    return unavailableBorrow(
      node,
      access,
      subject,
      diagnostics,
      Diagnostic.invalidBorrowPosition(node.span),
    )
  }
  const sourceType = subjectResult?.type
  const root =
    borrowRoot(subject) ??
    (sourceType === undefined
      ? undefined
      : Object.freeze({
          _tag: 'TemporaryRoot' as const,
          owner: Object.freeze({
            _tag: 'TemporaryOwnerId' as const,
            function: declaration.id,
            span: subject.syntax.span,
            ordinal: 0,
          }),
          value: subject,
          path: Object.freeze([]),
        }))
  if (root === undefined || sourceType === undefined) {
    return unavailableBorrow(
      node,
      access,
      subject,
      diagnostics,
      Diagnostic.invalidBorrowOperand(subjectNode?.span ?? node.span),
    )
  }
  if (expected === undefined && !Type.isFixedArray(sourceType) && !Type.isSlice(sourceType)) {
    if (access === 'Exclusive' && !exclusiveBorrowRoot(root)) {
      const name =
        subject._tag === 'Identifier' && 'spelling' in subject.reference
          ? subject.reference.spelling
          : '?'
      return unavailableBorrow(
        node,
        access,
        subject,
        diagnostics,
        Diagnostic.exclusiveBorrowRequiresMutable(name, subjectNode?.span ?? node.span),
      )
    }
    const type = Type.reference(access, sourceType)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Borrow',
        access,
        subject,
        formation: Object.freeze({ _tag: 'ValueBorrow', root, source: sourceType }),
        type: availableExpressionType(type),
        syntax: node,
      }),
      diagnostics,
      type,
    })
  }
  if (expected !== undefined && Type.isReference(expected)) {
    if (!TypeInference.infer(expected.target, sourceType, new Map())) {
      return unavailableBorrow(
        node,
        access,
        subject,
        diagnostics,
        Diagnostic.invalidBorrowOperand(subjectNode?.span ?? node.span),
      )
    }
    if (access === 'Exclusive' && !exclusiveBorrowRoot(root)) {
      const name =
        subject._tag === 'Identifier' && 'spelling' in subject.reference
          ? subject.reference.spelling
          : '?'
      return unavailableBorrow(
        node,
        access,
        subject,
        diagnostics,
        Diagnostic.exclusiveBorrowRequiresMutable(name, subjectNode?.span ?? node.span),
      )
    }
    if (access !== expected.access) {
      return unavailableBorrow(
        node,
        access,
        subject,
        diagnostics,
        Diagnostic.invalidBorrowOperand(subjectNode?.span ?? node.span),
      )
    }
    const type = Type.reference(access, sourceType)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Borrow',
        access,
        subject,
        formation: Object.freeze({ _tag: 'ValueBorrow', root, source: sourceType }),
        type: availableExpressionType(type),
        syntax: node,
      }),
      diagnostics,
      type,
    })
  }
  if (Type.isFixedArray(sourceType)) {
    if (access === 'Exclusive' && !exclusiveBorrowRoot(root)) {
      const name =
        subject._tag === 'Identifier' && 'spelling' in subject.reference
          ? subject.reference.spelling
          : '?'
      return unavailableBorrow(
        node,
        access,
        subject,
        diagnostics,
        Diagnostic.exclusiveBorrowRequiresMutable(name, subjectNode?.span ?? node.span),
      )
    }
    const type = Type.slice(access, sourceType.element)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Borrow',
        access,
        subject,
        formation: Object.freeze({ _tag: 'FixedArrayBorrow', root, array: sourceType }),
        type: availableExpressionType(type),
        syntax: node,
      }),
      diagnostics,
      type,
    })
  }
  if (Type.isSlice(sourceType)) {
    if (sourceType.access === 'Shared' && access === 'Exclusive') {
      return unavailableBorrow(
        node,
        access,
        subject,
        diagnostics,
        Diagnostic.invalidSliceReborrow(sourceType.access, access, node.span),
      )
    }
    const type = Type.slice(access, sourceType.element)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Borrow',
        access,
        subject,
        formation: Object.freeze({
          _tag: 'SliceReborrow',
          root,
          parent: sourceType,
          suspendsParent: sourceType.access === 'Exclusive',
        }),
        type: availableExpressionType(type),
        syntax: node,
      }),
      diagnostics,
      type,
    })
  }
  return unavailableBorrow(
    node,
    access,
    subject,
    diagnostics,
    Diagnostic.invalidBorrowOperand(subjectNode?.span ?? node.span),
  )
}

export interface StructTargetResult {
  readonly fact: StructTargetFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

export const intrinsicStruct = (
  type: Type.Nominal,
  syntax: SyntaxTree.Node,
  token: Token.Token,
): DeclarationFacts.StructFact => {
  const ordinal = Type.intrinsicNominalOrdinal(type)
  const id: DeclarationFacts.DeclarationId = Object.freeze({
    _tag: 'DeclarationId',
    sourceId: type.module,
    ordinal,
  })
  const fieldTypes: ReadonlyArray<readonly [string, Type.Type]> = Type.equals(type, Type.layout)
    ? Object.freeze([
        Object.freeze(['bytes', 'usize'] as const),
        Object.freeze(['alignment', 'usize'] as const),
      ])
    : Type.equals(type, Type.invalidAlignment)
      ? Object.freeze([Object.freeze(['alignment', 'usize'] as const)])
      : Object.freeze([])
  return Object.freeze({
    _tag: 'StructDeclaration',
    id,
    canonical: Object.freeze({
      _tag: 'Canonical',
      id: Object.freeze({
        _tag: 'CanonicalDeclarationId',
        module: type.module,
        name: type.name,
      }),
    }),
    visibility: 'Public',
    typeParameters: Object.freeze([]),
    name: Object.freeze({ _tag: 'Present', spelling: type.name, token }),
    fields: Object.freeze(
      fieldTypes.map(([name, fieldType], fieldOrdinal) =>
        Object.freeze({
          _tag: 'StructField' as const,
          id: Object.freeze({ _tag: 'FieldId' as const, struct: id, ordinal: fieldOrdinal }),
          state: Object.freeze({
            _tag: 'Unique' as const,
            id: Object.freeze({ _tag: 'FieldId' as const, struct: id, ordinal: fieldOrdinal }),
          }),
          visibility: 'Public' as const,
          name: Object.freeze({ _tag: 'Present' as const, spelling: name, token }),
          declaredType: Object.freeze({
            _tag: 'Resolved' as const,
            type: fieldType,
            spelling: Type.encode(fieldType),
            token,
            syntax,
          }),
          syntax,
        }),
      ),
    ),
    dependency: Object.freeze({ _tag: 'Available', types: Object.freeze([]) }),
    syntax,
  })
}

export const resolveStructTarget = (
  source: SourceFile.SourceFile,
  syntax: SyntaxTree.Node,
  resolution: ResolutionContext,
  caller?: DeclarationFact,
  inferConstructionArguments = false,
): StructTargetResult => {
  const environment = new Map(
    (caller?.typeParameters ?? []).flatMap((parameter) =>
      parameter.name._tag === 'Present' ? [[parameter.name.spelling, parameter.type] as const] : [],
    ),
  )
  const analyzed = DeclarationCollection.analyzeDeclaredType(source, syntax, environment)
  const nameResolution: NameResolution.Resolution = Object.freeze({
    _tag: 'NameResolution',
    modules: Object.freeze([resolution.scope]),
    diagnostics: Object.freeze([]),
  })
  if (inferConstructionArguments) {
    const applied = analyzed.fact._tag === 'Applied' ? analyzed.fact : undefined
    const targetFact = applied?.target ?? analyzed.fact
    const path = targetFact._tag === 'Unresolved' ? targetFact.path : undefined
    const base =
      path === undefined
        ? targetFact._tag === 'Resolved' && Type.isNominal(targetFact.type)
          ? targetFact.type
          : undefined
        : (() => {
            const candidate = NameResolution.resolveType(
              nameResolution,
              resolution.index,
              source.id,
              path,
            ).fact
            return candidate._tag === 'Resolved' && Type.isNominal(candidate.type)
              ? candidate.type
              : undefined
          })()
    const candidate =
      base === undefined
        ? undefined
        : DeclarationFacts.byCanonical(resolution.index, {
            _tag: 'CanonicalDeclarationId',
            module: base.module,
            name: base.name,
          })
    if (base !== undefined && candidate?._tag === 'StructDeclaration') {
      const supplied = applied?.arguments ?? []
      const sourceParameters = candidate.typeParameters.filter(
        (parameter) =>
          parameter.type.kind !== 'CallableRepresentation' &&
          parameter.type.kind !== 'EffectRepresentation',
      )
      if (supplied.length <= sourceParameters.length) {
        const resolvedArguments = supplied.map((argument) =>
          DeclarationResolution.resolveTypeFact(
            resolution.index,
            source.id,
            argument,
            (module, argumentPath) =>
              NameResolution.resolveType(nameResolution, resolution.index, module, argumentPath),
          ),
        )
        let suppliedOrdinal = 0
        const arguments_ = candidate.typeParameters.flatMap(
          (parameter): ReadonlyArray<Type.GenericArgument> => {
            if (
              parameter.type.kind === 'CallableRepresentation' ||
              parameter.type.kind === 'EffectRepresentation'
            )
              return [Type.representationParameterArgument(parameter.type)]
            const resolved = resolvedArguments.at(suppliedOrdinal)
            suppliedOrdinal += 1
            if (resolved === undefined) return [Type.parameterArgument(parameter.type)]
            if (resolved?.fact._tag !== 'Resolved') return []
            if (parameter.type.kind === 'Value')
              return Type.isTypeArgument(resolved.fact.type) ? [resolved.fact.type] : []
            if (
              parameter.type.kind === 'RequirementRow' &&
              Type.isParameter(resolved.fact.type) &&
              resolved.fact.type.kind === 'RequirementRow'
            )
              return [Type.requirementRowArgument([], [resolved.fact.type])]
            return []
          },
        )
        if (arguments_.length === candidate.typeParameters.length) {
          const parameters = candidate.typeParameters.map((parameter) => parameter.type)
          if (TypeInference.prefixSubstitution(parameters, arguments_) !== undefined) {
            const token = SyntaxTree.tokens(syntax).find(
              (candidateToken) => candidateToken.kind === 'Identifier',
            )
            if (token !== undefined)
              return Object.freeze({
                fact: Object.freeze({
                  _tag: 'Resolved',
                  struct: candidate,
                  type: Type.specializeNominal(base, arguments_),
                  token,
                }),
                diagnostics: Diagnostic.merge(
                  analyzed.diagnostics,
                  ...resolvedArguments.map((argument) => argument.diagnostics),
                ),
              })
          }
        }
      }
    }
  }
  const resolved = DeclarationResolution.resolveTypeFact(
    resolution.index,
    source.id,
    analyzed.fact,
    (module, path) => NameResolution.resolveType(nameResolution, resolution.index, module, path),
  )
  if (resolved.fact._tag === 'Resolved' && Type.isNominal(resolved.fact.type)) {
    if (Type.isIntrinsicNominal(resolved.fact.type) && !Type.isSharedCore(resolved.fact.type)) {
      const token = SyntaxTree.tokens(syntax).find((candidate) => candidate.kind === 'Identifier')
      if (token !== undefined)
        return Object.freeze({
          fact: Object.freeze({
            _tag: 'Resolved',
            struct: intrinsicStruct(resolved.fact.type, syntax, token),
            type: resolved.fact.type,
            token,
          }),
          diagnostics: Diagnostic.merge(analyzed.diagnostics, resolved.diagnostics),
        })
    }
    const declaration = DeclarationFacts.byCanonical(resolution.index, {
      _tag: 'CanonicalDeclarationId',
      module: resolved.fact.type.module,
      name: resolved.fact.type.name,
    })
    if (declaration?._tag === 'StructDeclaration') {
      const token = SyntaxTree.tokens(syntax).find((candidate) => candidate.kind === 'Identifier')
      if (token === undefined)
        return Object.freeze({
          fact: Object.freeze({ _tag: 'Unavailable' }),
          diagnostics: Diagnostic.merge(analyzed.diagnostics, resolved.diagnostics),
        })
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Resolved',
          struct: declaration,
          type: resolved.fact.type,
          token,
        }),
        diagnostics: Diagnostic.merge(analyzed.diagnostics, resolved.diagnostics),
      })
    }
  }
  const token = SyntaxTree.tokens(syntax).find((candidate) => candidate.kind === 'Identifier')
  const diagnostic = Diagnostic.expectedType(
    resolved.fact._tag === 'Resolved' ? Type.encode(resolved.fact.type) : 'unavailable struct',
    token?.span ?? syntax.span,
  )
  return Object.freeze({
    fact: Object.freeze({ _tag: 'Unavailable', cause: Diagnostic.identity(diagnostic) }),
    diagnostics: Diagnostic.merge(analyzed.diagnostics, resolved.diagnostics, [diagnostic]),
  })
}

export interface PatternCounters {
  pattern: number
  binding: number
  invalid: boolean
}

export interface PatternResult {
  readonly fact: PatternFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

export interface PatternTypeResult {
  readonly type?: Type.Type
  readonly declared: DeclarationFacts.DeclaredTypeFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

export const resolvePatternType = (
  source: SourceFile.SourceFile,
  syntax: SyntaxTree.Node,
  resolution: ResolutionContext,
  declaration: DeclarationFact,
): PatternTypeResult => {
  const environment = new Map(
    declaration.typeParameters.flatMap((parameter) =>
      parameter.name._tag === 'Present' ? [[parameter.name.spelling, parameter.type] as const] : [],
    ),
  )
  const analyzed = DeclarationCollection.analyzeDeclaredType(source, syntax, environment)
  const nameResolution: NameResolution.Resolution = Object.freeze({
    _tag: 'NameResolution',
    modules: Object.freeze([resolution.scope]),
    diagnostics: Object.freeze([]),
  })
  const resolved = DeclarationResolution.resolveTypeFact(
    resolution.index,
    source.id,
    analyzed.fact,
    (module, path) => NameResolution.resolveType(nameResolution, resolution.index, module, path),
  )
  return Object.freeze({
    ...(resolved.fact._tag === 'Resolved' ? { type: resolved.fact.type } : {}),
    declared: resolved.fact,
    diagnostics: Diagnostic.merge(analyzed.diagnostics, resolved.diagnostics),
  })
}

export const patternDeclaredName = (
  source: SourceFile.SourceFile,
  syntax: SyntaxTree.Node,
  token: Token.Token | undefined,
): DeclaredName =>
  token === undefined
    ? Object.freeze({ _tag: 'Unavailable', syntax })
    : Object.freeze({ _tag: 'Present', spelling: spelling(source, token), token })

export const analyzePattern = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  arm: Match.ArmId,
  access: Match.Access,
  scope: Scope,
  resolution: ResolutionContext,
  declaration: DeclarationFact,
  counters: PatternCounters,
  prefix: ReadonlyArray<DeclarationFacts.FieldId> = Object.freeze([]),
  localNames = new Map<string, SourceSpan.SourceSpan>(),
): PatternResult => {
  const id: Match.PatternId = Object.freeze({
    _tag: 'PatternId',
    arm,
    ordinal: counters.pattern,
  })
  counters.pattern += 1
  if (node.kind === 'ErrorPattern') {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'UnavailablePattern',
        id,
        bindings: Object.freeze([]),
        omitted: Object.freeze([]),
        complete: false,
        syntax: node,
      }),
      diagnostics: Object.freeze([]),
    })
  }
  if (node.kind === 'UniversalPattern') {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'UniversalPattern',
        id,
        bindings: Object.freeze([]),
        omitted: Object.freeze(access === 'Move' ? [Object.freeze([])] : []),
        syntax: node,
      }),
      diagnostics: Object.freeze([]),
    })
  }

  if (node.kind === 'BindingPattern') {
    // `Member name` binds the entire member payload: no field destructuring, nothing omitted.
    const bindingTargetSyntax =
      SyntaxTree.directNode(node, 'AppliedType') ?? childNode(node, 'TypePath')
    const bindingTarget = resolvePatternType(source, bindingTargetSyntax, resolution, declaration)
    const bindingDiagnostics: Array<Diagnostic.Diagnostic> = [...bindingTarget.diagnostics]
    const member = bindingTarget.type
    const bindingToken = node.children.find(
      (element): element is Token.Token =>
        SyntaxTree.isToken(element) && element.kind === 'Identifier',
    )
    const declaredName: DeclaredName =
      bindingToken === undefined
        ? Object.freeze({ _tag: 'Unavailable' as const, syntax: node })
        : Object.freeze({
            _tag: 'Present' as const,
            spelling: spelling(source, bindingToken),
            token: bindingToken,
          })
    if (declaredName._tag === 'Present') {
      const original =
        scopeSpanFor(scope, declaredName.spelling) ?? localNames.get(declaredName.spelling)
      if (original === undefined) localNames.set(declaredName.spelling, declaredName.token.span)
      else {
        counters.invalid = true
        bindingDiagnostics.push(
          Diagnostic.patternBindingConflict(
            declaredName.spelling,
            original,
            declaredName.token.span,
          ),
        )
      }
    }
    const wholeBinding: PatternBindingFact = Object.freeze({
      _tag: 'PatternBinding',
      id: Object.freeze({ _tag: 'PatternBindingId' as const, arm, ordinal: counters.binding }),
      name: declaredName,
      path: prefix,
      type: member === undefined ? unavailableExpressionType : availableExpressionType(member),
      access,
      syntax: node,
    })
    counters.binding += 1
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'TypePattern',
        id,
        ...(member === undefined ? {} : { member }),
        declared: bindingTarget.declared,
        bindings: Object.freeze([wholeBinding]),
        omitted: Object.freeze([]),
        complete: member !== undefined && !counters.invalid && isAvailableSyntax(node),
        syntax: node,
      }),
      diagnostics: Object.freeze(bindingDiagnostics),
    })
  }

  const targetSyntax = SyntaxTree.directNode(node, 'AppliedType') ?? childNode(node, 'TypePath')
  const target = resolveStructTarget(source, targetSyntax, resolution, declaration)
  const diagnostics: Array<Diagnostic.Diagnostic> = [...target.diagnostics]
  const struct = target.fact._tag === 'Resolved' ? target.fact.struct : undefined
  const nominal = target.fact._tag === 'Resolved' ? target.fact.type : undefined
  const structSubstitution =
    struct === undefined || nominal === undefined
      ? new Map<string, SemanticType>()
      : (TypeInference.substitution(
          struct.typeParameters.map((parameter) => parameter.type),
          nominal.arguments,
        ) ?? new Map())
  const label = nominal === undefined ? 'unknown struct' : Type.encode(nominal)
  const seen = new Map<string, PatternFieldFact>()
  const bindings: Array<PatternBindingFact> = []
  const fields = SyntaxTree.directNodes(node, 'PatternField').map((fieldNode): PatternFieldFact => {
    const identifiers = fieldNode.children.filter(
      (element): element is Token.Token =>
        SyntaxTree.isToken(element) && element.kind === 'Identifier',
    )
    const nameToken = identifiers.at(0)
    const name = nameToken === undefined ? undefined : spelling(source, nameToken)
    const lookup =
      struct === undefined || name === undefined
        ? undefined
        : DeclarationFacts.lookupField(struct.fields, name)
    let state: PatternFieldState = Object.freeze({ _tag: 'Unavailable' })
    let resolvedField: DeclarationFacts.FieldFact | undefined
    if (lookup?._tag === 'Resolved') {
      const original = seen.get(name ?? '')
      if (original === undefined) {
        resolvedField = lookup.field
        state = Object.freeze({ _tag: 'Resolved', field: lookup.field })
      } else {
        const diagnostic = Diagnostic.duplicatePatternField(
          name ?? '',
          original.syntax.span,
          nameToken?.span ?? fieldNode.span,
        )
        diagnostics.push(diagnostic)
        state = Object.freeze({
          _tag: 'Duplicate',
          field: lookup.field,
          cause: Diagnostic.identity(diagnostic),
        })
      }
    } else if (name !== undefined) {
      const diagnostic = Diagnostic.unknownStructField(
        label,
        name,
        nameToken?.span ?? fieldNode.span,
      )
      diagnostics.push(diagnostic)
      state = Object.freeze({ _tag: 'Unknown', cause: Diagnostic.identity(diagnostic) })
    }

    const nestedNode =
      SyntaxTree.directNode(fieldNode, 'NominalPattern') ??
      SyntaxTree.directNode(fieldNode, 'BindingPattern')
    let nested: PatternFact | undefined
    let binding: PatternBindingFact | undefined
    if (nestedNode !== undefined) {
      const nestedResult = analyzePattern(
        source,
        nestedNode,
        arm,
        access,
        scope,
        resolution,
        declaration,
        counters,
        resolvedField === undefined ? prefix : Object.freeze([...prefix, resolvedField.id]),
        localNames,
      )
      diagnostics.push(...nestedResult.diagnostics)
      nested = nestedResult.fact
      const expected =
        resolvedField?.declaredType._tag === 'Resolved'
          ? Type.substitute(resolvedField.declaredType.type, structSubstitution)
          : undefined
      if (
        expected !== undefined &&
        (nested._tag === 'NominalPattern' || nested._tag === 'TypePattern') &&
        nested.member !== undefined &&
        !Type.equals(expected, nested.member)
      ) {
        counters.invalid = true
        diagnostics.push(
          Diagnostic.matchMemberNotInScrutinee(
            Type.encode(nested.member),
            Type.encode(expected),
            nestedNode.span,
          ),
        )
      }
    } else if (resolvedField !== undefined) {
      const bindingToken = identifiers.at(1) ?? nameToken
      const declaredName = patternDeclaredName(source, fieldNode, bindingToken)
      const bindingType =
        resolvedField.declaredType._tag === 'Resolved'
          ? availableExpressionType(
              Type.substitute(resolvedField.declaredType.type, structSubstitution),
            )
          : unavailableExpressionType
      binding = Object.freeze({
        _tag: 'PatternBinding',
        id: Object.freeze({
          _tag: 'PatternBindingId',
          arm,
          ordinal: counters.binding,
        }),
        name: declaredName,
        field: resolvedField,
        path: Object.freeze([...prefix, resolvedField.id]),
        type: bindingType,
        access,
        syntax: fieldNode,
      })
      counters.binding += 1
      bindings.push(binding)
      if (declaredName._tag === 'Present') {
        const original =
          scopeSpanFor(scope, declaredName.spelling) ?? localNames.get(declaredName.spelling)
        if (original === undefined) localNames.set(declaredName.spelling, declaredName.token.span)
        else {
          counters.invalid = true
          diagnostics.push(
            Diagnostic.patternBindingConflict(
              declaredName.spelling,
              original,
              declaredName.token.span,
            ),
          )
        }
      }
    }
    if (nested?.bindings !== undefined) bindings.push(...nested.bindings)
    const fact: PatternFieldFact = Object.freeze({
      _tag: 'PatternField',
      name,
      ...(nameToken === undefined ? {} : { token: nameToken }),
      state,
      ...(binding === undefined ? {} : { binding }),
      ...(nested === undefined ? {} : { nested }),
      syntax: fieldNode,
    })
    if (name !== undefined && !seen.has(name)) seen.set(name, fact)
    return fact
  })

  const rest = SyntaxTree.directNode(node, 'RestPattern') !== undefined
  const omitted: Array<ReadonlyArray<DeclarationFacts.FieldId>> = fields.flatMap(
    (field) => field.nested?.omitted ?? [],
  )
  if (struct !== undefined && !rest) {
    for (const field of struct.fields) {
      if (field.name._tag !== 'Present' || seen.has(field.name.spelling)) continue
      diagnostics.push(Diagnostic.missingPatternField(label, field.name.spelling, node.span))
    }
  } else if (struct !== undefined && rest) {
    for (const field of struct.fields) {
      if (field.name._tag === 'Present' && seen.has(field.name.spelling)) continue
      omitted.push(Object.freeze([...prefix, field.id]))
    }
  }
  const complete =
    target.fact._tag === 'Resolved' &&
    !counters.invalid &&
    isAvailableSyntax(node) &&
    fields.every(
      (field) =>
        field.state._tag === 'Resolved' &&
        (field.nested === undefined ||
          ((field.nested._tag === 'NominalPattern' || field.nested._tag === 'TypePattern') &&
            field.nested.complete)),
    ) &&
    (rest ||
      struct?.fields.every(
        (field) => field.name._tag !== 'Present' || seen.has(field.name.spelling),
      ) === true)
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'NominalPattern',
      id,
      target: target.fact,
      ...(nominal === undefined ? {} : { member: nominal }),
      fields: Object.freeze(fields),
      bindings: Object.freeze(bindings),
      omitted: Object.freeze(omitted),
      rest,
      complete,
      syntax: node,
    }),
    diagnostics: Object.freeze(diagnostics),
  })
}

export const unavailableExpression = (syntax: SyntaxTree.Node): ExpressionFact =>
  Object.freeze({
    _tag: 'Identifier',
    reference: Object.freeze({ _tag: 'Unavailable', syntax }),
    type: unavailableExpressionType,
    syntax,
  })

export const matchAccess = (node: SyntaxTree.Node): Match.Access => {
  const access = SyntaxTree.directNode(node, 'MatchAccess')
  if (access === undefined) return 'Copy'
  if (directToken(access, 'MoveKeyword') !== undefined) return 'Move'
  if (directToken(access, 'Ampersand') === undefined) return 'Copy'
  return directToken(access, 'MutKeyword') === undefined ? 'Shared' : 'Exclusive'
}

export const analyzeMatch = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
  expected?: SemanticType,
  borrowAllowed = false,
): ExpressionResult => {
  const id: Match.MatchId = Object.freeze({
    _tag: 'MatchId',
    function: declaration.id,
    span: node.span,
  })
  const access = matchAccess(node)
  const expressionNodes = node.children.filter(isExpressionNode)
  const scrutineeNode = expressionNodes.at(0)
  const scrutinee =
    scrutineeNode === undefined
      ? undefined
      : analyzeExpression(source, scrutineeNode, declarations, declaration, scope, resolution)
  const diagnostics: Array<Diagnostic.Diagnostic> = [...(scrutinee?.diagnostics ?? [])]
  const members = scrutinee?.type === undefined ? undefined : Match.membersOf(scrutinee.type)

  const preliminary = SyntaxTree.directNodes(node, 'MatchArm').map((armNode, ordinal) => {
    const armId: Match.ArmId = Object.freeze({ _tag: 'MatchArmId', match: id, ordinal })
    const patternNode =
      SyntaxTree.directNode(armNode, 'ErrorPattern') ??
      SyntaxTree.directNode(armNode, 'NominalPattern') ??
      SyntaxTree.directNode(armNode, 'BindingPattern') ??
      SyntaxTree.directNode(armNode, 'UniversalPattern')
    if (patternNode === undefined) throw new RangeError('Match arm requires a pattern')
    const pattern = analyzePattern(
      source,
      patternNode,
      armId,
      access,
      scope,
      resolution,
      declaration,
      {
        pattern: 0,
        binding: 0,
        invalid: false,
      },
    )
    diagnostics.push(...pattern.diagnostics)
    return Object.freeze({ armNode, armId, pattern: pattern.fact })
  })
  const coverage = Match.cover(
    members ?? Object.freeze([]),
    preliminary.map(({ armNode, pattern }) =>
      Object.freeze({
        ...((pattern._tag === 'NominalPattern' || pattern._tag === 'TypePattern') &&
        pattern.member !== undefined
          ? { member: pattern.member }
          : {}),
        universal: pattern._tag === 'UniversalPattern',
        guarded: directToken(armNode, 'IfKeyword') !== undefined,
      }),
    ),
  )
  const arms = preliminary.map(({ armNode, armId, pattern }, ordinal): MatchArmFact => {
    const transition = coverage.transitions.at(ordinal)
    if (transition === undefined) throw new RangeError('Match coverage lost an arm')
    if (
      (pattern._tag === 'NominalPattern' || pattern._tag === 'TypePattern') &&
      pattern.member !== undefined &&
      members !== undefined &&
      !members.some((member) => Type.equals(member, pattern.member ?? member))
    ) {
      diagnostics.push(
        Diagnostic.matchMemberNotInScrutinee(
          Type.encode(pattern.member),
          scrutinee?.type === undefined ? 'unknown' : Type.encode(scrutinee.type),
          pattern.syntax.span,
        ),
      )
    } else if (
      pattern._tag !== 'UnavailablePattern' &&
      !transition.reachable &&
      (members?.length ?? 0) > 0
    ) {
      diagnostics.push(
        Diagnostic.unreachableMatchArm(
          pattern._tag === 'UniversalPattern'
            ? '_'
            : pattern.member === undefined
              ? 'unknown'
              : Type.encode(pattern.member),
          armNode.span,
        ),
      )
    }
    const armExpressions = armNode.children.filter(isExpressionNode)
    const guarded = directToken(armNode, 'IfKeyword') !== undefined
    const guardNode = guarded ? armExpressions.at(0) : undefined
    const resultNode = armExpressions.at(guarded ? 1 : 0)
    const armScope: Scope = Object.freeze({
      parameters: scope.parameters,
      bindings: scope.bindings,
      patternBindings: Object.freeze([...scope.patternBindings, ...pattern.bindings]),
    })
    const guard =
      guardNode === undefined
        ? undefined
        : analyzeExpression(source, guardNode, declarations, declaration, armScope, resolution)
    if (guard !== undefined) {
      diagnostics.push(...guard.diagnostics)
      if (guard.type !== undefined && guard.type !== 'bool') {
        diagnostics.push(
          Diagnostic.matchGuardNotBool(Type.encode(guard.type), guardNode?.span ?? armNode.span),
        )
      }
    }
    const result =
      resultNode === undefined
        ? undefined
        : analyzeExpression(
            source,
            resultNode,
            declarations,
            declaration,
            armScope,
            resolution,
            expected,
            borrowAllowed,
          )
    if (result !== undefined) diagnostics.push(...result.diagnostics)
    return Object.freeze({
      _tag: 'MatchArm',
      id: armId,
      pattern,
      bindings: pattern.bindings,
      ...(guard === undefined ? {} : { guard: guard.fact }),
      result: result?.fact ?? unavailableExpression(resultNode ?? armNode),
      before: transition.before,
      after: transition.after,
      reachable: transition.reachable,
      syntax: armNode,
    })
  })
  if (
    members !== undefined &&
    !coverage.exhaustive &&
    !preliminary.some(({ pattern }) => pattern._tag === 'UnavailablePattern')
  ) {
    diagnostics.push(Diagnostic.incompleteMatch(coverage.missing.map(Type.encode), node.span))
  }
  const reachableTypes = arms.flatMap((arm) =>
    arm.reachable && arm.result.type._tag === 'Available' ? [arm.result.type.type] : [],
  )
  const unavailableReachableResult = arms.some(
    (arm) => arm.reachable && arm.result.type._tag !== 'Available',
  )
  const joined = Match.join(reachableTypes)
  if (joined._tag === 'Incompatible') {
    let divergentRepresentations:
      | {
          readonly divergence: Type.RepresentationDivergence
          readonly spans: readonly [SourceSpan.SourceSpan, SourceSpan.SourceSpan]
        }
      | undefined
    const reachableResults = arms.flatMap((arm) =>
      arm.reachable && arm.result.type._tag === 'Available'
        ? [Object.freeze({ type: arm.result.type.type, span: arm.result.syntax.span })]
        : [],
    )
    for (const [leftOrdinal, left] of reachableResults.entries()) {
      for (const right of reachableResults.slice(leftOrdinal + 1)) {
        const divergence = Type.firstRepresentationDivergence(left.type, right.type)
        if (divergence !== undefined) {
          divergentRepresentations = Object.freeze({
            divergence,
            spans: Object.freeze([left.span, right.span] as const),
          })
          break
        }
      }
      if (divergentRepresentations !== undefined) break
    }
    diagnostics.push(
      divergentRepresentations === undefined
        ? Diagnostic.incompatibleMatchResults(joined.types.map(Type.encode), node.span)
        : Diagnostic.divergentRepresentationJoin(
            Type.encodeGenericArgument(divergentRepresentations.divergence.left),
            Type.encodeGenericArgument(divergentRepresentations.divergence.right),
            divergentRepresentations.spans,
            node.span,
          ),
    )
  }
  const hasInvalidGuard = arms.some(
    (arm) =>
      arm.guard !== undefined &&
      arm.guard.type._tag === 'Available' &&
      arm.guard.type.type !== 'bool',
  )
  const joinedEffect =
    joined._tag === 'Joined'
      ? Type.isRepresented(joined.type)
        ? Type.isEffect(joined.type.contract)
          ? joined.type.contract
          : undefined
        : Type.isEffect(joined.type)
          ? joined.type
          : undefined
      : undefined
  const effectAlternatives =
    joinedEffect === undefined
      ? Object.freeze([])
      : Object.freeze(
          arms.flatMap((arm) => {
            if (!arm.reachable) return []
            const representation = representationOfExpression(arm.result)
            return representation !== undefined &&
              Type.isExactRepresentationArgument(representation) &&
              Type.isEffectIdentityArgument(representation.identity) &&
              Type.isEffect(representation.contract)
              ? [representation]
              : []
          }),
        )
  const reachableEffectArms =
    joinedEffect === undefined ? 0 : arms.filter((arm) => arm.reachable).length
  const unavailableEffectComposite =
    joinedEffect !== undefined && effectAlternatives.length !== reachableEffectArms
  if (unavailableEffectComposite)
    diagnostics.push(
      Diagnostic.nonFiniteEffectJoin(
        'every reachable alternative must retain one exact static Effect representation',
        node.span,
      ),
    )
  const callableSites = arms.flatMap((arm) =>
    arm.reachable && arm.result._tag === 'CallableSection'
      ? [Hir.executableSiteKey(arm.result.site)]
      : [],
  )
  const erasesCallableIdentity = new Set(callableSites).size > 1
  if (erasesCallableIdentity) diagnostics.push(Diagnostic.callableIdentityErasure(node.span))
  const type =
    members !== undefined &&
    coverage.exhaustive &&
    arms.every(
      (arm) =>
        arm.reachable &&
        (arm.pattern._tag === 'UniversalPattern' ||
          ((arm.pattern._tag === 'NominalPattern' || arm.pattern._tag === 'TypePattern') &&
            arm.pattern.complete)),
    ) &&
    !unavailableReachableResult &&
    !hasInvalidGuard &&
    !unavailableEffectComposite &&
    !erasesCallableIdentity &&
    joined._tag === 'Joined'
      ? availableExpressionType(
          joinedEffect === undefined
            ? joined.type
            : Type.represented(
                joinedEffect,
                joinedEffect,
                effectAlternatives.length === 1
                  ? (effectAlternatives.at(0) ??
                      Type.compositeEffectRepresentationArgument(joinedEffect, effectAlternatives))
                  : Type.compositeEffectRepresentationArgument(joinedEffect, effectAlternatives),
              ),
        )
      : unavailableExpressionType
  const fact: MatchExpressionFact = Object.freeze({
    _tag: 'Match',
    id,
    access,
    scrutinee: scrutinee?.fact ?? unavailableExpression(scrutineeNode ?? node),
    members: Object.freeze([...(members ?? [])]),
    arms: Object.freeze(arms),
    exhaustive: coverage.exhaustive,
    type,
    syntax: node,
  })
  return Object.freeze({
    fact,
    diagnostics: Object.freeze(diagnostics),
    type: type._tag === 'Available' ? type.type : undefined,
  })
}

export const callableRepresentationTarget = (
  reference: CallReferenceFact,
): Type.CallableIdentityArgument['target'] | undefined => {
  if (reference._tag === 'Resolved') {
    const canonical = reference.declaration.canonical
    return canonical._tag === 'Canonical'
      ? Object.freeze({
          _tag: 'Declaration',
          module: canonical.id.module,
          name: canonical.id.name,
        })
      : undefined
  }
  return reference._tag === 'ResolvedBuiltin'
    ? Object.freeze({
        _tag: 'Builtin',
        actor: reference.actor,
        operation: reference.operation,
        intrinsic: Object.freeze({
          actor: reference.intrinsic.actor,
          name: reference.intrinsic.name,
        }),
      })
    : undefined
}

export const exactCallableRepresentation = (
  reference: CallReferenceFact,
  contract: Type.Callable,
  typeArguments: ReadonlyArray<Type.GenericArgument> = Object.freeze([]),
  environment?: Type.CallableEnvironmentIdentity,
): Type.ExactRepresentationArgument | undefined => {
  const target = callableRepresentationTarget(reference)
  if (target === undefined) return undefined
  const identity =
    target._tag === 'Declaration'
      ? `declaration:${target.module}:${target.name}`
      : `builtin:${target.actor}:${target.operation}`
  return Type.exactRepresentationArgument(
    Type.callableIdentityArgument(identity, target, typeArguments, environment),
    contract,
  )
}

export const exactEffectDeclarationRepresentation = (
  declaration: DeclarationFact,
  contract: Type.Effect,
  typeArguments: ReadonlyArray<Type.GenericArgument>,
): Type.ExactRepresentationArgument | undefined => {
  if (declaration.functionKind !== 'Effect' || declaration.canonical._tag !== 'Canonical')
    return undefined
  const owner = Object.freeze({
    declaration: Object.freeze({
      module: declaration.canonical.id.module,
      name: declaration.canonical.id.name,
    }),
    typeArguments,
  })
  const site: Hir.EffectSiteId = Object.freeze({
    _tag: 'EffectSiteId',
    function: declaration.id,
    owner: declaration.canonical.id,
    ordinal: -1,
    span: declaration.syntax.span,
  })
  return Type.exactRepresentationArgument(
    Type.effectIdentityArgument(Hir.effectRepresentationIdentity(site), owner),
    contract,
  )
}

export const exactEffectIdentityOfExpression = (
  expression: ExpressionFact,
): Type.EffectIdentityArgument | undefined => {
  const representation = representationOfExpression(expression)
  return representation?._tag === 'ExactRepresentationArgument' &&
    Type.isEffectIdentityArgument(representation.identity)
    ? representation.identity
    : undefined
}

export const hiddenEffectArguments = (
  declaration: DeclarationFact,
  substitution: Type.Substitution,
  argumentAt: (ordinal: number) => ExpressionFact | undefined,
): ReadonlyArray<Type.EffectIdentityArgument> =>
  Object.freeze(
    declaration.parameters.flatMap((parameter, ordinal) => {
      const declared = parameter.declaredType
      if (declared._tag !== 'Resolved') return []
      const specialized = Type.substitute(declared.type, substitution)
      const contract = Type.isRepresented(specialized) ? specialized.contract : specialized
      if (!Type.isEffect(contract)) return []
      const argument = argumentAt(ordinal)
      const identity =
        argument === undefined ? undefined : exactEffectIdentityOfExpression(argument)
      return identity === undefined ? [] : [identity]
    }),
  )

export const exactEffectApplicationContract = (
  _declaration: DeclarationFact,
  _substitution: Type.Substitution,
  contract: Type.Effect,
): Type.Effect => contract

export const effectCallableApplicationRepresentation = (
  expression: CallableApplyExpressionFact,
  contract: Type.Effect,
): Type.ExactRepresentationArgument | undefined => {
  const callee = expression.callee
  if (callee._tag !== 'CallableSection' || callee.reference._tag !== 'Resolved') return undefined
  const substitution = new Map(callee.substitution)
  for (const [parameter, argument] of expression.substitution) substitution.set(parameter, argument)
  const declaredArguments = Object.freeze(
    callee.reference.declaration.typeParameters.map(
      (parameter) =>
        substitution.get(Type.key(parameter.type)) ??
        Type.substituteGenericArgument(parameter.type, substitution),
    ),
  )
  const applicationArgument = (ordinal: number): ExpressionFact | undefined => {
    const captured = callee.captures.find((capture) => capture.parameterOrdinal === ordinal)
    if (captured !== undefined) return captured.expression
    const argumentOrdinal = callee.remainingParameters.indexOf(ordinal)
    return argumentOrdinal < 0 ? undefined : expression.arguments.at(argumentOrdinal)?.expression
  }
  return exactEffectDeclarationRepresentation(
    callee.reference.declaration,
    exactEffectApplicationContract(callee.reference.declaration, substitution, contract),
    Object.freeze([
      ...declaredArguments,
      ...hiddenEffectArguments(callee.reference.declaration, substitution, applicationArgument),
    ]),
  )
}

/**
 * Recovers a compile-time representation from semantic expression structure. This is deliberately
 * frontend-owned: later phases consume the retained argument and never reconstruct it from syntax.
 */
export function representationOfExpression(
  expression: ExpressionFact,
): Type.RepresentationArgument | undefined {
  if (expression.type._tag === 'Available' && Type.isRepresented(expression.type.type))
    return expression.type.type.representation.argument
  if (expression._tag === 'FunctionItem' && expression.type._tag === 'Available') {
    const contract = expression.type.type
    return Type.isCallable(contract)
      ? exactCallableRepresentation(expression.reference, contract)
      : undefined
  }
  if (expression._tag === 'CallableSection' && expression.type._tag === 'Available') {
    const contract = expression.type.type
    if (!Type.isCallable(contract)) return undefined
    if (expression.environmentOwner === undefined) return undefined
    const environment = Hir.callableEnvironmentIdentity(
      expression.site,
      expression.environmentOwner,
    )
    return exactCallableRepresentation(
      expression.reference,
      contract,
      expression.typeArguments,
      environment,
    )
  }
  if (expression._tag === 'EffectBlock' && expression.type._tag === 'Available') {
    const contract = expression.type.type
    if (!Type.isEffect(contract)) return undefined
    const site = expression.site
    return Type.exactRepresentationArgument(
      Type.effectIdentityArgument(
        Hir.effectRepresentationIdentity(site),
        expression.representationOwner,
      ),
      contract,
    )
  }
  if (
    expression._tag === 'Call' &&
    expression.reference._tag === 'Resolved' &&
    expression.contract._tag === 'Compatible' &&
    expression.type._tag === 'Available' &&
    Type.isEffect(expression.type.type)
  ) {
    return exactEffectDeclarationRepresentation(
      expression.reference.declaration,
      expression.type.type,
      Object.freeze([
        ...expression.contract.typeArguments,
        ...hiddenEffectArguments(
          expression.reference.declaration,
          expression.contract.substitution,
          (ordinal) => expression.arguments.at(ordinal)?.expression,
        ),
      ]),
    )
  }
  if (
    expression._tag === 'CallableApply' &&
    expression.type._tag === 'Available' &&
    Type.isEffect(expression.type.type)
  ) {
    return effectCallableApplicationRepresentation(expression, expression.type.type)
  }
  if (expression._tag === 'Identifier' && expression.reference._tag === 'ResolvedBinding')
    return representationOfExpression(expression.reference.binding.initializer)
  if (expression._tag === 'Move') return representationOfExpression(expression.subject)
  if (expression._tag === 'Grouped') return representationOfExpression(expression.expression)
  return undefined
}

export interface InferredStructArgument {
  readonly argument: Type.GenericArgument
  readonly span: SourceSpan.SourceSpan
}

export const isOwnStructArgument = (
  parameter: Type.Parameter,
  argument: Type.GenericArgument,
): boolean => Type.equalsGenericArgument(Type.parameterArgument(parameter), argument)

export const analyzeStructLiteral = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult => {
  const targetSyntax = SyntaxTree.directNode(node, 'AppliedType') ?? childNode(node, 'TypePath')
  const target = resolveStructTarget(source, targetSyntax, resolution, declaration, true)
  const diagnostics: Array<Diagnostic.Diagnostic> = [...target.diagnostics]
  const struct = target.fact._tag === 'Resolved' ? target.fact.struct : undefined
  const nominal = target.fact._tag === 'Resolved' ? target.fact.type : undefined
  const nominalLabel = nominal === undefined ? 'unknown struct' : Type.encode(nominal)
  const inferredArguments = new Map<string, InferredStructArgument>()
  const argumentOrigins = new Map<string, ReadonlyArray<SourceSpan.SourceSpan>>()
  const explicitArguments = new Set<string>()
  if (struct !== undefined && nominal !== undefined) {
    for (const [ordinal, parameter] of struct.typeParameters.entries()) {
      const argument = nominal.arguments.at(ordinal)
      if (argument === undefined || isOwnStructArgument(parameter.type, argument)) continue
      const parameterKey = Type.key(parameter.type)
      inferredArguments.set(parameterKey, Object.freeze({ argument, span: targetSyntax.span }))
      argumentOrigins.set(parameterKey, Object.freeze([targetSyntax.span]))
      explicitArguments.add(parameterKey)
    }
  }
  const structSubstitution = new Map<string, Type.GenericArgument>()
  for (const [parameterKey, inferred] of inferredArguments)
    structSubstitution.set(parameterKey, inferred.argument)
  const definingModule = nominal?.module
  const authorized =
    definingModule !== undefined &&
    struct?.syntax.kind === 'StructDeclaration' &&
    struct.fields.every((field) => field.visibility === 'Public' || definingModule === source.id)
  const accessDiagnostic =
    nominal !== undefined && !authorized
      ? Diagnostic.inaccessibleStructConstruction(Type.encode(nominal), node.span)
      : undefined
  if (accessDiagnostic !== undefined) diagnostics.push(accessDiagnostic)

  const seen = new Map<string, StructInitializerFact>()
  const initializers = SyntaxTree.directNodes(node, 'StructFieldInitializer').map(
    (initializer): StructInitializerFact => {
      const nameToken = directToken(initializer, 'Identifier')
      const name = nameToken === undefined ? undefined : spelling(source, nameToken)
      const fieldLookup =
        struct === undefined || name === undefined
          ? undefined
          : DeclarationFacts.lookupField(struct.fields, name)
      const expected =
        fieldLookup?._tag === 'Resolved' && fieldLookup.field.declaredType._tag === 'Resolved'
          ? Type.substitute(fieldLookup.field.declaredType.type, structSubstitution)
          : undefined
      const contextualExpected =
        expected !== undefined && Type.isRepresented(expected) ? expected.contract : expected
      const expressionNode = initializer.children.find(isExpressionNode)
      if (expressionNode === undefined) {
        throw new RangeError('Struct initializer requires an expression node')
      }
      const expression = analyzeExpression(
        source,
        expressionNode,
        declarations,
        declaration,
        scope,
        resolution,
        contextualExpected,
      )
      if (expression === undefined) {
        throw new RangeError(`Cannot analyze struct initializer ${expressionNode.kind}`)
      }
      diagnostics.push(...expression.diagnostics)
      let state: StructInitializerState = Object.freeze({ _tag: 'Unavailable' })
      if (name !== undefined && nameToken !== undefined && struct !== undefined) {
        const previous = seen.get(name)
        if (fieldLookup?._tag !== 'Resolved') {
          const diagnostic = Diagnostic.unknownStructField(nominalLabel, name, nameToken.span)
          diagnostics.push(diagnostic)
          state = Object.freeze({ _tag: 'Unknown', cause: Diagnostic.identity(diagnostic) })
        } else if (previous !== undefined) {
          const diagnostic = Diagnostic.duplicateStructInitializer(
            name,
            previous.syntax.span,
            nameToken.span,
          )
          diagnostics.push(diagnostic)
          state = Object.freeze({
            _tag: 'Duplicate',
            field: fieldLookup.field,
            cause: Diagnostic.identity(diagnostic),
          })
        } else if (
          fieldLookup.field.visibility === 'Private' &&
          nominal !== undefined &&
          nominal.module !== source.id &&
          accessDiagnostic !== undefined
        ) {
          state = Object.freeze({
            _tag: 'Inaccessible',
            field: fieldLookup.field,
            cause: Diagnostic.identity(accessDiagnostic),
          })
        } else if (
          fieldLookup.field.declaredType._tag === 'Resolved' &&
          expression.type !== undefined
        ) {
          const expectedType = Type.substitute(
            fieldLookup.field.declaredType.type,
            structSubstitution,
          )
          const expectedValue = Type.isRepresented(expectedType)
            ? expectedType.contract
            : expectedType
          const actualValue = Type.isRepresented(expression.type)
            ? expression.type.contract
            : expression.type
          let representationDiagnostic: Diagnostic.Diagnostic | undefined
          if (Type.isRepresented(expectedType)) {
            const currentSubstitution = new Map(structSubstitution)
            for (const [parameterKey, inferred] of inferredArguments)
              currentSubstitution.set(parameterKey, inferred.argument)
            const candidateSubstitution = new Map(currentSubstitution)
            if (TypeInference.infer(expectedType.contract, actualValue, candidateSubstitution)) {
              const siteSubstitution = new Map<string, Type.GenericArgument>()
              TypeInference.infer(expectedType.contract, actualValue, siteSubstitution)
              for (const parameter of struct.typeParameters) {
                if (
                  parameter.type.kind === 'CallableRepresentation' ||
                  parameter.type.kind === 'EffectRepresentation'
                )
                  continue
                const parameterKey = Type.key(parameter.type)
                const inferred = siteSubstitution.get(parameterKey)
                if (inferred === undefined || isOwnStructArgument(parameter.type, inferred))
                  continue
                if (inferredArguments.get(parameterKey) === undefined)
                  inferredArguments.set(
                    parameterKey,
                    Object.freeze({ argument: inferred, span: expressionNode.span }),
                  )
                argumentOrigins.set(
                  parameterKey,
                  Object.freeze([
                    ...(argumentOrigins.get(parameterKey) ?? []),
                    expressionNode.span,
                  ]),
                )
              }
            }
            const representationSubstitution = new Map(structSubstitution)
            for (const [parameterKey, inferred] of inferredArguments)
              representationSubstitution.set(parameterKey, inferred.argument)
            const specialized = Type.substitute(expectedType, representationSubstitution)
            if (!Type.isRepresented(specialized))
              throw new RangeError('represented struct field lost its representation contract')
            const specializedExpectedType = specialized
            const actualRepresentation = representationOfExpression(expression.fact)
            const requiredArgument = expectedType.representation.argument
            if (actualRepresentation === undefined) {
              representationDiagnostic = Diagnostic.structFieldTypeMismatch(
                name,
                Type.encode(specializedExpectedType),
                Type.encode(expression.type),
                expressionNode.span,
              )
            } else if (requiredArgument._tag === 'RepresentationParameterArgument') {
              const parameter = requiredArgument.parameter
              const parameterKey = Type.key(parameter)
              const previousRepresentation = inferredArguments.get(parameterKey)
              if (
                previousRepresentation !== undefined &&
                !Type.equalsGenericArgument(previousRepresentation.argument, actualRepresentation)
              ) {
                representationDiagnostic = Diagnostic.conflictingInitializerRepresentation(
                  parameter.name,
                  Type.encodeGenericArgument(previousRepresentation.argument),
                  Type.encodeGenericArgument(actualRepresentation),
                  previousRepresentation.span,
                  expressionNode.span,
                )
              } else {
                const represented = Type.represented(
                  Type.isCallable(actualValue) || Type.isEffect(actualValue)
                    ? actualValue
                    : specializedExpectedType.contract,
                  specializedExpectedType.representation.requiredBound,
                  actualRepresentation,
                )
                if (represented.representation.admissibility._tag === 'Unavailable') {
                  const requiredParameter = struct.typeParameters.find(
                    (candidate) => Type.key(candidate.type) === Type.key(parameter),
                  )
                  const actualParameter =
                    actualRepresentation._tag === 'RepresentationParameterArgument'
                      ? declaration.typeParameters.find(
                          (candidate) =>
                            Type.key(candidate.type) === Type.key(actualRepresentation.parameter),
                        )
                      : undefined
                  representationDiagnostic = Diagnostic.incompatibleRepresentationBound(
                    parameter.name,
                    Type.encode(specializedExpectedType.representation.requiredBound),
                    Type.encode(represented.contract),
                    expressionNode.span,
                    {
                      ...(requiredParameter === undefined
                        ? {}
                        : { requiredDeclarationSpan: requiredParameter.syntax.span }),
                      ...(actualParameter === undefined
                        ? {}
                        : { actualDeclarationSpan: actualParameter.syntax.span }),
                    },
                  )
                  if (previousRepresentation === undefined)
                    inferredArguments.set(
                      parameterKey,
                      Object.freeze({ argument: actualRepresentation, span: expressionNode.span }),
                    )
                } else if (previousRepresentation === undefined) {
                  inferredArguments.set(
                    parameterKey,
                    Object.freeze({ argument: actualRepresentation, span: expressionNode.span }),
                  )
                }
                if (
                  previousRepresentation === undefined ||
                  Type.equalsGenericArgument(previousRepresentation.argument, actualRepresentation)
                )
                  argumentOrigins.set(
                    parameterKey,
                    Object.freeze([
                      ...(argumentOrigins.get(parameterKey) ?? []),
                      expressionNode.span,
                    ]),
                  )
              }
            } else if (!Type.equalsGenericArgument(requiredArgument, actualRepresentation)) {
              representationDiagnostic = Diagnostic.structFieldTypeMismatch(
                name,
                Type.encodeGenericArgument(requiredArgument),
                Type.encodeGenericArgument(actualRepresentation),
                expressionNode.span,
              )
            }
          } else {
            const currentSubstitution = new Map(structSubstitution)
            for (const [parameterKey, inferred] of inferredArguments)
              currentSubstitution.set(parameterKey, inferred.argument)
            const candidateSubstitution = new Map(currentSubstitution)
            if (!TypeInference.infer(expectedType, expression.type, candidateSubstitution)) {
              const impliedSubstitution = new Map<string, Type.GenericArgument>()
              if (TypeInference.infer(expectedType, expression.type, impliedSubstitution)) {
                for (const parameter of struct.typeParameters) {
                  if (parameter.type.kind !== 'Value') continue
                  const parameterKey = Type.key(parameter.type)
                  const previous = inferredArguments.get(parameterKey)
                  const implied = impliedSubstitution.get(parameterKey)
                  if (
                    previous === undefined ||
                    implied === undefined ||
                    Type.equalsGenericArgument(previous.argument, implied)
                  )
                    continue
                  representationDiagnostic = Diagnostic.typeArgumentConflict(
                    nominalLabel,
                    parameter.type.name,
                    Type.encodeGenericArgument(previous.argument),
                    Type.encodeGenericArgument(implied),
                    expressionNode.span,
                    previous.span,
                  )
                  break
                }
              }
              const specializedExpected = Type.substitute(expectedType, currentSubstitution)
              const divergence = Type.firstRepresentationDivergence(
                specializedExpected,
                expression.type,
              )
              if (representationDiagnostic === undefined && divergence !== undefined) {
                const parameter = struct.typeParameters.find((candidate) => {
                  const inferred = inferredArguments.get(Type.key(candidate.type))
                  return (
                    inferred !== undefined &&
                    Type.equalsGenericArgument(inferred.argument, divergence.left)
                  )
                })
                const original =
                  parameter === undefined
                    ? undefined
                    : inferredArguments.get(Type.key(parameter.type))
                if (parameter !== undefined && original !== undefined)
                  representationDiagnostic = Diagnostic.conflictingInitializerRepresentation(
                    parameter.type.name,
                    Type.encodeGenericArgument(divergence.left),
                    Type.encodeGenericArgument(divergence.right),
                    original.span,
                    expressionNode.span,
                  )
              }
            } else {
              const siteSubstitution = new Map<string, Type.GenericArgument>()
              TypeInference.infer(expectedType, expression.type, siteSubstitution)
              for (const parameter of struct.typeParameters) {
                const parameterKey = Type.key(parameter.type)
                const inferred = siteSubstitution.get(parameterKey)
                if (
                  inferredArguments.get(parameterKey) === undefined &&
                  inferred !== undefined &&
                  !isOwnStructArgument(parameter.type, inferred)
                ) {
                  inferredArguments.set(
                    parameterKey,
                    Object.freeze({ argument: inferred, span: expressionNode.span }),
                  )
                  argumentOrigins.set(parameterKey, Object.freeze([expressionNode.span]))
                } else if (
                  inferred !== undefined &&
                  !isOwnStructArgument(parameter.type, inferred) &&
                  Type.equalsGenericArgument(
                    inferredArguments.get(parameterKey)?.argument ?? inferred,
                    inferred,
                  )
                ) {
                  argumentOrigins.set(
                    parameterKey,
                    Object.freeze([
                      ...(argumentOrigins.get(parameterKey) ?? []),
                      expressionNode.span,
                    ]),
                  )
                }
              }
            }
          }
          const compatibilitySubstitution = new Map(structSubstitution)
          for (const [parameterKey, inferred] of inferredArguments)
            compatibilitySubstitution.set(parameterKey, inferred.argument)
          const compatibleExpected = Type.substitute(expectedValue, compatibilitySubstitution)
          const compatibleValue = typesCompatible(actualValue, compatibleExpected)
          if (representationDiagnostic !== undefined || !compatibleValue) {
            const diagnostic =
              representationDiagnostic ??
              unionConversionDiagnostic(actualValue, compatibleExpected, expressionNode.span) ??
              Diagnostic.structFieldTypeMismatch(
                name,
                Type.encode(compatibleExpected),
                Type.encode(actualValue),
                expressionNode.span,
              )
            diagnostics.push(diagnostic)
            state = Object.freeze({
              _tag: 'TypeMismatch',
              field: fieldLookup.field,
              cause: Diagnostic.identity(diagnostic),
            })
          } else {
            state = Object.freeze({ _tag: 'Resolved', field: fieldLookup.field })
          }
        }
      }
      const fact: StructInitializerFact = Object.freeze({
        _tag: 'StructInitializer',
        name,
        ...(nameToken === undefined ? {} : { token: nameToken }),
        expression: expression.fact,
        state,
        syntax: initializer,
      })
      if (name !== undefined && !seen.has(name)) seen.set(name, fact)
      return fact
    },
  )

  const completedArguments =
    struct === undefined || nominal === undefined
      ? undefined
      : nominal.arguments.map((argument, ordinal): Type.GenericArgument => {
          const parameter = struct.typeParameters.at(ordinal)?.type
          if (parameter === undefined) return argument
          return inferredArguments.get(Type.key(parameter))?.argument ?? argument
        })
  const unresolvedParameters =
    struct === undefined || completedArguments === undefined
      ? []
      : struct.typeParameters.flatMap((parameter, ordinal) => {
          const argument = completedArguments.at(ordinal)
          return argument !== undefined && isOwnStructArgument(parameter.type, argument)
            ? [parameter]
            : []
        })
  for (const parameter of unresolvedParameters) {
    diagnostics.push(
      Diagnostic.uninferredTypeParameter(nominalLabel, parameter.type.name, parameter.syntax.span),
    )
  }
  const completedNominal =
    nominal === undefined ||
    completedArguments === undefined ||
    unresolvedParameters.length > 0 ||
    (struct !== undefined &&
      TypeInference.substitution(
        struct.typeParameters.map((parameter) => parameter.type),
        completedArguments,
      ) === undefined)
      ? undefined
      : Type.nominal(nominal.module, nominal.name, completedArguments)
  const typeArguments: ReadonlyArray<StructTypeArgumentFact> = Object.freeze(
    struct?.typeParameters.map((parameter, ordinal) => {
      const parameterKey = Type.key(parameter.type)
      const argument = completedArguments?.at(ordinal)
      const origins = argumentOrigins.get(parameterKey) ?? Object.freeze([])
      return Object.freeze({
        parameter: parameter.type,
        ...(argument === undefined || isOwnStructArgument(parameter.type, argument)
          ? {}
          : { argument }),
        source:
          argument === undefined || isOwnStructArgument(parameter.type, argument)
            ? ('Unavailable' as const)
            : explicitArguments.has(parameterKey)
              ? ('Explicit' as const)
              : ('Inferred' as const),
        origins,
      })
    }) ?? [],
  )
  const completedTarget: StructTargetFact =
    completedNominal !== undefined && target.fact._tag === 'Resolved'
      ? Object.freeze({ ...target.fact, type: completedNominal })
      : target.fact

  if (struct !== undefined && completedNominal !== undefined) {
    for (const field of struct.fields) {
      if (field.name._tag !== 'Present' || seen.has(field.name.spelling)) continue
      if (field.visibility === 'Private' && completedNominal.module !== source.id) continue
      diagnostics.push(
        Diagnostic.missingStructInitializer(
          Type.encode(completedNominal),
          field.name.spelling,
          node.span,
        ),
      )
    }
  }

  const fields =
    struct === undefined
      ? []
      : struct.fields.flatMap((field) => {
          if (field.name._tag !== 'Present') return []
          const fieldName = field.name.spelling
          const initializer = initializers.find(
            (candidate) => candidate.name === fieldName && candidate.state._tag === 'Resolved',
          )
          return initializer === undefined ? [] : [{ field, initializer }]
        })
  const complete =
    struct !== undefined &&
    completedNominal !== undefined &&
    authorized &&
    SyntaxTree.isAvailableSyntax(node) &&
    fields.length === struct.fields.length &&
    initializers.length === struct.fields.length &&
    initializers.every((initializer) => initializer.state._tag === 'Resolved')
  const type =
    complete && completedNominal !== undefined
      ? availableExpressionType(completedNominal)
      : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'StructLiteral',
      target: completedTarget,
      authorized,
      typeArguments,
      initializers: Object.freeze(initializers),
      fields: Object.freeze(fields),
      type,
      syntax: node,
    }),
    diagnostics: Object.freeze(diagnostics),
    type: complete ? completedNominal : undefined,
  })
}

export const analyzeArrayLiteral = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
  expected?: SemanticType,
): ExpressionResult => {
  const expectedArray = expected !== undefined && Type.isFixedArray(expected) ? expected : undefined
  const elementNodes = node.children.filter(isExpressionNode)
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  let elementType = expectedArray?.element
  let elementOrigin = expectedArray === undefined ? undefined : node.span
  const elements = elementNodes.map((elementNode, ordinal): ArrayElementFact => {
    const element = analyzeExpression(
      source,
      elementNode,
      declarations,
      declaration,
      scope,
      resolution,
      elementType,
    )
    if (element === undefined)
      throw new RangeError(`Cannot analyze array element ${elementNode.kind}`)
    diagnostics.push(...element.diagnostics)
    if (elementType === undefined && element.type !== undefined) {
      elementType = element.type
      elementOrigin = elementNode.span
    }
    let compatibility: ArrayElementFact['compatibility']
    if (element.type === undefined || elementType === undefined) {
      compatibility = Object.freeze({ _tag: 'Unavailable' })
    } else if (!typesCompatible(element.type, elementType)) {
      const diagnostic =
        representationJoinDiagnostic(
          elementType,
          element.type,
          elementOrigin ?? node.span,
          elementNode.span,
          elementNode.span,
        ) ??
        unionConversionDiagnostic(element.type, elementType, elementNode.span) ??
        Diagnostic.arrayElementTypeMismatch(
          Type.encode(elementType),
          Type.encode(element.type),
          ordinal,
          elementNode.span,
        )
      diagnostics.push(diagnostic)
      compatibility = Object.freeze({
        _tag: 'TypeMismatch',
        expected: elementType,
        actual: element.type,
      })
    } else {
      compatibility = Object.freeze({ _tag: 'Compatible' })
    }
    return Object.freeze({
      _tag: 'ArrayElement',
      ordinal,
      expression: element.fact,
      ...(elementType === undefined ? {} : { expected: elementType }),
      compatibility,
      syntax: elementNode,
    })
  })

  const actualLength = elements.length
  let state: ArrayLiteralState
  if (elementType === undefined && actualLength === 0) {
    const diagnostic = Diagnostic.emptyArrayNeedsContext(node.span)
    diagnostics.push(diagnostic)
    state = Object.freeze({ _tag: 'MissingContext' })
  } else if (expectedArray !== undefined && expectedArray.length !== actualLength) {
    const diagnostic = Diagnostic.arrayLengthMismatch(expectedArray.length, actualLength, node.span)
    diagnostics.push(diagnostic)
    state = Object.freeze({
      _tag: 'LengthMismatch',
      expected: expectedArray.length,
      actual: actualLength,
    })
  } else if (elements.some((element) => element.compatibility._tag === 'TypeMismatch')) {
    state = Object.freeze({ _tag: 'IncompatibleElements' })
  } else if (
    elementType === undefined ||
    elements.some((element) => element.compatibility._tag === 'Unavailable') ||
    !SyntaxTree.isAvailableSyntax(node)
  ) {
    state = Object.freeze({ _tag: 'Unavailable' })
  } else {
    state = Object.freeze({
      _tag: 'Complete',
      type: expectedArray ?? Type.fixedArray(elementType, actualLength),
    })
  }
  const type =
    state._tag === 'Complete' ? availableExpressionType(state.type) : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'ArrayLiteral',
      elements: Object.freeze(elements),
      ...(expectedArray === undefined ? {} : { expected: expectedArray }),
      ...(elementType === undefined ? {} : { elementType }),
      length: actualLength,
      state,
      type,
      syntax: node,
    }),
    diagnostics: Object.freeze(diagnostics),
    type: type._tag === 'Available' ? type.type : undefined,
  })
}

export const analyzeIndexProjection = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult => {
  const expressions = node.children.filter(isExpressionNode)
  const subjectNode = expressions.at(0)
  const indexNode = expressions.at(1)
  if (subjectNode === undefined || indexNode === undefined) {
    throw new RangeError('Index projection requires subject and index expressions')
  }
  const subject = analyzeExpression(
    source,
    subjectNode,
    declarations,
    declaration,
    scope,
    resolution,
  )
  const index = analyzeExpression(
    source,
    indexNode,
    declarations,
    declaration,
    scope,
    resolution,
    'usize',
  )
  if (subject === undefined || index === undefined) {
    throw new RangeError('Cannot analyze index projection operands')
  }
  const diagnostics: Array<Diagnostic.Diagnostic> = [...subject.diagnostics, ...index.diagnostics]
  const array =
    subject.type !== undefined && Type.isFixedArray(subject.type) ? subject.type : undefined
  const slice = subject.type !== undefined && Type.isSlice(subject.type) ? subject.type : undefined
  if (subject.type !== undefined && array === undefined && slice === undefined) {
    diagnostics.push(Diagnostic.indexOnNonArray(Type.encode(subject.type), subjectNode.span))
  }
  if (index.type !== undefined && index.type !== 'usize') {
    diagnostics.push(Diagnostic.indexNotUsize(Type.encode(index.type), indexNode.span))
  }
  let bounds: BoundsFact = Object.freeze({ _tag: 'Unavailable' })
  if (array !== undefined && index.type === 'usize') {
    const literal =
      index.fact._tag === 'Integer' && index.fact.integer._tag === 'Available'
        ? index.fact.integer.value <= BigInt(Number.MAX_SAFE_INTEGER)
          ? Number(index.fact.integer.value)
          : Number.POSITIVE_INFINITY
        : undefined
    if (literal === undefined) bounds = Object.freeze({ _tag: 'Runtime', length: array.length })
    else if (literal < 0 || literal >= array.length) {
      const diagnostic = Diagnostic.indexOutOfBounds(literal, array.length, indexNode.span)
      diagnostics.push(diagnostic)
      bounds = Object.freeze({
        _tag: 'Invalid',
        index: literal,
        length: array.length,
        cause: Diagnostic.identity(diagnostic),
      })
    } else bounds = Object.freeze({ _tag: 'Proven', index: literal, length: array.length })
  } else if (slice !== undefined && index.type === 'usize') {
    bounds = Object.freeze({ _tag: 'RuntimeSlice' })
  }
  const available =
    (array !== undefined || slice !== undefined) &&
    index.type === 'usize' &&
    bounds._tag !== 'Invalid' &&
    bounds._tag !== 'Unavailable' &&
    SyntaxTree.isAvailableSyntax(node)
  const element = array?.element ?? slice?.element
  const type =
    available && element !== undefined
      ? availableExpressionType(element)
      : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'IndexProjection',
      subject: subject.fact,
      index: index.fact,
      ...(array === undefined ? {} : { array, elementType: array.element }),
      ...(slice === undefined
        ? {}
        : { slice, elementType: slice.element, borrowAccess: slice.access }),
      access: 'CopyRead',
      bounds,
      type,
      syntax: node,
    }),
    diagnostics: Object.freeze(diagnostics),
    type: type._tag === 'Available' ? type.type : undefined,
  })
}

export const analyzeProjection = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult => {
  const subjectNode = node.children.find(isExpressionNode)
  if (subjectNode === undefined) throw new RangeError('Projection requires a subject expression')
  const subject = analyzeExpression(
    source,
    subjectNode,
    declarations,
    declaration,
    scope,
    resolution,
  )
  if (subject === undefined) throw new RangeError(`Cannot analyze projection ${subjectNode.kind}`)
  const diagnostics: Array<Diagnostic.Diagnostic> = [...subject.diagnostics]
  const fieldToken = directToken(node, 'Identifier')
  const fieldName = fieldToken === undefined ? undefined : spelling(source, fieldToken)
  // A reference projects the fields of its target: the read happens through the borrow, so
  // the projected value is typed by the target while consumption stays a partial-move error.
  const reference =
    subject.type !== undefined &&
    Type.isReference(subject.type) &&
    Type.isNominal(subject.type.target)
      ? subject.type
      : undefined
  const nominal =
    subject.type !== undefined && Type.isNominal(subject.type)
      ? subject.type
      : reference !== undefined && Type.isNominal(reference.target)
        ? reference.target
        : undefined
  const slice = subject.type !== undefined && Type.isSlice(subject.type) ? subject.type : undefined
  const borrowAccess =
    subject.fact._tag === 'IndexProjection' || subject.fact._tag === 'FieldProjection'
      ? subject.fact.borrowAccess
      : undefined
  let state: ProjectionState = Object.freeze({ _tag: 'Unavailable' })
  let type: SemanticType | undefined
  if (slice !== undefined && fieldName === 'length') {
    state = Object.freeze({ _tag: 'SliceLength' })
    type = 'usize'
  } else if (slice !== undefined && fieldName !== undefined && fieldToken !== undefined) {
    const diagnostic = Diagnostic.unknownProjectedField(
      Type.encode(slice),
      fieldName,
      fieldToken.span,
    )
    diagnostics.push(diagnostic)
    state = Object.freeze({ _tag: 'Unavailable', cause: Diagnostic.identity(diagnostic) })
  } else if (subject.type !== undefined && nominal === undefined && fieldToken !== undefined) {
    const diagnostic = Diagnostic.projectionOnNonStruct(Type.encode(subject.type), fieldToken.span)
    diagnostics.push(diagnostic)
    state = Object.freeze({ _tag: 'Unavailable', cause: Diagnostic.identity(diagnostic) })
  } else if (nominal !== undefined && fieldName !== undefined && fieldToken !== undefined) {
    const member = DeclarationFacts.byCanonical(resolution.index, {
      _tag: 'CanonicalDeclarationId',
      module: nominal.module,
      name: nominal.name,
    })
    const struct = member?._tag === 'StructDeclaration' ? member : undefined
    const lookup =
      struct === undefined ? undefined : DeclarationFacts.lookupField(struct.fields, fieldName)
    if (lookup?._tag !== 'Resolved') {
      const diagnostic = Diagnostic.unknownProjectedField(
        Type.encode(nominal),
        fieldName,
        fieldToken.span,
      )
      diagnostics.push(diagnostic)
      state = Object.freeze({ _tag: 'Unavailable', cause: Diagnostic.identity(diagnostic) })
    } else if (lookup.field.visibility === 'Private' && nominal.module !== source.id) {
      const diagnostic = Diagnostic.inaccessibleProjectedField(
        Type.encode(nominal),
        fieldName,
        fieldToken.span,
      )
      diagnostics.push(diagnostic)
      state = Object.freeze({ _tag: 'Unavailable', cause: Diagnostic.identity(diagnostic) })
    } else if (lookup.field.declaredType._tag === 'Resolved') {
      state = Object.freeze({ _tag: 'Resolved', field: lookup.field })
      const substitution =
        struct === undefined
          ? new Map<string, SemanticType>()
          : (TypeInference.substitution(
              struct.typeParameters.map((parameter) => parameter.type),
              nominal.arguments,
            ) ?? new Map())
      type = Type.substitute(lookup.field.declaredType.type, substitution)
    }
  }
  const typeFact = type === undefined ? unavailableExpressionType : availableExpressionType(type)
  const projectionAccess = borrowAccess ?? reference?.access
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'FieldProjection',
      subject: subject.fact,
      ...(nominal === undefined ? {} : { nominal }),
      ...(projectionAccess === undefined ? {} : { borrowAccess: projectionAccess }),
      fieldName,
      ...(fieldToken === undefined ? {} : { fieldToken }),
      state,
      type: typeFact,
      syntax: node,
    }),
    diagnostics: Object.freeze(diagnostics),
    type,
  })
}

import type { CallTypeArgumentsResult } from './CallResolution.js'
import {
  analyzeArgumentNodes,
  analyzeArguments,
  analyzeCallContract,
  analyzeCallTypeArguments,
  analyzeFunctionItem,
  boundOperationReference,
  builtinSignature,
  captureAccess,
  copyAssumptionsOf,
  executableSite,
  executableSpecializationOwner,
  finishCallableApplication,
  finishCallableSection,
  hasAvailableCallSyntax,
  interfaceConstraintDiagnostics,
  interfaceOperationContract,
  isSectionArity,
  ownedProviderCaptureAccess,
  resolvedFunctionReference,
  serviceOperation,
  sourceCallable,
  unavailableIdentifierFact,
} from './CallResolution.js'
export function analyzePlaceReplace(
  source: SourceFile.SourceFile,
  call: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult {
  const argumentList = SyntaxTree.directNode(call, 'ArgumentList')
  const nodes =
    argumentList === undefined ? [] : argumentList.children.filter(isRecursiveArgumentNode)
  const destinationNode = nodes.at(0)
  const valueNode = nodes.at(1)
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  if (destinationNode === undefined || valueNode === undefined || nodes.length !== 2) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'PlaceReplace' as const,
        reference: intrinsicReference(source, call),
        destination: unavailableIdentifierFact(call),
        value: unavailableIdentifierFact(call),
        compatible: false,
        type: unavailableExpressionType,
        syntax: call,
      }),
      diagnostics: Object.freeze([
        Diagnostic.wrongCallArity(
          Object.freeze({ _tag: 'BuiltinTarget', actor: 'Place', operation: 'replace' }),
          2,
          nodes.length,
          call.span,
        ),
      ]),
      type: undefined,
    })
  }
  const destination = analyzeExpression(
    source,
    destinationNode,
    declarations,
    declaration,
    scope,
    resolution,
  )
  if (destination === undefined) {
    throw new RangeError(`Semantic analysis cannot analyze ${destinationNode.kind}`)
  }
  diagnostics.push(...destination.diagnostics)
  const value = analyzeExpression(
    source,
    valueNode,
    declarations,
    declaration,
    scope,
    resolution,
    destination.type,
  )
  if (value === undefined) {
    throw new RangeError(`Semantic analysis cannot analyze ${valueNode.kind}`)
  }
  diagnostics.push(...value.diagnostics)
  const root = assignmentRoot(destination.fact)
  if (root === undefined) {
    if (SyntaxTree.isAvailableSyntax(destinationNode) && destination.diagnostics.length === 0) {
      diagnostics.push(Diagnostic.invalidAssignmentPlace(destinationNode.span))
    }
  } else if (root._tag === 'BindingFact' && root.mutability === 'Immutable') {
    diagnostics.push(
      Diagnostic.immutableAssignment(
        root.name._tag === 'Present' ? root.name.spelling : '?',
        destinationNode.span,
      ),
    )
  } else if (
    root._tag === 'ParameterDeclaration' &&
    (root.declaredType._tag !== 'Resolved' ||
      !(
        (Type.isSlice(root.declaredType.type) || Type.isReference(root.declaredType.type)) &&
        root.declaredType.type.access === 'Exclusive'
      ) ||
      (destination.fact._tag !== 'IndexProjection' && destination.fact._tag !== 'FieldProjection'))
  ) {
    diagnostics.push(Diagnostic.invalidAssignmentPlace(destinationNode.span))
  }
  const compatible =
    destination.type !== undefined &&
    value.type !== undefined &&
    typesCompatible(value.type, destination.type)
  if (destination.type !== undefined && value.type !== undefined && !compatible) {
    const expectedOrigin =
      root?._tag === 'BindingFact' ? root.initializer.syntax.span : destinationNode.span
    diagnostics.push(
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
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'PlaceReplace' as const,
      reference: intrinsicReference(source, call),
      destination: destination.fact,
      ...(root === undefined ? {} : { root }),
      value: value.fact,
      compatible,
      type:
        destination.type === undefined
          ? unavailableExpressionType
          : Object.freeze({ _tag: 'Available' as const, type: destination.type }),
      syntax: call,
    }),
    diagnostics: Object.freeze(diagnostics),
    type: destination.type,
  })
}

export const directProviderReference = (
  expression: ExpressionFact,
): BindingDeclarationFact | ParameterFact | undefined => {
  if (expression._tag === 'Identifier') {
    if (expression.reference._tag === 'ResolvedBinding') return expression.reference.binding
    if (expression.reference._tag === 'Resolved') return expression.reference.parameter
    return undefined
  }
  if (expression._tag === 'Borrow' || expression._tag === 'Move')
    return directProviderReference(expression.subject)
  if (expression._tag === 'Grouped') return directProviderReference(expression.expression)
  return undefined
}

export const selectedRequirementShape = (
  row: Type.RequirementsRow,
): { readonly capability: Type.Nominal | Type.Parameter; readonly role: string } | undefined => {
  const concrete = RowAlgebra.concretize(Type.requirementRowPolicy(), row)
  if (concrete._tag === 'Concrete') {
    const selected = concrete.row.members.at(0)
    return concrete.row.members.length === 1 && selected !== undefined
      ? Object.freeze({ capability: selected.capability, role: selected.role })
      : undefined
  }
  return row.expression._tag === 'Singleton'
    ? Object.freeze({
        capability: row.expression.member.capability,
        role: row.expression.member.role,
      })
    : undefined
}

export const intrinsicContractReference = (
  operation: Intrinsic.Operation,
  operationToken: Token.Token,
): Extract<CallReferenceFact, { readonly _tag: 'ResolvedIntrinsicContract' }> => {
  if (operation.rule._tag !== 'ContractRule')
    throw new RangeError('intrinsic contract reference requires a contract operation')
  const contract =
    operation.rule.contract.unsafe === operation.unsafe
      ? operation.rule.contract
      : CallableContract.make({
          functionKind: operation.rule.contract.functionKind,
          unsafe: operation.unsafe,
          binders: operation.rule.contract.binders,
          parameters: operation.rule.contract.parameters,
          result: operation.rule.contract.result,
          constraints: operation.rule.contract.constraints,
          captures: operation.rule.contract.captures,
        })
  return Object.freeze({
    _tag: 'ResolvedIntrinsicContract',
    spelling: `Intrinsic.${operation.spelling}`,
    token: operationToken,
    intrinsic: operation,
    contract,
  })
}

export const effectBindingProvider = (
  operation: Intrinsic.Operation,
  substitution: Type.Substitution,
  evidence: ReadonlyArray<Constraint.ConstraintEvidence>,
  provider: ExpressionFact,
  span: SourceSpan.SourceSpan,
  index?: DeclarationIndex.Index,
): EffectRequirementBindingFact | undefined => {
  if (
    operation.rule._tag !== 'ContractRule' ||
    operation.rule.post !== 'BindRequirement' ||
    operation.rule.providerMode === undefined
  )
    return undefined
  const wanted = operation.rule.contract.constraints
    .map((constraint) => Constraint.substitute(constraint, substitution))
    .find(
      (constraint): constraint is Constraint.ProviderSelection =>
        constraint._tag === 'ProviderSelectionConstraint',
    )
  if (wanted === undefined) return undefined
  const providerReference = directProviderReference(provider)
  if (
    providerReference === undefined ||
    !(Type.isNominal(wanted.provider) || Type.isParameter(wanted.provider))
  )
    return undefined
  const wantedKey = Constraint.key(wanted)
  const proof = evidence.find(
    (candidate) =>
      (candidate._tag === 'Assumed' || candidate._tag === 'RequirementSelection') &&
      candidate.wantedKey === wantedKey,
  )
  if (proof === undefined) return undefined
  const selected = selectedRequirementShape(wanted.selected)
  return Object.freeze({
    _tag: 'EffectRequirementBinding',
    reference: providerReference,
    selected: wanted.selected,
    evidence,
    ...(selected === undefined ? {} : { capability: selected.capability }),
    providerType: wanted.provider,
    ...(selected === undefined ? {} : { role: selected.role }),
    selectionAccess: operation.rule.providerMode,
    captureAccess:
      provider._tag === 'Move' &&
      provider.subject.type._tag === 'Available' &&
      index !== undefined &&
      ConformanceProof.copyType(index, provider.subject.type.type)
        ? 'Copy'
        : captureAccess(provider, index),
    span,
  })
}

export const sectionIntrinsicReference = (
  section: CallableSectionExpressionFact,
): IntrinsicReferenceFact => {
  if (section.reference._tag !== 'ResolvedIntrinsicContract')
    return Object.freeze({ _tag: 'UnavailableIntrinsicReference', syntax: section.syntax })
  const actor = Intrinsic.findActor('Intrinsic')
  const actorToken = section.path._tag === 'ReferencePath' ? section.path.qualifier : undefined
  if (actor === undefined || actorToken === undefined)
    return Object.freeze({ _tag: 'UnavailableIntrinsicReference', syntax: section.syntax })
  return Object.freeze({
    _tag: 'ResolvedIntrinsicReference',
    actor,
    operation: section.reference.intrinsic,
    actorToken,
    operationToken: section.reference.token,
  })
}

export const finishIntrinsicContractCall = (
  source: SourceFile.SourceFile,
  call: SyntaxTree.Node,
  operation: Intrinsic.Operation,
  operationToken: Token.Token,
  argumentsResult: ArgumentsResult,
  typeArguments: CallTypeArgumentsResult,
  resolution: ResolutionContext,
  caller: DeclarationFact,
): ExpressionResult => {
  if (operation.rule._tag !== 'ContractRule')
    throw new RangeError('intrinsic contract finisher received a non-contract operation')
  const reference = intrinsicContractReference(operation, operationToken)
  const analyzed = analyzeCallContract(
    call,
    reference,
    argumentsResult.facts,
    hasAvailableCallSyntax(call),
    typeArguments,
    resolution,
    caller,
  )
  const unsafeDiagnostic = unsafeCallDiagnostic(
    operation.unsafe,
    reference.spelling,
    call,
    resolution,
  )
  const substitution =
    analyzed.fact._tag === 'Compatible'
      ? analyzed.fact.substitution
      : new Map<string, Type.GenericArgument>()
  const substitutedResult = Type.substitute(operation.rule.contract.result, substitution)
  const type =
    analyzed.fact._tag === 'Compatible' &&
    unsafeDiagnostic === undefined &&
    Type.isEffect(substitutedResult)
      ? availableExpressionType(
          Type.effectWithRows(
            substitutedResult.success,
            substitutedResult.failureRow,
            intrinsicEffectCaptureAccess(
              operation,
              argumentsResult.facts,
              resolution.index,
              copyAssumptionsOf(caller),
            ),
            substitutedResult.requirementRow,
          ),
        )
      : unavailableExpressionType
  const protected_ = argumentsResult.facts.at(0)
  const evidence = analyzed.fact._tag === 'Compatible' ? analyzed.fact.evidence : Object.freeze([])
  if (operation.rule.post === 'CatchFailure') {
    const handler = argumentsResult.facts.at(1)
    const wanted = operation.rule.contract.constraints
      .map((constraint) => Constraint.substitute(constraint, substitution))
      .find(
        (constraint): constraint is Constraint.FailureSubset =>
          constraint._tag === 'FailureSubsetConstraint',
      )
    const wantedKey = wanted === undefined ? undefined : Constraint.key(wanted)
    const proved =
      wantedKey !== undefined &&
      evidence.some(
        (candidate) =>
          (candidate._tag === 'Assumed' && candidate.wantedKey === wantedKey) ||
          (candidate._tag === 'FailureSubset' &&
            wanted !== undefined &&
            RowAlgebra.equals(Type.failureRowPolicy(), candidate.selected, wanted.selected) &&
            RowAlgebra.equals(Type.failureRowPolicy(), candidate.source, wanted.source)),
      )
    const handlerType = handler?.type._tag === 'Available' ? handler.type.type : undefined
    const handlerEffect =
      handlerType !== undefined && Type.isCallable(handlerType) && Type.isEffect(handlerType.result)
        ? handlerType.result
        : undefined
    const catchAvailable =
      type._tag === 'Available' &&
      protected_ !== undefined &&
      handler !== undefined &&
      wanted !== undefined &&
      proved
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'EffectCatch',
        reference: intrinsicReference(source, call),
        protected: protected_?.expression ?? unavailableExpression(call),
        handler: handler?.expression ?? unavailableExpression(call),
        ...(wanted === undefined ? {} : { selected: Type.failureType(wanted.selected) }),
        protectedRow: wanted?.source ?? RowAlgebra.concrete(Type.failureRowPolicy(), []),
        handlerRow: handlerEffect?.failureRow ?? RowAlgebra.concrete(Type.failureRowPolicy(), []),
        residualRow:
          wanted === undefined
            ? RowAlgebra.concrete(Type.failureRowPolicy(), [])
            : RowAlgebra.without(Type.failureRowPolicy(), wanted.source, wanted.selected),
        evidence,
        type: catchAvailable ? type : unavailableExpressionType,
        syntax: call,
      }),
      diagnostics: Object.freeze([
        ...argumentsResult.diagnostics,
        ...typeArguments.diagnostics,
        ...analyzed.diagnostics,
        ...(unsafeDiagnostic === undefined ? [] : [unsafeDiagnostic]),
      ]),
      type: catchAvailable && type._tag === 'Available' ? type.type : undefined,
    })
  }
  const provider = argumentsResult.facts.at(1)
  const binding =
    provider === undefined
      ? undefined
      : effectBindingProvider(
          operation,
          substitution,
          evidence,
          provider.expression,
          provider.syntax.span,
          resolution.index,
        )
  const bindingAvailable =
    type._tag === 'Available' && protected_ !== undefined && binding !== undefined
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'EffectBindRequirement',
      reference: intrinsicReference(source, call),
      protected: protected_?.expression ?? unavailableExpression(call),
      ...(bindingAvailable
        ? {
            provider: binding,
          }
        : {}),
      type,
      syntax: call,
    }),
    diagnostics: Object.freeze([
      ...argumentsResult.diagnostics,
      ...typeArguments.diagnostics,
      ...analyzed.diagnostics,
      ...(unsafeDiagnostic === undefined ? [] : [unsafeDiagnostic]),
    ]),
    type: type._tag === 'Available' ? type.type : undefined,
  })
}

export function analyzeBuiltinCall(
  source: SourceFile.SourceFile,
  call: SyntaxTree.Node,
  argumentsResult: ArgumentsResult,
  typeArguments: CallTypeArgumentsResult,
  resolution: ResolutionContext,
  caller: DeclarationFact,
): ExpressionResult {
  const identifiers = callReferenceTokens(call)
  const actorToken = identifiers.at(0)
  const operationToken = identifiers.at(1)

  if (actorToken === undefined || operationToken === undefined) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Call',
        reference: Object.freeze({
          _tag: 'Unavailable',
          syntax: unavailableSyntax(call, 'Identifier'),
        }),
        path: referencePath(call),
        typeArguments: typeArguments.facts,
        arguments: argumentsResult.facts,
        mappings: Object.freeze([]),
        contract: Object.freeze({
          _tag: 'Unavailable',
          reason: Object.freeze({ _tag: 'UnavailableCallSyntax', syntax: call }),
        }),
        type: unavailableExpressionType,
        syntax: call,
      }),
      diagnostics: Object.freeze([...argumentsResult.diagnostics, ...typeArguments.diagnostics]),
      type: undefined,
    })
  }

  const actorSpelling = spelling(source, actorToken)
  const operationSpelling = spelling(source, operationToken)
  const actor = Intrinsic.findActor(actorSpelling)
  const operation = Intrinsic.findOperation(actorSpelling, operationSpelling)
  if (
    operation?.rule._tag === 'ContractRule' &&
    isSectionArity(operation.rule.contract.parameters.length, argumentsResult.facts.length)
  )
    return finishCallableSection(
      call,
      intrinsicContractReference(operation, operationToken),
      argumentsResult,
      typeArguments,
      resolution,
      caller,
    )
  if (operation?.rule._tag === 'ContractRule')
    return finishIntrinsicContractCall(
      source,
      call,
      operation,
      operationToken,
      argumentsResult,
      typeArguments,
      resolution,
      caller,
    )
  const signature = builtinSignature(actorSpelling, operationSpelling)
  const declaredTypeParameters = signature?.typeParameters ?? Object.freeze([])
  const specializationDiagnostic =
    typeArguments.explicit &&
    (typeArguments.types === undefined ||
      typeArguments.types.length !== declaredTypeParameters.length)
      ? Diagnostic.typeArgumentArity(
          `${actorSpelling}.${operationSpelling}`,
          declaredTypeParameters.length,
          typeArguments.types?.length ?? 0,
          call.span,
        )
      : undefined
  const substitution = new Map<string, Type.GenericArgument>()
  if (
    typeArguments.explicit &&
    typeArguments.types !== undefined &&
    typeArguments.types.length === declaredTypeParameters.length
  ) {
    for (const [ordinal, parameter] of declaredTypeParameters.entries()) {
      const argument = typeArguments.types.at(ordinal)
      if (argument !== undefined) substitution.set(Type.key(parameter), argument)
    }
  } else if (!typeArguments.explicit && signature !== undefined) {
    for (const [ordinal, parameter] of signature.parameters.entries()) {
      const argument = argumentsResult.facts.at(ordinal)
      if (argument?.type._tag === 'Available')
        TypeInference.infer(parameter, argument.type.type, substitution)
    }
  }
  const missingInference = declaredTypeParameters.find(
    (parameter) => substitution.get(Type.key(parameter)) === undefined,
  )
  const inferenceDiagnostic =
    specializationDiagnostic === undefined && missingInference !== undefined
      ? Diagnostic.typeArgumentArity(
          `${actorSpelling}.${operationSpelling}`,
          declaredTypeParameters.length,
          0,
          call.span,
        )
      : undefined
  const instantiatedParameters =
    signature === undefined
      ? Object.freeze([])
      : Object.freeze(
          signature.parameters.map((parameter) => Type.substitute(parameter, substitution)),
        )
  const instantiatedResult =
    signature === undefined ? undefined : Type.substitute(signature.result, substitution)
  const unsafeDiagnostic =
    signature === undefined
      ? undefined
      : unsafeCallDiagnostic(
          signature.unsafe === true,
          `${actorSpelling}.${operationSpelling}`,
          call,
          resolution,
        )
  const missingDiagnostic =
    actor === undefined
      ? Diagnostic.unknownActor(actorSpelling, actorToken.span)
      : signature === undefined
        ? Diagnostic.unknownActorOperation(actorSpelling, operationSpelling, operationToken.span)
        : undefined
  const reference: CallReferenceFact =
    signature !== undefined
      ? Object.freeze({
          _tag: 'ResolvedBuiltin',
          spelling: `${actorSpelling}.${operationSpelling}`,
          token: operationToken,
          actor: actorSpelling,
          operation: signature.operation,
          intrinsic: signature.id,
          parameters: instantiatedParameters,
          result: instantiatedResult ?? signature.result,
          unsafe: signature.unsafe === true,
          ...(signature.returnedBorrowParameter === undefined
            ? {}
            : { returnedBorrowParameter: signature.returnedBorrowParameter }),
        })
      : Object.freeze({
          _tag: 'Missing',
          spelling: `${actorSpelling}.${operationSpelling}`,
          token: actor === undefined ? actorToken : operationToken,
          ...(missingDiagnostic === undefined
            ? {}
            : { cause: Diagnostic.identity(missingDiagnostic) }),
        })
  if (
    reference._tag === 'ResolvedBuiltin' &&
    declaredTypeParameters.length === 0 &&
    isSectionArity(reference.parameters.length, argumentsResult.facts.length)
  ) {
    return finishCallableSection(
      call,
      reference,
      argumentsResult,
      typeArguments,
      resolution,
      caller,
    )
  }
  const callContract = analyzeCallContract(call, reference, argumentsResult.facts)
  const expressionType =
    hasAvailableCallSyntax(call) &&
    reference._tag === 'ResolvedBuiltin' &&
    specializationDiagnostic === undefined &&
    inferenceDiagnostic === undefined &&
    unsafeDiagnostic === undefined
      ? availableExpressionType(reference.result)
      : unavailableExpressionType

  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Call',
      reference,
      path: referencePath(call),
      typeArguments: typeArguments.facts,
      arguments: argumentsResult.facts,
      mappings: callContract.mappings,
      contract: callContract.fact,
      type: expressionType,
      syntax: call,
    }),
    diagnostics: Object.freeze([
      ...(missingDiagnostic === undefined ? [] : [missingDiagnostic]),
      ...(specializationDiagnostic === undefined ? [] : [specializationDiagnostic]),
      ...(inferenceDiagnostic === undefined ? [] : [inferenceDiagnostic]),
      ...(unsafeDiagnostic === undefined ? [] : [unsafeDiagnostic]),
      ...argumentsResult.diagnostics,
      ...typeArguments.diagnostics,
      ...callContract.diagnostics,
    ]),
    type: expressionType._tag === 'Available' ? expressionType.type : undefined,
  })
}

export const builtinArgumentMappings = (
  reference: CallReferenceFact,
  argumentsList: ReadonlyArray<ArgumentFact>,
): ReadonlyArray<BuiltinArgumentMappingFact> =>
  reference._tag !== 'ResolvedBuiltin'
    ? Object.freeze([])
    : Object.freeze(
        reference.parameters.flatMap(
          (expected, ordinal): ReadonlyArray<BuiltinArgumentMappingFact> => {
            const argument = argumentsList.at(ordinal)
            return argument === undefined
              ? []
              : [Object.freeze({ _tag: 'BuiltinArgumentMapping', argument, ordinal, expected })]
          },
        ),
      )

export const analyzeGroupedExpression = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
  expected?: SemanticType,
  borrowAllowed = false,
): ExpressionResult => {
  const child = node.children.find(isExpressionNode)
  const expression =
    child === undefined
      ? undefined
      : analyzeExpression(
          source,
          child,
          declarations,
          declaration,
          scope,
          resolution,
          expected,
          borrowAllowed,
        )
  if (expression === undefined) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Grouped',
        expression: Object.freeze({
          _tag: 'Integer',
          integer: Object.freeze({ _tag: 'Unavailable', syntax: node }),
          type: unavailableExpressionType,
          syntax: node,
        }),
        type: unavailableExpressionType,
        syntax: node,
      }),
      diagnostics: Object.freeze([]),
      type: undefined,
    })
  }
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Grouped',
      expression: expression.fact,
      type: expression.fact.type,
      syntax: node,
    }),
    diagnostics: expression.diagnostics,
    type: expression.type,
  })
}

/**
 * Analyzes `&&` or `||`. Both operands must be `bool` and the result is `bool`. HIR retains the
 * right operand as a conditional region, so its ordinary effects, moves, loans, and cleanup stay
 * on the path that executes it.
 */
export const analyzeShortCircuitExpression = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  operator: Operator.ShortCircuit,
  operandNodes: ReadonlyArray<SyntaxTree.Node>,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult => {
  const boolean: SemanticType = Scalar.boolean.spelling
  const argumentsResult = analyzeArgumentNodes(
    source,
    node,
    operandNodes,
    declarations,
    declaration,
    scope,
    resolution,
    Object.freeze(operandNodes.map(() => boolean)),
  )
  const operandDiagnostics = argumentsResult.facts.flatMap((argument) =>
    argument.type._tag === 'Available' && !Type.equals(argument.type.type, boolean)
      ? [
          Diagnostic.argumentTypeMismatch(
            Type.encode(boolean),
            Type.encode(argument.type.type),
            argument.syntax.span,
          ),
        ]
      : [],
  )
  const rejected =
    operandDiagnostics.length > 0 ||
    argumentsResult.facts.length !== 2 ||
    argumentsResult.facts.some((argument) => argument.type._tag !== 'Available')
  const type = rejected ? unavailableExpressionType : availableExpressionType(boolean)
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'ShortCircuit',
      operator,
      arguments: argumentsResult.facts,
      type,
      syntax: node,
    }),
    diagnostics: Object.freeze([...argumentsResult.diagnostics, ...operandDiagnostics]),
    type: type._tag === 'Available' ? type.type : undefined,
  })
}

export interface OperatorContractSelection extends InterfaceOperationFact {
  readonly declaration: DeclarationFacts.ServiceOperationFact
  readonly parameters: ReadonlyArray<SemanticType>
  readonly result: SemanticType
  readonly label: string
}

export const operatorContractSelection = (
  capability: Type.Nominal,
  provider: Type.Type,
  operation: DeclarationFacts.InterfaceOperationApplicationFact,
): OperatorContractSelection | undefined => {
  const contract = interfaceOperationContract(operation)
  const name = operation.declaration.name
  if (contract === undefined || name._tag !== 'Present') return undefined
  return Object.freeze({
    capability,
    provider,
    operation: name.spelling,
    contract: operation,
    declaration: contract.declaration,
    parameters: contract.parameters,
    result: contract.result,
    label: `${Type.encode(capability)}.${name.spelling}`,
  })
}

export const boundOperatorSelections = (
  declaration: DeclarationFact,
  operator: Operator.Eligible,
): ReadonlyArray<OperatorContractSelection> =>
  Object.freeze(
    declaration.typeParameters.flatMap((parameter) =>
      parameter.bounds.flatMap((bound) =>
        bound._tag !== 'ResolvedBound'
          ? []
          : bound.application.operations.flatMap((operation) => {
              if (operation.declaration.operator?.operator !== operator) return []
              const selected = operatorContractSelection(
                bound.application.capability,
                parameter.type,
                operation,
              )
              return selected === undefined ? [] : [selected]
            }),
      ),
    ),
  )

export const concreteOperatorSelections = (
  index: DeclarationIndex.Index,
  module: string,
  operator: Operator.Eligible,
): ReadonlyArray<OperatorContractSelection> => {
  const interfaces = index.modules.flatMap((headers) => headers.interfaces)
  const selections = index.modules.flatMap((headers) =>
    headers.conformances.flatMap((conformance) => {
      if (
        conformance.validity._tag !== 'ValidConformance' ||
        conformance.coherence._tag !== 'Coherent' ||
        conformance.termination._tag !== 'Terminating' ||
        conformance.capability._tag !== 'Resolved' ||
        conformance.provider._tag !== 'Resolved' ||
        !Type.isNominal(conformance.capability.type)
      )
        return []
      const capability = conformance.capability.type
      const provider = conformance.provider.type
      const interface_ = interfaces.find(
        (candidate) =>
          candidate.canonical._tag === 'Canonical' &&
          candidate.canonical.id.module === capability.module &&
          candidate.canonical.id.name === capability.name &&
          (candidate.visibility === 'Public' || candidate.canonical.id.module === module),
      )
      if (interface_ === undefined) return []
      const proof = ConformanceProof.prove(index, provider, capability)
      if (
        proof._tag !== 'Proved' ||
        proof.selection._tag !== 'SourceSelection' ||
        proof.selection.module !== conformance.module ||
        proof.selection.ordinal !== conformance.ordinal
      )
        return []
      const application = DeclarationFacts.interfaceApplication(interface_, capability, provider)
      if (application?.available !== true) return []
      return application.operations.flatMap((operation) => {
        if (operation.declaration.operator?.operator !== operator) return []
        const selected = operatorContractSelection(capability, provider, operation)
        return selected === undefined ? [] : [selected]
      })
    }),
  )
  const unique = new Map<string, OperatorContractSelection>()
  for (const selection of selections)
    unique.set(
      `${Type.key(selection.capability)}\u0000${Type.key(selection.provider)}\u0000${selection.operation}`,
      selection,
    )
  return Object.freeze([...unique.values()])
}

export const operatorSelectionMatches = (
  selection: OperatorContractSelection,
  arguments_: ReadonlyArray<ArgumentFact>,
): boolean =>
  selection.parameters.length === arguments_.length &&
  arguments_.every((argument, ordinal) => {
    const expected = selection.parameters.at(ordinal)
    return (
      expected !== undefined &&
      argument.type._tag === 'Available' &&
      (typesCompatible(argument.type.type, expected) ||
        contextualIntegerCompatible(argument.expression, expected))
    )
  })

export const finishInterfaceOperator = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  operator: Operator.Eligible,
  operatorToken: Token.Token,
  operandNodes: ReadonlyArray<SyntaxTree.Node>,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
  selection: OperatorContractSelection,
): ExpressionResult => {
  const argumentsResult = analyzeArgumentNodes(
    source,
    node,
    operandNodes,
    declarations,
    declaration,
    scope,
    resolution,
    selection.parameters,
  )
  const arguments_ = Object.freeze(
    argumentsResult.facts.map((argument) => {
      let expression = argument.expression
      while (expression._tag === 'Grouped') expression = expression.expression
      return expression === argument.expression
        ? argument
        : Object.freeze({ ...argument, expression, syntax: expression.syntax })
    }),
  )
  const reference: CallReferenceFact = Object.freeze({
    _tag: 'ResolvedBoundOperation',
    spelling: selection.label,
    token: operatorToken,
    capability: selection.capability,
    provider: selection.provider,
    operation: selection.operation,
    declaration: selection.declaration,
    interfaceContract: selection.contract,
    parameters: selection.parameters,
    result: selection.result,
  })
  const contract = analyzeCallContract(node, reference, arguments_, true)
  const type =
    contract.fact._tag === 'Compatible'
      ? availableExpressionType(selection.result)
      : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Operator',
      operator,
      reference,
      arguments: arguments_,
      mappings: Object.freeze(
        selection.parameters.flatMap((expected, ordinal) => {
          const argument = arguments_.at(ordinal)
          return argument === undefined
            ? []
            : [
                Object.freeze({
                  _tag: 'BuiltinArgumentMapping' as const,
                  argument,
                  ordinal,
                  expected,
                }),
              ]
        }),
      ),
      contract: contract.fact,
      interfaceOperation: selection,
      ...(selection.contract.functionKind === 'Effect'
        ? { witnessEffectSite: executableSite('EffectSiteId', resolution, node) }
        : {}),
      type,
      syntax: node,
    }),
    diagnostics: Object.freeze([...argumentsResult.diagnostics, ...contract.diagnostics]),
    type: type._tag === 'Available' ? type.type : undefined,
  })
}

export const analyzeOperatorExpression = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
  expected?: SemanticType,
): ExpressionResult => {
  const operatorToken = node.children.find(
    (element): element is Token.Token =>
      SyntaxTree.isToken(element) &&
      (node.kind === 'PrefixExpression'
        ? Operator.prefix(element.kind) !== undefined
        : Operator.infix(element.kind) !== undefined),
  )
  const operator =
    operatorToken === undefined
      ? undefined
      : node.kind === 'PrefixExpression'
        ? Operator.prefix(operatorToken.kind)
        : Operator.infix(operatorToken.kind)?.operator
  const operandNodes = node.children.filter(isExpressionNode)
  if (operator !== undefined && Operator.isShortCircuit(operator)) {
    return analyzeShortCircuitExpression(
      source,
      node,
      operator,
      operandNodes,
      declarations,
      declaration,
      scope,
      resolution,
    )
  }
  const initialExpected =
    typeof expected === 'string' &&
    Scalar.isSpelling(expected) &&
    node.kind === 'InfixExpression' &&
    operator !== undefined &&
    !Operator.isPredicate(operator)
      ? Object.freeze(operandNodes.map(() => expected))
      : Object.freeze([])
  let argumentsResult = analyzeArgumentNodes(
    source,
    node,
    operandNodes,
    declarations,
    declaration,
    scope,
    resolution,
    initialExpected,
  )
  if (operator === undefined || operatorToken === undefined) {
    const reference: CallReferenceFact = Object.freeze({
      _tag: 'Unavailable',
      syntax: unavailableSyntax(node, node.kind === 'PrefixExpression' ? 'Minus' : 'Plus'),
    })
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Operator',
        operator: node.kind === 'PrefixExpression' ? 'Negate' : 'Add',
        reference,
        arguments: argumentsResult.facts,
        mappings: Object.freeze([]),
        contract: Object.freeze({
          _tag: 'Unavailable',
          reason: Object.freeze({ _tag: 'UnavailableCallSyntax', syntax: node }),
        }),
        type: unavailableExpressionType,
        syntax: node,
      }),
      diagnostics: argumentsResult.diagnostics,
      type: undefined,
    })
  }

  const firstType = argumentsResult.facts.at(0)?.type
  if (
    firstType?._tag === 'Available' &&
    typeof firstType.type === 'string' &&
    Scalar.isSpelling(firstType.type) &&
    (initialExpected.length === 0 || firstType.type !== expected) &&
    operandNodes.length > 1
  ) {
    argumentsResult = analyzeArgumentNodes(
      source,
      node,
      operandNodes,
      declarations,
      declaration,
      scope,
      resolution,
      Object.freeze(operandNodes.map(() => firstType.type)),
    )
  }
  const selectedFirstType = argumentsResult.facts.at(0)?.type
  const builtinOperand =
    selectedFirstType?._tag === 'Available' &&
    (Type.isString(selectedFirstType.type) || Scalar.isSpelling(selectedFirstType.type))
  if (!builtinOperand) {
    const candidates = [
      ...boundOperatorSelections(declaration, operator),
      ...concreteOperatorSelections(resolution.index, source.id, operator),
    ].filter((candidate) => operatorSelectionMatches(candidate, argumentsResult.facts))
    if (candidates.length === 1) {
      const candidate = candidates.at(0)
      if (candidate !== undefined)
        return finishInterfaceOperator(
          source,
          node,
          operator,
          operatorToken,
          operandNodes,
          declarations,
          declaration,
          scope,
          resolution,
          candidate,
        )
    }
    const operatorSpelling = spelling(source, operatorToken)
    const operandTypes = argumentsResult.facts.flatMap((argument) =>
      argument.type._tag === 'Available' ? [Type.encode(argument.type.type)] : [],
    )
    const diagnostic =
      candidates.length > 1
        ? Diagnostic.ambiguousOperator(
            operatorSpelling,
            candidates.map((candidate) => candidate.label),
            operatorToken.span,
          )
        : Diagnostic.operatorNotApplicable(operatorSpelling, operandTypes, operatorToken.span)
    const reference: CallReferenceFact = Object.freeze({
      _tag: 'Missing',
      spelling: operatorSpelling,
      token: operatorToken,
      cause: Diagnostic.identity(diagnostic),
    })
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Operator',
        operator,
        reference,
        arguments: argumentsResult.facts,
        mappings: Object.freeze([]),
        contract: Object.freeze({
          _tag: 'Unavailable',
          reason: Object.freeze({ _tag: 'UnavailableCallSyntax', syntax: node }),
          cause: Diagnostic.identity(diagnostic),
        }),
        type: unavailableExpressionType,
        syntax: node,
      }),
      diagnostics: Object.freeze([...argumentsResult.diagnostics, diagnostic]),
      type: undefined,
    })
  }
  const selectedActor: Operator.Actor =
    selectedFirstType?._tag === 'Available' && Type.isString(selectedFirstType.type)
      ? 'string'
      : selectedFirstType?._tag === 'Available' && Scalar.isSpelling(selectedFirstType.type)
        ? selectedFirstType.type
        : Scalar.defaultInteger.spelling
  const target = Operator.target(operator, selectedActor)
  const signature = builtinSignature(target.actor, target.operation, 'Primitive')
  if (signature === undefined) throw new RangeError('Compiler operator table is inconsistent')
  const operatorParameters = signature.parameters
  const operatorResult = signature.result
  const reference: CallReferenceFact = Object.freeze({
    _tag: 'ResolvedBuiltin',
    spelling: `${target.actor}.${target.operation}`,
    token: operatorToken,
    actor: target.actor,
    operation: signature.operation,
    intrinsic: signature.id,
    parameters: operatorParameters,
    result: operatorResult,
    unsafe: signature.unsafe === true,
  })
  const contract = analyzeCallContract(
    node,
    reference,
    argumentsResult.facts,
    isAvailableSyntax(node),
  )
  const expressionType =
    contract.fact._tag === 'Compatible'
      ? availableExpressionType(operatorResult)
      : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Operator',
      operator,
      reference,
      arguments: argumentsResult.facts,
      mappings: builtinArgumentMappings(reference, argumentsResult.facts),
      contract: contract.fact,
      type: expressionType,
      syntax: node,
    }),
    diagnostics: Object.freeze([...argumentsResult.diagnostics, ...contract.diagnostics]),
    type: expressionType._tag === 'Available' ? expressionType.type : undefined,
  })
}

export const analyzePipelineExpression = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult => {
  const inputNode = pipelineInput(node)
  const target = pipelineCallable(node)
  const callable =
    target === undefined
      ? undefined
      : analyzeExpression(source, target, declarations, declaration, scope, resolution)
  const expectedInput =
    callable?.type !== undefined && Type.isCallable(callable.type)
      ? callable.type.parameters.at(0)
      : undefined
  const input =
    inputNode === undefined
      ? undefined
      : analyzeExpression(
          source,
          inputNode,
          declarations,
          declaration,
          scope,
          resolution,
          expectedInput,
        )
  const inputFact = input?.fact ?? unavailableExpression(inputNode ?? node)
  const callableResult =
    callable ??
    Object.freeze({
      fact: unavailableExpression(target ?? node),
      diagnostics: Object.freeze([]),
      type: undefined,
    })
  return finishCallableApplication(
    node,
    callableResult,
    Object.freeze({
      facts: Object.freeze([argumentFact(declaration, node.span, inputFact, 0)]),
      diagnostics: input?.diagnostics ?? Object.freeze([]),
    }),
    Object.freeze({
      explicit: false,
      facts: Object.freeze([]),
      diagnostics: Object.freeze([]),
    }),
    Object.freeze({
      _tag: 'PipelineCallableApplication',
      left: inputFact,
      callable: callableResult.fact,
      evaluation: 'LeftThenCallable',
    }),
    resolution,
    declaration,
  )
}

export const effectExpressionAccess = (
  expression: ExpressionFact,
  index: DeclarationIndex.Index | undefined,
  assumptions: ReadonlySet<string> = new Set(),
): Type.Effect['access'] => {
  if (expression._tag === 'Move') {
    if (expression.subject.type._tag === 'Available' && Type.isEffect(expression.subject.type.type))
      return expression.subject.type.type.access
    if (
      expression.subject.type._tag === 'Available' &&
      Type.isCallable(expression.subject.type.type)
    )
      return expression.subject.type.type.mode
    if (
      expression.subject.type._tag === 'Available' &&
      index !== undefined &&
      ConformanceProof.copyType(index, expression.subject.type.type, assumptions)
    )
      return 'Shared'
    return 'Take'
  }
  if (expression._tag === 'Borrow')
    return expression.access === 'Exclusive' ? 'Exclusive' : 'Shared'
  if (expression._tag === 'Grouped')
    return effectExpressionAccess(expression.expression, index, assumptions)
  if (expression._tag === 'CallableSection') return expression.mode
  if (expression.type._tag === 'Available' && Type.isEffect(expression.type.type))
    return expression.type.type.access
  if (expression.type._tag === 'Available' && Type.isCallable(expression.type.type))
    return expression.type.type.mode
  return 'Shared'
}

export const effectCaptureAccess = (
  arguments_: ReadonlyArray<ArgumentFact>,
  index: DeclarationIndex.Index | undefined,
  assumptions: ReadonlySet<string> = new Set(),
): Type.Effect['access'] => {
  const accesses = arguments_.map((argument) =>
    effectExpressionAccess(argument.expression, index, assumptions),
  )
  return strongestEffectAccess(...accesses)
}

export const intrinsicEffectCaptureAccess = (
  operation: Intrinsic.Operation,
  arguments_: ReadonlyArray<ArgumentFact>,
  index: DeclarationIndex.Index,
  assumptions: ReadonlySet<string> = new Set(),
): Type.Effect['access'] => {
  if (
    operation.rule._tag !== 'ContractRule' ||
    operation.rule.post !== 'BindRequirement' ||
    operation.rule.providerMode !== 'Take'
  )
    return effectCaptureAccess(arguments_, index, assumptions)
  const accesses = arguments_.map((argument, ordinal) =>
    ordinal === 1
      ? ownedProviderCaptureAccess(argument.expression, index, assumptions)
      : effectExpressionAccess(argument.expression, index, assumptions),
  )
  return strongestEffectAccess(...accesses.flatMap((access) => (access === 'Copy' ? [] : [access])))
}

export const strongestEffectAccess = (
  ...accesses: ReadonlyArray<Type.Effect['access']>
): Type.Effect['access'] =>
  accesses.includes('Take') ? 'Take' : accesses.includes('Exclusive') ? 'Exclusive' : 'Shared'

export const intrinsicOperationTarget = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
): Intrinsic.Operation | undefined => {
  const identifiers = callReferenceTokens(node)
  const qualifier = identifiers.at(0)
  const member = identifiers.at(1)
  return qualifier === undefined || member === undefined
    ? undefined
    : Intrinsic.findOperation(spelling(source, qualifier), spelling(source, member))
}

export const intrinsicReference = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
): IntrinsicReferenceFact => {
  const identifiers = callReferenceTokens(node)
  const actorToken = identifiers.at(0)
  const operationToken = identifiers.at(1)
  const actor =
    actorToken === undefined ? undefined : Intrinsic.findActor(spelling(source, actorToken))
  const operation =
    actor === undefined || operationToken === undefined
      ? undefined
      : Intrinsic.findOperation(actor.spelling, spelling(source, operationToken))
  return actorToken === undefined ||
    operationToken === undefined ||
    actor === undefined ||
    operation === undefined
    ? Object.freeze({ _tag: 'UnavailableIntrinsicReference', syntax: node })
    : Object.freeze({
        _tag: 'ResolvedIntrinsicReference',
        actor,
        operation,
        actorToken,
        operationToken,
      })
}

export const isEffectResultTarget = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
): boolean => {
  const rule = intrinsicOperationTarget(source, node)?.rule
  return rule?._tag === 'EffectRule' && rule.operation === 'Result'
}

export const analyzeEffectResult = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult => {
  const pipelined = node.kind === 'PipelineExpression'
  const target = pipelined ? (pipelineCallable(node) ?? node) : node
  const list = SyntaxTree.directNode(target, 'ArgumentList')
  const argumentNodes =
    list?.children.filter((element): element is SyntaxTree.Node =>
      isRecursiveArgumentNode(element),
    ) ?? []
  const protectedNode = pipelined ? pipelineInput(node) : argumentNodes.at(0)
  const protectedResult =
    protectedNode === undefined
      ? undefined
      : analyzeExpression(source, protectedNode, declarations, declaration, scope, resolution)
  const protectedEffect =
    protectedResult?.type !== undefined && Type.isEffect(protectedResult.type)
      ? protectedResult.type
      : undefined
  const diagnostics: Array<Diagnostic.Diagnostic> = [...(protectedResult?.diagnostics ?? [])]
  if (argumentNodes.length !== (pipelined ? 0 : 1))
    diagnostics.push(
      Diagnostic.invalidEffectHandler('result requires exactly one Effect', node.span),
    )
  if (protectedEffect === undefined)
    diagnostics.push(
      Diagnostic.invalidEffectHandler(
        'the protected argument is not an Effect',
        protectedNode?.span ?? node.span,
      ),
    )
  const failureValue = protectedEffect === undefined ? 'never' : Type.failureType(protectedEffect)
  const type =
    protectedEffect === undefined
      ? unavailableExpressionType
      : availableExpressionType(
          Type.effectWithRows(
            Type.result(protectedEffect.success, failureValue),
            RowAlgebra.concrete(Type.failureRowPolicy(), []),
            protectedEffect.access,
            protectedEffect.requirementRow,
          ),
        )
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'EffectResult',
      reference: intrinsicReference(source, target),
      protected: protectedResult?.fact ?? unavailableExpression(node),
      type,
      syntax: node,
    }),
    diagnostics: Object.freeze(diagnostics),
    type: type._tag === 'Available' ? type.type : undefined,
  })
}

/** Finalizes ordinary lexical captures for one source Effect body. */
export const effectCaptureFacts = (
  statements: ReadonlyArray<StatementFact>,
  firstLocalBinding: number,
  index?: DeclarationIndex.Index,
  assumptions: ReadonlySet<string> = new Set(),
): ReadonlyArray<EffectCaptureFact> => {
  const captures = new Map<string, EffectCaptureFact>()
  const rank = (access: EffectCaptureFact['access']): number =>
    access === 'Take' ? 3 : access === 'Exclusive' ? 2 : access === 'Shared' ? 1 : 0
  const recordReference = (
    reference: BindingDeclarationFact | ParameterFact | undefined,
    requested: EffectCaptureFact['access'],
    span: SourceSpan.SourceSpan,
    copy: boolean,
  ): void => {
    if (reference === undefined) return
    if (reference._tag === 'BindingFact' && reference.id.ordinal >= firstLocalBinding) return
    const key = `${reference._tag}:${reference.id.ordinal}`
    const access = requested === 'Shared' && copy ? 'Copy' : requested
    const prior = captures.get(key)
    if (prior === undefined || rank(access) > rank(prior.access)) {
      captures.set(key, Object.freeze({ _tag: 'EffectCapture', reference, access, span }))
    }
  }
  const record = (fact: IdentifierExpressionFact, requested: EffectCaptureFact['access']): void => {
    const reference =
      fact.reference._tag === 'ResolvedBinding'
        ? fact.reference.binding
        : fact.reference._tag === 'Resolved'
          ? fact.reference.parameter
          : undefined
    recordReference(
      reference,
      requested,
      fact.syntax.span,
      fact.type._tag === 'Available' &&
        !Type.containsViewBorrow(fact.type.type) &&
        (index === undefined
          ? typeof fact.type.type === 'string'
          : ConformanceProof.copyType(index, fact.type.type, assumptions)),
    )
  }
  const expression = (
    fact: ExpressionFact,
    requested: EffectCaptureFact['access'] = 'Shared',
  ): void => {
    switch (fact._tag) {
      case 'Identifier':
        record(fact, requested)
        return
      case 'Move':
        expression(fact.subject, 'Take')
        return
      case 'Borrow':
        expression(fact.subject, fact.access === 'Exclusive' ? 'Exclusive' : 'Shared')
        return
      case 'Grouped':
      case 'FieldProjection':
        expression(fact._tag === 'Grouped' ? fact.expression : fact.subject, requested)
        return
      case 'IndexProjection':
        expression(fact.subject, requested)
        expression(fact.index)
        return
      case 'StructLiteral':
        for (const item of fact.initializers) expression(item.expression)
        return
      case 'ArrayLiteral':
        for (const item of fact.elements) expression(item.expression)
        return
      case 'Match':
        expression(fact.scrutinee, requested)
        for (const arm of fact.arms) {
          if (arm.guard !== undefined) expression(arm.guard)
          expression(arm.result)
        }
        return
      case 'Operator':
      case 'ShortCircuit':
      case 'Call':
        for (const argument of fact.arguments) expression(argument.expression)
        return
      case 'FunctionItem':
        return
      case 'CallableSection':
        for (const capture of fact.captures) expression(capture.expression)
        return
      case 'CallableApply':
        expression(fact.callee)
        for (const argument of fact.arguments) expression(argument.expression)
        return
      case 'Run':
        expression(fact.subject)
        return
      case 'EffectResult':
        expression(fact.protected)
        return
      case 'EffectCatch':
        expression(fact.protected)
        expression(fact.handler)
        return
      case 'EffectBindRequirement':
        expression(fact.protected)
        recordReference(
          fact.provider?.reference,
          fact.provider?.captureAccess ?? 'Shared',
          fact.syntax.span,
          false,
        )
        return
      case 'EffectBlock':
        // Constructing a nested deferred Effect still reads the environment needed to create
        // that Effect value. Bubble those dependencies into the enclosing Effect runner so a
        // parameter used only by the nested body remains available when the child is formed.
        for (const capture of fact.captures)
          recordReference(capture.reference, capture.access, capture.span, false)
        return
      case 'Integer':
      case 'Boolean':
      case 'Character':
      case 'Constant':
        return
    }
  }
  const visit = (items: ReadonlyArray<StatementFact>): void => {
    for (const statement of items) {
      switch (statement._tag) {
        case 'UnsafeStatement':
          visit(statement.statements)
          break
        case 'BindStatement':
          expression(statement.binding.initializer)
          break
        case 'PatternBindStatement':
          expression(statement.selection.source)
          break
        case 'ExpressionStatement':
          expression(statement.expression)
          break
        case 'IfStatement':
          expression(statement.condition)
          visit(statement.taken)
          visit(statement.otherwise)
          break
        case 'IfLetStatement':
          expression(statement.selection.source)
          visit(statement.taken)
          visit(statement.otherwise)
          break
        case 'WriteStatement':
          expression(statement.destination, 'Exclusive')
          expression(statement.value)
          break
        case 'WhileStatement':
          expression(statement.condition)
          visit(statement.body)
          break
        case 'ReturnStatement':
        case 'FailStatement':
        case 'DropStatement':
          expression(statement.expression)
          break
        case 'BreakStatement':
        case 'ContinueStatement':
          break
      }
    }
  }
  visit(statements)
  return Object.freeze(
    [...captures.values()].sort(
      (left, right) =>
        left.reference.id.ordinal - right.reference.id.ordinal ||
        left.span.start - right.span.start,
    ),
  )
}

export function analyzeExpression(
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
  expected?: SemanticType,
  borrowAllowed = false,
): ExpressionResult | undefined {
  if (node.kind === 'UnsafeExpression') {
    const call = SyntaxTree.directNode(node, 'CallExpression')
    if (call === undefined) return undefined
    const analyzed = analyzeExpression(
      source,
      call,
      declarations,
      declaration,
      scope,
      Object.freeze({
        ...resolution,
        unsafeCallSpans: Object.freeze([...(resolution.unsafeCallSpans ?? []), call.span]),
      }),
      expected,
      borrowAllowed,
    )
    if (analyzed === undefined) return undefined
    const invokesUnsafe = (() => {
      const fact = analyzed.fact
      if (fact._tag === 'CallableApply')
        return (
          fact.callee.type._tag === 'Available' &&
          Type.isCallable(fact.callee.type.type) &&
          fact.callee.type.type.unsafe
        )
      if (fact._tag !== 'Call') return false
      switch (fact.reference._tag) {
        case 'Resolved':
          return fact.reference.declaration.unsafe
        case 'ResolvedBuiltin':
          return fact.reference.unsafe
        case 'ResolvedIntrinsicContract':
          return fact.reference.intrinsic.unsafe
        case 'ResolvedServiceOperation':
          return fact.reference.operation.unsafe
        case 'ResolvedBoundOperation':
          return fact.reference.interfaceContract.unsafe
        default:
          return false
      }
    })()
    const diagnostic = invokesUnsafe
      ? undefined
      : Diagnostic.misplacedUnsafeAcknowledgement(node.span)
    return Object.freeze({
      fact: analyzed.fact,
      diagnostics: Object.freeze([
        ...analyzed.diagnostics,
        ...(diagnostic === undefined ? [] : [diagnostic]),
      ]),
      type: diagnostic === undefined ? analyzed.type : undefined,
    })
  }
  if (node.kind === 'EffectExpression') {
    const representationOwner = executableSpecializationOwner(resolution)
    const block = SyntaxTree.directNode(node, 'Block')
    if (block === undefined)
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'EffectBlock',
          site: executableSite('EffectSiteId', resolution, node),
          ...(representationOwner === undefined ? {} : { representationOwner }),
          statements: Object.freeze([]),
          captures: Object.freeze([]),
          bindings: Object.freeze([]),
          regions: Object.freeze([]),
          type: unavailableExpressionType,
          syntax: node,
        }),
        diagnostics: Object.freeze([]),
        type: undefined,
      })
    const firstLocalBinding = resolution.nextBindingOrdinal?.value ?? 0
    const nested: BodyContext = {
      source,
      declaration,
      declarations,
      bindings: [],
      diagnostics: [],
      regions: [],
      loops: [],
      resolution,
      nextBindingOrdinal: resolution.nextBindingOrdinal ?? { value: 0 },
      regionBase: 1_000_000 + node.span.start * 100,
      effectBlock: true,
    }
    const statements = analyzeStatements(nested, block, scope)
    const returned: Array<ExpressionFact> = []
    const failures: Array<Type.Nominal> = []
    const collectTerminals = (items: ReadonlyArray<StatementFact>): void => {
      for (const statement of items) {
        if (statement._tag === 'ReturnStatement') returned.push(statement.expression)
        else if (
          statement._tag === 'FailStatement' &&
          statement.failure !== undefined &&
          Type.isNominal(statement.failure)
        )
          failures.push(statement.failure)
        else if (statement._tag === 'IfStatement' || statement._tag === 'IfLetStatement') {
          collectTerminals(statement.taken)
          collectTerminals(statement.otherwise)
        } else if (statement._tag === 'WhileStatement') collectTerminals(statement.body)
      }
    }
    collectTerminals(statements)
    const success = returned.at(-1)?.type
    const captures = effectCaptureFacts(
      statements,
      firstLocalBinding,
      resolution.index,
      copyAssumptionsOf(declaration),
    )
    const access = strongestEffectAccess(
      ...captures.flatMap((capture) => (capture.access === 'Copy' ? [] : [capture.access])),
    )
    const type =
      success?._tag === 'Available'
        ? availableExpressionType(Type.effect(success.type, failures, access))
        : unavailableExpressionType
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'EffectBlock',
        site: executableSite('EffectSiteId', resolution, node),
        ...(representationOwner === undefined ? {} : { representationOwner }),
        statements,
        captures,
        bindings: Object.freeze(nested.bindings),
        regions: Object.freeze(nested.regions),
        type,
        syntax: node,
      }),
      diagnostics: Object.freeze(nested.diagnostics),
      type: type._tag === 'Available' ? type.type : undefined,
    })
  }
  if (node.kind === 'BooleanLiteralExpression') {
    const token = directToken(node, 'TrueKeyword') ?? directToken(node, 'FalseKeyword')
    const type = token === undefined ? unavailableExpressionType : availableBoolExpressionType
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Boolean',
        value: token?.kind === 'TrueKeyword',
        type,
        syntax: node,
      }),
      diagnostics: Object.freeze([]),
      type: type._tag === 'Available' ? type.type : undefined,
    })
  }

  if (node.kind === 'IntegerLiteralExpression') {
    const integer = analyzeInteger(source, node, expected)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Integer',
        integer: integer.fact,
        type:
          integer.fact._tag === 'Available'
            ? availableExpressionType(integer.fact.type)
            : unavailableExpressionType,
        syntax: node,
      }),
      diagnostics: integer.diagnostics,
      type: integer.fact._tag === 'Available' ? integer.fact.type : undefined,
    })
  }

  if (node.kind === 'FloatingLiteralExpression') {
    const floating = analyzeFloating(source, node, expected)
    const fact = floating.fact
    const type =
      fact._tag === 'Available' ? availableExpressionType(fact.type) : unavailableExpressionType
    return Object.freeze({
      fact: Object.freeze({ _tag: 'Floating', floating: fact, type, syntax: node }),
      diagnostics: floating.diagnostics,
      type: fact._tag === 'Available' ? fact.type : undefined,
    })
  }

  if (node.kind === 'StaticTextLiteralExpression') {
    const token = directToken(node, 'TextLiteral') ?? directToken(node, 'ByteStringLiteral')
    const bytes =
      token === undefined ? undefined : Option.getOrUndefined(SourceFile.slice(source, token.span))
    const form = bytes === undefined ? undefined : LiteralForm.recognize(bytes)
    const result =
      bytes === undefined || form === undefined
        ? undefined
        : StaticText.decode(Array.from(bytes), form)
    const diagnostic =
      result?._tag === 'Invalid'
        ? Diagnostic.invalidStaticLiteral(result.detail, node.span)
        : undefined
    const data = result?._tag === 'Decoded' ? result.data : undefined
    const type =
      data === undefined
        ? unavailableExpressionType
        : availableExpressionType(data.kind === 'Text' ? Type.string : Type.slice('Shared', 'u8'))
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'StaticText',
        ...(data === undefined ? {} : { data }),
        type,
        syntax: node,
      }),
      diagnostics: Object.freeze(diagnostic === undefined ? [] : [diagnostic]),
      type: type._tag === 'Available' ? type.type : undefined,
    })
  }

  if (node.kind === 'CharacterLiteralExpression') {
    const token = directToken(node, 'CharLiteral')
    const bytes =
      token === undefined ? undefined : Option.getOrUndefined(SourceFile.slice(source, token.span))
    const form = bytes === undefined ? undefined : LiteralForm.recognize(bytes)
    const result =
      bytes === undefined || form === undefined
        ? undefined
        : StaticText.decodeScalar(Array.from(bytes), form)
    const diagnostic =
      result?._tag === 'Invalid'
        ? Diagnostic.invalidStaticLiteral(result.detail, node.span)
        : undefined
    const scalar = result?._tag === 'Scalar' ? result.value : undefined
    const type = scalar === undefined ? unavailableExpressionType : availableExpressionType('char')
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Character',
        ...(scalar === undefined ? {} : { value: scalar }),
        type,
        syntax: node,
      }),
      diagnostics: Object.freeze(diagnostic === undefined ? [] : [diagnostic]),
      type: type._tag === 'Available' ? type.type : undefined,
    })
  }

  if (node.kind === 'UnitExpression') {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Unit',
        type: availableExpressionType(Type.unit),
        syntax: node,
      }),
      diagnostics: Object.freeze([]),
      type: Type.unit,
    })
  }

  if (node.kind === 'IdentifierExpression') {
    const value = analyzeIdentifier(source, node, scope)
    if (
      value.fact._tag === 'Identifier' &&
      (value.fact.reference._tag === 'Resolved' ||
        value.fact.reference._tag === 'ResolvedBinding' ||
        value.fact.reference._tag === 'ResolvedPattern')
    )
      return value
    return (
      analyzeConstantReference(source, node, resolution) ??
      analyzeFunctionItem(source, node, declarations, resolution) ??
      value
    )
  }

  if (node.kind === 'RunExpression') {
    const subjectNode = node.children.find(isExpressionNode)
    const subject =
      subjectNode === undefined
        ? undefined
        : analyzeExpression(source, subjectNode, declarations, declaration, scope, resolution)
    if (subject === undefined) throw new RangeError('Run expression requires one effect subject')
    const effect =
      subject.type !== undefined && Type.isEffect(subject.type)
        ? subject.type
        : subject.type !== undefined &&
            Type.isRepresented(subject.type) &&
            Type.isEffect(subject.type.contract)
          ? subject.type.contract
          : undefined
    const type =
      effect !== undefined ? availableExpressionType(effect.success) : unavailableExpressionType
    const allowed =
      declaration.functionKind === 'Effect' ? declaration.failureRow.failures : Object.freeze([])
    const unhandled =
      (effect === undefined ? [] : Type.failureMembers(effect)).filter(
        (failure) => !allowed.some((candidate) => Type.equals(candidate, failure)),
      ) ?? []
    const symbolicFailuresUnhandled =
      effect !== undefined &&
      RowAlgebra.concretize(Type.failureRowPolicy(), effect.failureRow)._tag === 'Residual' &&
      !RowAlgebra.isKnownSubset(
        Type.failureRowPolicy(),
        effect.failureRow,
        declaration.functionKind === 'Effect'
          ? declaration.failureRow.row
          : RowAlgebra.concrete(Type.failureRowPolicy(), []),
      )
    const allowedRequirements =
      declaration.functionKind === 'Effect'
        ? declaration.requirementRow.requirements
        : Object.freeze<Type.Requirement[]>([])
    const unsatisfiedRequirements =
      (effect === undefined ? [] : Type.requirementMembers(effect)).filter(
        (requirement) =>
          !allowedRequirements.some(
            (allowed) =>
              Type.equals(allowed.capability, requirement.capability) &&
              allowed.role === requirement.role &&
              (allowed.access === 'Exclusive' || allowed.access === requirement.access),
          ),
      ) ?? []
    const symbolicRequirementsUnsatisfied =
      effect !== undefined &&
      RowAlgebra.concretize(Type.requirementRowPolicy(), effect.requirementRow)._tag ===
        'Residual' &&
      !RowAlgebra.isKnownSubset(
        Type.requirementRowPolicy(),
        effect.requirementRow,
        declaration.functionKind === 'Effect'
          ? declaration.requirementRow.row
          : RowAlgebra.concrete(Type.requirementRowPolicy(), []),
      )
    const diagnostics = [...subject.diagnostics]
    if (effect === undefined && subject.type !== undefined)
      diagnostics.push(Diagnostic.runNonEffect(Type.encode(subject.type), node.span))
    if (unhandled.length > 0 || symbolicFailuresUnhandled)
      diagnostics.push(
        Diagnostic.unhandledEffectFailures(
          unhandled.length > 0
            ? unhandled.map(Type.encode)
            : [
                RowAlgebra.encode(
                  Type.failureRowPolicy(),
                  effect?.failureRow ?? RowAlgebra.concrete(Type.failureRowPolicy(), []),
                  Type.encode,
                  Type.encode,
                  (member) => member.parameter.name,
                ),
              ],
          node.span,
        ),
      )
    if (unsatisfiedRequirements.length > 0 || symbolicRequirementsUnsatisfied)
      diagnostics.push(
        Diagnostic.unhandledEffectRequirements(
          unsatisfiedRequirements.length > 0
            ? unsatisfiedRequirements.map(
                (requirement) =>
                  `${requirement.access === 'Exclusive' ? '&mut ' : '&'}${Type.encode(requirement.capability)}${requirement.role === 'DefaultRole' ? '' : `@${requirement.role}`}`,
              )
            : [
                RowAlgebra.encode(
                  Type.requirementRowPolicy(),
                  effect?.requirementRow ?? RowAlgebra.concrete(Type.requirementRowPolicy(), []),
                  (requirement) =>
                    `${requirement.access === 'Exclusive' ? '&mut ' : '&'}${Type.encode(requirement.capability)}${requirement.role === 'DefaultRole' ? '' : `@${requirement.role}`}`,
                  Type.encode,
                  (member) =>
                    `${member.access === 'Exclusive' ? '&mut ' : '&'}${member.capability.name}${member.role === 'DefaultRole' ? '' : `@${member.role}`}`,
                ),
              ],
          node.span,
        ),
      )
    return Object.freeze({
      fact: Object.freeze({ _tag: 'Run', subject: subject.fact, type, syntax: node }),
      diagnostics: Object.freeze(diagnostics),
      type: type._tag === 'Available' ? type.type : undefined,
    })
  }

  if (node.kind === 'MoveExpression') {
    const move = analyzeMove(source, node, declarations, declaration, scope, resolution)
    return Object.freeze({
      fact: move.fact,
      diagnostics: move.diagnostics,
      type: move.type,
    })
  }

  if (node.kind === 'BorrowExpression') {
    return analyzeBorrow(
      source,
      node,
      declarations,
      declaration,
      scope,
      resolution,
      expected,
      borrowAllowed,
    )
  }

  if (node.kind === 'MatchExpression') {
    return analyzeMatch(
      source,
      node,
      declarations,
      declaration,
      scope,
      resolution,
      expected,
      borrowAllowed,
    )
  }

  if (node.kind === 'StructLiteralExpression') {
    return analyzeStructLiteral(source, node, declarations, declaration, scope, resolution)
  }

  if (node.kind === 'ArrayLiteralExpression') {
    return analyzeArrayLiteral(source, node, declarations, declaration, scope, resolution, expected)
  }

  if (node.kind === 'FieldProjectionExpression') {
    return (
      analyzeConstantReference(source, node, resolution) ??
      analyzeFunctionItem(source, node, declarations, resolution) ??
      analyzeProjection(source, node, declarations, declaration, scope, resolution)
    )
  }

  if (node.kind === 'IndexProjectionExpression') {
    return analyzeIndexProjection(source, node, declarations, declaration, scope, resolution)
  }

  if (node.kind === 'GroupedExpression') {
    return analyzeGroupedExpression(
      source,
      node,
      declarations,
      declaration,
      scope,
      resolution,
      expected,
      borrowAllowed,
    )
  }

  if (node.kind === 'PrefixExpression' || node.kind === 'InfixExpression') {
    return analyzeOperatorExpression(
      source,
      node,
      declarations,
      declaration,
      scope,
      resolution,
      expected,
    )
  }

  if (node.kind === 'PipelineExpression' || node.kind === 'CallExpression') {
    const operationTarget = node.kind === 'PipelineExpression' ? pipelineCallable(node) : node
    if (operationTarget !== undefined && isEffectResultTarget(source, operationTarget))
      return analyzeEffectResult(source, node, declarations, declaration, scope, resolution)
    if (node.kind === 'PipelineExpression')
      return analyzePipelineExpression(source, node, declarations, declaration, scope, resolution)
  }

  if (node.kind !== 'CallExpression') return undefined

  const callTypeArguments = analyzeCallTypeArguments(source, node, declaration, resolution)
  const argumentsResult = analyzeArguments(
    source,
    node,
    declarations,
    declaration,
    scope,
    resolution,
    callTypeArguments,
  )

  const calleeNode = callCallee(node)
  const calleeResult = analyzeExpression(
    source,
    calleeNode,
    declarations,
    declaration,
    scope,
    resolution,
  )
  const resolvedValueCallee =
    calleeResult?.fact._tag === 'Identifier' &&
    (calleeResult.fact.reference._tag === 'Resolved' ||
      calleeResult.fact.reference._tag === 'ResolvedBinding' ||
      calleeResult.fact.reference._tag === 'ResolvedPattern')
  if (
    calleeResult !== undefined &&
    calleeResult.fact._tag !== 'FunctionItem' &&
    ((calleeResult.type !== undefined &&
      (Type.isCallable(calleeResult.type) ||
        (Type.isRepresented(calleeResult.type) && Type.isCallable(calleeResult.type.contract)))) ||
      (calleeResult.type !== undefined && calleeNode.kind !== 'IdentifierExpression') ||
      resolvedValueCallee ||
      calleeResult.fact._tag === 'Constant')
  ) {
    return finishCallableApplication(
      node,
      calleeResult,
      argumentsResult,
      callTypeArguments,
      undefined,
      resolution,
      declaration,
    )
  }

  const identifiers = callReferenceTokens(node)
  if (identifiers.length === 2) {
    const qualifierToken = identifiers.at(0)
    const memberToken = identifiers.at(1)
    if (qualifierToken === undefined || memberToken === undefined)
      return analyzeBuiltinCall(
        source,
        node,
        argumentsResult,
        callTypeArguments,
        resolution,
        declaration,
      )
    const qualifier = spelling(source, qualifierToken)
    const member = spelling(source, memberToken)
    if (intrinsicOperationTarget(source, node)?.rule._tag === 'PlaceRule') {
      return analyzePlaceReplace(source, node, declarations, declaration, scope, resolution)
    }
    const qualifierLookup = NameResolution.lookup(resolution.scope, resolution.index, qualifier)
    if (qualifierLookup._tag === 'Intrinsic') {
      const libraryReference =
        qualifier === 'Effect'
          ? resolvedFunctionReference(source, node, declarations, resolution)
          : undefined
      if (libraryReference?._tag === 'Resolved')
        return finishDeclarationCall(
          node,
          libraryReference,
          argumentsResult,
          callTypeArguments,
          undefined,
          declaration,
          resolution,
        )
      return analyzeBuiltinCall(
        source,
        node,
        argumentsResult,
        callTypeArguments,
        resolution,
        declaration,
      )
    }
    if (
      qualifierLookup._tag === 'Resolved' &&
      qualifierLookup.declaration._tag === 'ServiceDeclaration'
    ) {
      const operation = serviceOperation(qualifierLookup.declaration, member)
      const diagnostic =
        operation === undefined
          ? Diagnostic.unknownActorOperation(qualifier, member, memberToken.span)
          : undefined
      const reference: CallReferenceFact =
        operation === undefined
          ? Object.freeze({
              _tag: 'Missing',
              spelling: `${qualifier}.${member}`,
              token: memberToken,
              ...(diagnostic === undefined ? {} : { cause: Diagnostic.identity(diagnostic) }),
            })
          : Object.freeze({
              _tag: 'ResolvedServiceOperation',
              spelling: `${qualifier}.${member}`,
              token: memberToken,
              service: qualifierLookup.declaration,
              operation,
            })
      return finishDeclarationCall(
        node,
        reference,
        argumentsResult,
        callTypeArguments,
        diagnostic,
        declaration,
        resolution,
      )
    }
    if (
      qualifierLookup._tag === 'Resolved' &&
      qualifierLookup.declaration._tag === 'InterfaceDeclaration'
    ) {
      const bound = boundOperationReference(
        declaration,
        qualifierLookup.declaration,
        qualifier,
        member,
        memberToken,
      )
      if (bound?._tag === 'AmbiguousBound') {
        const ambiguous = Diagnostic.ambiguousBoundOperation(
          `${qualifier}.${member}`,
          bound.parameters,
          memberToken.span,
        )
        return finishDeclarationCall(
          node,
          Object.freeze({
            _tag: 'Missing',
            spelling: `${qualifier}.${member}`,
            token: memberToken,
            cause: Diagnostic.identity(ambiguous),
          }),
          argumentsResult,
          callTypeArguments,
          ambiguous,
          declaration,
          resolution,
        )
      }
      if (bound !== undefined)
        return finishBoundOperationCall(
          node,
          bound.reference,
          argumentsResult,
          callTypeArguments,
          resolution,
        )
    }
    if (
      qualifierLookup._tag === 'Resolved' &&
      (qualifierLookup.declaration._tag === 'StructDeclaration' ||
        qualifierLookup.declaration._tag === 'InterfaceDeclaration') &&
      qualifierLookup.declaration.canonical._tag === 'Canonical'
    ) {
      const actorModule = qualifierLookup.declaration.canonical.id.module
      const memberLookup = DeclarationFacts.lookup(resolution.index, actorModule, member)
      const candidate = memberLookup._tag === 'Resolved' ? memberLookup.declaration : undefined
      const diagnostic =
        candidate === undefined
          ? Diagnostic.unknownActorOperation(qualifier, member, memberToken.span)
          : candidate.visibility === 'Private'
            ? Diagnostic.inaccessibleImportedMember(actorModule, member, memberToken.span)
            : undefined
      const reference: CallReferenceFact =
        candidate !== undefined && candidate.visibility === 'Public'
          ? Object.freeze({
              _tag: 'Resolved',
              spelling: `${qualifier}.${member}`,
              token: memberToken,
              declaration: candidate,
            })
          : Object.freeze({
              _tag: 'Missing',
              spelling: `${qualifier}.${member}`,
              token: memberToken,
              ...(diagnostic === undefined ? {} : { cause: Diagnostic.identity(diagnostic) }),
            })
      return finishDeclarationCall(
        node,
        reference,
        argumentsResult,
        callTypeArguments,
        diagnostic,
        declaration,
        resolution,
      )
    }
    if (qualifierLookup._tag === 'Namespace') {
      const memberLookup = DeclarationFacts.lookup(resolution.index, qualifierLookup.module, member)
      const candidate = memberLookup._tag === 'Resolved' ? memberLookup.declaration : undefined
      const diagnostic =
        candidate === undefined
          ? Diagnostic.unknownImportedMember(qualifierLookup.module, member, memberToken.span)
          : candidate.visibility === 'Private'
            ? Diagnostic.inaccessibleImportedMember(
                qualifierLookup.module,
                member,
                memberToken.span,
              )
            : undefined
      const reference: CallReferenceFact =
        candidate !== undefined && candidate.visibility === 'Public'
          ? Object.freeze({
              _tag: 'Resolved',
              spelling: `${qualifier}.${member}`,
              token: memberToken,
              declaration: candidate,
            })
          : Object.freeze({
              _tag: 'Missing',
              spelling: `${qualifier}.${member}`,
              token: memberToken,
              ...(diagnostic === undefined ? {} : { cause: Diagnostic.identity(diagnostic) }),
            })
      return finishDeclarationCall(
        node,
        reference,
        argumentsResult,
        callTypeArguments,
        diagnostic,
        declaration,
        resolution,
      )
    }
    const diagnostic =
      qualifierLookup._tag === 'Missing' || qualifierLookup._tag === 'Resolved'
        ? Diagnostic.unknownActor(qualifier, qualifierToken.span)
        : undefined
    const inheritedCause =
      qualifierLookup._tag === 'Unavailable'
        ? qualifierLookup.cause
        : qualifierLookup._tag === 'Conflict'
          ? qualifierLookup.conflict.cause
          : undefined
    const reference: CallReferenceFact = Object.freeze({
      _tag: 'Missing',
      spelling: `${qualifier}.${member}`,
      token: qualifierToken,
      ...(diagnostic !== undefined
        ? { cause: Diagnostic.identity(diagnostic) }
        : inheritedCause === undefined
          ? {}
          : { cause: inheritedCause }),
    })
    return finishDeclarationCall(
      node,
      reference,
      argumentsResult,
      callTypeArguments,
      diagnostic,
      declaration,
      resolution,
    )
  }

  const token = identifiers.at(0)
  if (token === undefined) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Call',
        reference: Object.freeze({
          _tag: 'Unavailable',
          syntax: unavailableSyntax(callCallee(node), 'Identifier'),
        }),
        path: referencePath(node),
        typeArguments: callTypeArguments.facts,
        arguments: argumentsResult.facts,
        mappings: Object.freeze([]),
        contract: Object.freeze({
          _tag: 'Unavailable',
          reason: Object.freeze({
            _tag: 'UnavailableCallSyntax',
            syntax: node,
          }),
        }),
        type: unavailableExpressionType,
        syntax: node,
      }),
      diagnostics: argumentsResult.diagnostics,
      type: undefined,
    })
  }

  const tokenSpelling = spelling(source, token)
  const resolvedLookup = NameResolution.lookup(resolution.scope, resolution.index, tokenSpelling)
  const localLookup = lookupDeclaration(declarations, tokenSpelling)
  const lookup: DeclarationFacts.DeclarationLookup =
    resolvedLookup._tag === 'Conflict'
      ? Object.freeze({
          _tag: 'Ambiguous',
          spelling: tokenSpelling,
          declarations: Object.freeze(
            resolvedLookup.conflict.bindings.flatMap((binding) => {
              if (binding._tag !== 'LocalDeclaration' && binding._tag !== 'ImportedMember')
                return []
              const declaration = DeclarationFacts.byCanonical(
                resolution.index,
                binding.declaration,
              )
              return declaration?._tag === 'FunctionDeclaration' ? [declaration] : []
            }),
          ),
        })
      : localLookup._tag === 'Ambiguous'
        ? localLookup
        : resolvedLookup._tag === 'Resolved' &&
            resolvedLookup.declaration._tag === 'FunctionDeclaration'
          ? Object.freeze({
              _tag: 'Resolved',
              spelling: tokenSpelling,
              declaration: resolvedLookup.declaration,
            })
          : resolvedLookup._tag === 'Missing'
            ? localLookup
            : Object.freeze({ _tag: 'Missing', spelling: tokenSpelling })
  const missingDiagnostic =
    lookup._tag === 'Missing' && resolvedLookup._tag !== 'Unavailable'
      ? Diagnostic.unknownFunction(tokenSpelling, token.span)
      : undefined
  const reference: CallReferenceFact =
    lookup._tag === 'Resolved'
      ? Object.freeze({
          _tag: 'Resolved',
          spelling: tokenSpelling,
          token,
          declaration: lookup.declaration,
        })
      : lookup._tag === 'Ambiguous'
        ? Object.freeze({
            _tag: 'Ambiguous',
            spelling: tokenSpelling,
            token,
            declarations: lookup.declarations,
            ...(resolvedLookup._tag === 'Conflict' ? { cause: resolvedLookup.conflict.cause } : {}),
          })
        : Object.freeze({
            _tag: 'Missing',
            spelling: tokenSpelling,
            token,
            ...(missingDiagnostic !== undefined
              ? { cause: Diagnostic.identity(missingDiagnostic) }
              : resolvedLookup._tag === 'Unavailable' && resolvedLookup.cause !== undefined
                ? { cause: resolvedLookup.cause }
                : {}),
          })
  if (
    reference._tag === 'Resolved' &&
    isSectionArity(reference.declaration.parameters.length, argumentsResult.facts.length)
  ) {
    return finishCallableSection(
      node,
      reference,
      argumentsResult,
      callTypeArguments,
      resolution,
      declaration,
    )
  }
  const callContract = analyzeCallContract(
    node,
    reference,
    argumentsResult.facts,
    hasAvailableCallSyntax(node),
    callTypeArguments,
    resolution,
    declaration,
  )
  const constraintDiagnostics = interfaceConstraintDiagnostics(
    reference,
    callContract,
    resolution.index,
    declaration,
    node.span,
  )
  const unsafeDiagnostic = unsafeCallDiagnostic(
    reference._tag === 'Resolved' && reference.declaration.unsafe,
    reference.spelling,
    node,
    resolution,
  )
  const syntaxAvailable = hasAvailableCallSyntax(node)
  const expressionType =
    syntaxAvailable &&
    reference._tag === 'Resolved' &&
    reference.declaration.returnType._tag === 'Resolved' &&
    callContract.fact._tag === 'Compatible' &&
    constraintDiagnostics.length === 0 &&
    unsafeDiagnostic === undefined
      ? availableExpressionType(
          (() => {
            const substitution =
              callContract.fact._tag === 'Compatible'
                ? callContract.fact.substitution
                : new Map<string, Type.GenericArgument>()
            const success = Type.substitute(reference.declaration.returnType.type, substitution)
            if (reference.declaration.functionKind !== 'Effect')
              return Type.isEffect(success)
                ? Type.effectWithRows(
                    success.success,
                    success.failureRow,
                    effectCaptureAccess(
                      argumentsResult.facts,
                      resolution.index,
                      copyAssumptionsOf(declaration),
                    ),
                    success.requirementRow,
                  )
                : success
            return Type.effectWithRows(
              success,
              Type.substituteFailureRow(reference.declaration.failureRow.row, substitution),
              effectCaptureAccess(
                argumentsResult.facts,
                resolution.index,
                copyAssumptionsOf(declaration),
              ),
              Type.substituteRequirementsRow(
                reference.declaration.requirementRow.row,
                substitution,
              ),
            )
          })(),
        )
      : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Call',
      reference,
      path: referencePath(node),
      typeArguments: callTypeArguments.facts,
      arguments: argumentsResult.facts,
      mappings: callContract.mappings,
      contract: callContract.fact,
      type: expressionType,
      syntax: node,
    }),
    diagnostics: Object.freeze([
      ...(missingDiagnostic === undefined ? [] : [missingDiagnostic]),
      ...argumentsResult.diagnostics,
      ...callTypeArguments.diagnostics,
      ...callContract.diagnostics,
      ...constraintDiagnostics,
      ...(unsafeDiagnostic === undefined ? [] : [unsafeDiagnostic]),
    ]),
    type: expressionType._tag === 'Available' ? expressionType.type : undefined,
  })
}

export const finishDeclarationCall = (
  node: SyntaxTree.Node,
  reference: CallReferenceFact,
  argumentsResult: ArgumentsResult,
  callTypeArguments: CallTypeArgumentsResult,
  diagnostic: Diagnostic.Diagnostic | undefined,
  caller: DeclarationFact,
  resolution: ResolutionContext,
): ExpressionResult => {
  if (
    reference._tag === 'Resolved' &&
    isSectionArity(reference.declaration.parameters.length, argumentsResult.facts.length)
  ) {
    const section = finishCallableSection(
      node,
      reference,
      argumentsResult,
      callTypeArguments,
      resolution,
      caller,
    )
    return diagnostic === undefined
      ? section
      : Object.freeze({
          ...section,
          diagnostics: Object.freeze([diagnostic, ...section.diagnostics]),
        })
  }
  const callContract = analyzeCallContract(
    node,
    reference,
    argumentsResult.facts,
    hasAvailableCallSyntax(node),
    callTypeArguments,
    resolution,
    caller,
  )
  const constraintDiagnostics = interfaceConstraintDiagnostics(
    reference,
    callContract,
    resolution.index,
    caller,
    node.span,
  )
  const callable = sourceCallable(reference)
  const unsafeDiagnostic = unsafeCallDiagnostic(
    callable?.unsafe === true,
    'spelling' in reference ? reference.spelling : 'callable',
    node,
    resolution,
  )
  const expressionType =
    hasAvailableCallSyntax(node) &&
    callable !== undefined &&
    callable.returnType._tag === 'Resolved' &&
    callContract.fact._tag === 'Compatible' &&
    constraintDiagnostics.length === 0 &&
    unsafeDiagnostic === undefined
      ? availableExpressionType(
          (() => {
            const substitution =
              callContract.fact._tag === 'Compatible'
                ? callContract.fact.substitution
                : new Map<string, Type.GenericArgument>()
            const success = Type.substitute(callable.returnType.type, substitution)
            if (callable.functionKind !== 'Effect')
              return Type.isEffect(success)
                ? Type.effectWithRows(
                    success.success,
                    success.failureRow,
                    effectCaptureAccess(
                      argumentsResult.facts,
                      resolution.index,
                      copyAssumptionsOf(caller),
                    ),
                    success.requirementRow,
                  )
                : success
            return Type.effectWithRows(
              success,
              Type.substituteFailureRow(callable.failureRow.row, substitution),
              effectCaptureAccess(
                argumentsResult.facts,
                resolution.index,
                copyAssumptionsOf(caller),
              ),
              Type.substituteRequirementsRow(callable.requirementRow.row, substitution),
            )
          })(),
        )
      : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Call',
      reference,
      path: referencePath(node),
      typeArguments: callTypeArguments.facts,
      arguments: argumentsResult.facts,
      mappings: callContract.mappings,
      contract: callContract.fact,
      type: expressionType,
      syntax: node,
    }),
    diagnostics: Object.freeze([
      ...(diagnostic === undefined ? [] : [diagnostic]),
      ...argumentsResult.diagnostics,
      ...callTypeArguments.diagnostics,
      ...callContract.diagnostics,
      ...constraintDiagnostics,
      ...(unsafeDiagnostic === undefined ? [] : [unsafeDiagnostic]),
    ]),
    type: expressionType._tag === 'Available' ? expressionType.type : undefined,
  })
}

/**
 * Finishes one call to an operation the enclosing declaration's bound declares.
 *
 * The contract is the interface's own, over the bounded parameter, so the call checks exactly like
 * a compiler-known operation's. It carries no type arguments of its own: the only type the call
 * varies over is the bounded parameter, and that one is supplied by the specialization of the
 * declaration this body belongs to.
 */
export const finishBoundOperationCall = (
  node: SyntaxTree.Node,
  reference: Extract<CallReferenceFact, { readonly _tag: 'ResolvedBoundOperation' }>,
  argumentsResult: ArgumentsResult,
  callTypeArguments: CallTypeArgumentsResult,
  resolution: ResolutionContext,
): ExpressionResult => {
  const typeArgumentDiagnostic =
    callTypeArguments.explicit && callTypeArguments.facts.length > 0
      ? Diagnostic.typeArgumentArity(
          reference.spelling,
          0,
          callTypeArguments.facts.length,
          node.span,
        )
      : undefined
  const callContract = analyzeCallContract(node, reference, argumentsResult.facts)
  const unsafeDiagnostic = unsafeCallDiagnostic(
    reference.interfaceContract.unsafe,
    reference.spelling,
    node,
    resolution,
  )
  const expressionType =
    hasAvailableCallSyntax(node) &&
    typeArgumentDiagnostic === undefined &&
    callContract.fact._tag === 'Compatible' &&
    unsafeDiagnostic === undefined
      ? availableExpressionType(reference.result)
      : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Call',
      reference,
      path: referencePath(node),
      typeArguments: callTypeArguments.facts,
      arguments: argumentsResult.facts,
      mappings: callContract.mappings,
      contract: callContract.fact,
      ...(reference.interfaceContract.functionKind === 'Effect'
        ? { witnessEffectSite: executableSite('EffectSiteId', resolution, node) }
        : {}),
      type: expressionType,
      syntax: node,
    }),
    diagnostics: Object.freeze([
      ...(typeArgumentDiagnostic === undefined ? [] : [typeArgumentDiagnostic]),
      ...argumentsResult.diagnostics,
      ...callTypeArguments.diagnostics,
      ...callContract.diagnostics,
      ...(unsafeDiagnostic === undefined ? [] : [unsafeDiagnostic]),
    ]),
    type: expressionType._tag === 'Available' ? expressionType.type : undefined,
  })
}

export const statementExpressionNode = (statement: SyntaxTree.Node): SyntaxTree.Node => {
  const expression = statement.children.find((element): element is SyntaxTree.Node =>
    isExpressionNode(element),
  )
  if (expression === undefined) {
    throw new RangeError('Semantic analysis expected a statement expression')
  }
  return expression
}

export const compareDiagnostics = (
  left: Diagnostic.Diagnostic,
  right: Diagnostic.Diagnostic,
): number =>
  left.span.start - right.span.start ||
  left.span.end - right.span.end ||
  (left.code < right.code ? -1 : left.code > right.code ? 1 : 0)

export interface FunctionAnalysis {
  readonly fact: FunctionFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

export const bindingName = (
  source: SourceFile.SourceFile,
  statement: SyntaxTree.Node,
): DeclarationFacts.DeclaredName => {
  const token = directToken(statement, 'Identifier')
  return token === undefined
    ? Object.freeze({
        _tag: 'Unavailable' as const,
        syntax: unavailableSyntax(statement, 'Identifier'),
      })
    : Object.freeze({ _tag: 'Present' as const, spelling: spelling(source, token), token })
}

export const scopeSpanFor = (
  scope: Scope,
  spellingText: string,
): SourceSpan.SourceSpan | undefined => {
  const binding = scope.bindings.findLast(
    (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === spellingText,
  )
  if (binding?.name._tag === 'Present') return binding.name.token.span
  const patternBinding = scope.patternBindings.findLast(
    (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === spellingText,
  )
  if (patternBinding?.name._tag === 'Present') return patternBinding.name.token.span
  const parameter = scope.parameters.find(
    (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === spellingText,
  )
  return parameter?.name._tag === 'Present' ? parameter.name.token.span : undefined
}

export interface BodyContext {
  readonly source: SourceFile.SourceFile
  readonly declaration: DeclarationFact
  readonly declarations: ReadonlyArray<DeclarationFact>
  readonly bindings: Array<BindingDeclarationFact>
  readonly diagnostics: Array<Diagnostic.Diagnostic>
  readonly regions: Array<Hir.RegionId>
  readonly loops: Array<Hir.LoopId>
  readonly resolution: ResolutionContext
  readonly nextBindingOrdinal: { value: number }
  readonly regionBase?: number
  readonly effectBlock?: true
}

export interface ResolutionContext {
  readonly scope: NameResolution.ModuleScope
  readonly index: DeclarationIndex.Index
  readonly unsafeSpans?: ReadonlyArray<SourceSpan.SourceSpan>
  /** Exact direct-call spans acknowledged by the expression form `unsafe call(...)`. */
  readonly unsafeCallSpans?: ReadonlyArray<SourceSpan.SourceSpan>
  readonly nextBindingOrdinal?: { value: number }
  readonly executableFunction?: DeclarationId
  readonly executableOwner?: DeclarationFacts.CanonicalId
  readonly executableSites?: ReadonlyMap<SyntaxTree.Node, number>
}

export const unsafeCallAuthorized = (
  resolution: ResolutionContext | undefined,
  call: SyntaxTree.Node,
): boolean =>
  resolution !== undefined &&
  ((resolution.unsafeSpans ?? []).some(
    (span) =>
      span.sourceId === call.span.sourceId &&
      span.start <= call.span.start &&
      span.end >= call.span.end,
  ) ||
    (resolution.unsafeCallSpans ?? []).some(
      (span) =>
        span.sourceId === call.span.sourceId &&
        span.start === call.span.start &&
        span.end === call.span.end,
    ))
