import * as MachineFunction from './MachineFunction.js'
import * as DeclarationProperty from './DeclarationProperty.js'
import * as NativeRequirement from './NativeRequirement.js'
import * as ForeignContract from './ForeignContract.js'
import * as Option from 'effect/Option'
import * as AggregateIdentity from './AggregateIdentity.js'
import type {
  ArrayLengthFact,
  BoundFact,
  CanonicalEnumMemberId,
  CanonicalId,
  CanonicalState,
  CanonicalUnionVariantId,
  ConformanceFact,
  ConformanceRequirementFact,
  ConstantFact,
  ConstantLiteralFact,
  ConstraintFact,
  DeclarationFact,
  InherentImplFact,
  DeclarationId,
  DeclaredName,
  DeclaredTypeFact,
  EnumDiscriminantFact,
  EnumFact,
  EnumMemberFact,
  EnumMemberId,
  EnumRepresentationFact,
  FailureRowFact,
  FieldFact,
  FieldId,
  FieldOwnerId,
  FieldState,
  ForeignStaticFact,
  FunctionBodyTemplate,
  InterfaceFact,
  MemberFact,
  ModuleHeaders,
  OpaqueResultFact,
  ParameterFact,
  RequirementRoleFact,
  RequirementRowFact,
  ReturnTypeFact,
  RowExpressionFact,
  ServiceFact,
  ServiceOperationFact,
  ServiceOperationId,
  ServiceOperationState,
  StaticExpressionTemplate,
  StructFact,
  TypeParameterFact,
  TypePathFact,
  TypeResolution,
  UnionFact,
  UnionVariantFact,
  UnionVariantId,
} from './DeclarationFacts.js'
import {
  enumValueOperation,
  interfaceOperationContracts,
  presentParameterEntries,
  requirementRoleIdentity,
} from './DeclarationFacts.js'
import * as DeclarationLifetime from './DeclarationLifetime.js'
import * as Lifetime from './Lifetime.js'
import * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as DigitSeparator from './internal/DigitSeparator.js'
import * as DurationLiteral from './internal/DurationLiteral.js'
import * as ForeignSymbol from './ForeignSymbol.js'
import * as ImportPath from './ImportPath.js'
import * as IntegerLiteral from './internal/IntegerLiteral.js'
import * as LiteralForm from './LiteralForm.js'
import type * as ModuleClosure from './ModuleClosure.js'
import * as Operator from './Operator.js'
import * as RequirementRow from './RequirementRow.js'
import * as RowAlgebra from './RowAlgebra.js'
import * as Scalar from './Scalar.js'
import * as SourceFile from './SourceFile.js'
import * as SourceSpan from './SourceSpan.js'
import * as StaticText from './StaticText.js'
import type * as SyntaxFile from './SyntaxFile.js'
import * as SyntaxTree from './SyntaxTree.js'
import * as Token from './Token.js'
import * as Type from './Type.js'

export const spelling = (source: SourceFile.SourceFile, token: Token.Token): string =>
  Option.getOrThrowWith(
    SourceFile.spelling(source, token.span),
    () => new RangeError(`Header token span does not belong to source ${source.id}`),
  )

const templateElement = (
  source: SourceFile.SourceFile,
  element: SyntaxTree.Element,
): ReadonlyArray<unknown> | undefined => {
  if (SyntaxTree.isNode(element))
    return Object.freeze([
      'Node',
      element.kind,
      Object.freeze(
        element.children.flatMap((child) => {
          const retained = templateElement(source, child)
          return retained === undefined ? [] : [retained]
        }),
      ),
    ])
  if (SyntaxTree.isToken(element)) {
    if (
      element.kind === 'Whitespace' ||
      element.kind === 'LineComment' ||
      element.kind === 'DocComment' ||
      element.kind === 'ModuleDocComment'
    )
      return undefined
    return Object.freeze(['Token', element.kind, spelling(source, element)])
  }
  return Object.freeze(['Missing', element.expected])
}

const bodyTemplate = (
  source: SourceFile.SourceFile,
  declaration: SyntaxTree.Node,
): FunctionBodyTemplate | undefined => {
  const block = SyntaxTree.directNode(declaration, 'Block')
  if (block === undefined) return undefined
  const tokens = SyntaxTree.tokens(declaration)
  const requiresStaticEvaluation = tokens.some(
    (token) => token.kind === 'StaticKeyword' || token.kind === 'CompileErrorKeyword',
  )
  if (!requiresStaticEvaluation) return undefined
  return Object.freeze({
    _tag: 'FunctionBodyTemplate',
    syntax: block,
    canonical: JSON.stringify(templateElement(source, block)),
  })
}

const staticExpressionTemplate = (
  source: SourceFile.SourceFile,
  syntax: SyntaxTree.Node,
): StaticExpressionTemplate =>
  Object.freeze({
    _tag: 'StaticExpressionTemplate',
    syntax,
    canonical: JSON.stringify(templateElement(source, syntax)),
  })

const retainedTypePath = (
  source: SourceFile.SourceFile,
  syntax: SyntaxTree.Node,
): TypePathFact | undefined => {
  const segments = SyntaxTree.tokens(syntax)
    .filter((token) => token.kind === 'Identifier')
    .map((token) => Object.freeze({ spelling: spelling(source, token), token }))
  return segments.length === 0
    ? undefined
    : Object.freeze({
        _tag: 'TypePath',
        spelling: segments.map((segment) => segment.spelling).join('.'),
        segments: Object.freeze(segments),
        syntax,
      })
}

export const collectedRequirementRole = (
  source: SourceFile.SourceFile,
  requirement: SyntaxTree.Node,
): RequirementRoleFact => {
  const at = SyntaxTree.directToken(requirement, 'Identifier')
  const roleSyntax =
    at === undefined ? undefined : SyntaxTree.directNodes(requirement, 'TypePath').at(-1)
  const path = roleSyntax?.kind === 'TypePath' ? retainedTypePath(source, roleSyntax) : undefined
  return path === undefined
    ? Object.freeze({ _tag: 'DefaultRole' })
    : Object.freeze({ _tag: 'UnresolvedRole', path })
}

export const childNode = (parent: SyntaxTree.Node, kind: SyntaxTree.NodeKind): SyntaxTree.Node => {
  const child = SyntaxTree.directNode(parent, kind)
  if (child === undefined)
    throw new RangeError(`Header collection expected ${kind} below ${parent.kind}`)
  return child
}

export const isDeclaredTypeNode = (element: SyntaxTree.Element): element is SyntaxTree.Node =>
  SyntaxTree.isNode(element) &&
  (element.kind === 'TypePath' ||
    element.kind === 'AppliedType' ||
    element.kind === 'FixedArrayType' ||
    element.kind === 'SliceType' ||
    element.kind === 'ReferenceType' ||
    element.kind === 'PointerType' ||
    element.kind === 'CallableType' ||
    element.kind === 'ForeignFunctionType' ||
    element.kind === 'UnitType' ||
    element.kind === 'ParenthesizedType' ||
    element.kind === 'ExactRepresentationType' ||
    element.kind === 'OpaqueResultType' ||
    element.kind === 'UnionType')

export const declaredTypeNode = (parent: SyntaxTree.Node): SyntaxTree.Node => {
  const child = parent.children.find((element): element is SyntaxTree.Node =>
    isDeclaredTypeNode(element),
  )
  if (child === undefined) throw new RangeError(`Header collection expected a declared type`)
  return child
}

export const presentName = (source: SourceFile.SourceFile, node: SyntaxTree.Node): DeclaredName => {
  const token = SyntaxTree.directToken(node, 'Identifier')
  return token === undefined
    ? Object.freeze({
        _tag: 'Unavailable',
        syntax: SyntaxTree.unavailableChild(node, 'Identifier'),
      })
    : Object.freeze({ _tag: 'Present', spelling: spelling(source, token), token })
}

export const constantLiteral = (
  source: SourceFile.SourceFile,
  initializer: SyntaxTree.Node,
): ConstantLiteralFact => {
  if (initializer.kind === 'BooleanLiteralExpression') {
    const token =
      SyntaxTree.directToken(initializer, 'TrueKeyword') ??
      SyntaxTree.directToken(initializer, 'FalseKeyword')
    return token === undefined
      ? Object.freeze({ _tag: 'Unavailable', syntax: initializer })
      : Object.freeze({ _tag: 'BooleanLiteral', value: token.kind === 'TrueKeyword', token })
  }
  if (initializer.kind === 'CharacterLiteralExpression') {
    const token = SyntaxTree.directToken(initializer, 'CharLiteral')
    if (token === undefined) return Object.freeze({ _tag: 'Unavailable', syntax: initializer })
    const bytes = Option.getOrUndefined(SourceFile.slice(source, token.span))
    const form = bytes === undefined ? undefined : LiteralForm.recognize(bytes)
    if (bytes === undefined || form === undefined)
      return Object.freeze({ _tag: 'Unavailable', syntax: initializer })
    const decoded = StaticText.decodeScalar(Array.from(bytes), form)
    return decoded._tag === 'Scalar'
      ? Object.freeze({ _tag: 'CharacterLiteral', value: decoded.value, token })
      : Object.freeze({ _tag: 'Malformed', detail: decoded.detail, syntax: initializer })
  }
  if (initializer.kind === 'IntegerLiteralExpression') {
    const token = SyntaxTree.directToken(initializer, 'DecimalInteger')
    if (token === undefined) return Object.freeze({ _tag: 'Unavailable', syntax: initializer })
    const digits = spelling(source, token)
    const negative = SyntaxTree.directToken(initializer, 'Minus') !== undefined
    const magnitude = IntegerLiteral.magnitude(digits)
    return Object.freeze({
      _tag: 'IntegerLiteral',
      value: negative ? -magnitude : magnitude,
      spelling: `${negative ? '-' : ''}${digits}`,
      token,
    })
  }
  if (initializer.kind === 'DurationLiteralExpression') {
    const token = SyntaxTree.directToken(initializer, 'DurationLiteral')
    if (token === undefined) return Object.freeze({ _tag: 'Unavailable', syntax: initializer })
    const bytes = Option.getOrUndefined(SourceFile.slice(source, token.span))
    if (bytes === undefined) return Object.freeze({ _tag: 'Unavailable', syntax: initializer })
    const parsed = DurationLiteral.parse(bytes)
    if (parsed._tag === 'Invalid')
      return Object.freeze({ _tag: 'Unavailable', syntax: initializer })
    return Object.freeze({
      _tag: 'DurationLiteral',
      value: parsed.nanoseconds,
      spelling: spelling(source, token),
      token,
    })
  }
  if (initializer.kind === 'FloatingLiteralExpression') {
    const token = SyntaxTree.directToken(initializer, 'DecimalFloat')
    if (token === undefined) return Object.freeze({ _tag: 'Unavailable', syntax: initializer })
    const literal = DigitSeparator.strip(spelling(source, token))
    return Object.freeze({
      _tag: 'FloatingLiteral',
      spelling: `${SyntaxTree.directToken(initializer, 'Minus') === undefined ? '' : '-'}${literal}`,
      token,
    })
  }
  if (initializer.kind === 'StaticTextLiteralExpression') {
    const token =
      SyntaxTree.directToken(initializer, 'TextLiteral') ??
      SyntaxTree.directToken(initializer, 'ByteStringLiteral')
    if (token === undefined) return Object.freeze({ _tag: 'Unavailable', syntax: initializer })
    const bytes = Option.getOrUndefined(SourceFile.slice(source, token.span))
    const form = bytes === undefined ? undefined : LiteralForm.recognize(bytes)
    if (bytes === undefined || form === undefined)
      return Object.freeze({ _tag: 'Unavailable', syntax: initializer })
    // The header decodes once so every reference — in this module or an importing one — shares
    // the exact bytes the equivalent `let` binding would produce.
    const decoded = StaticText.decode(Array.from(bytes), form)
    return decoded._tag === 'Decoded'
      ? Object.freeze({ _tag: 'StringLiteral', data: decoded.data, token })
      : Object.freeze({ _tag: 'Malformed', detail: decoded.detail, syntax: initializer })
  }
  return Object.freeze({ _tag: 'Unavailable', syntax: initializer })
}

interface AppliedRequirement {
  readonly capability: TypeResolution
  readonly role: ReturnType<typeof collectedRequirementRole>
  readonly access: 'Shared' | 'Exclusive'
  readonly syntax: SyntaxTree.Node
}

interface AppliedRows {
  readonly failureRowSyntax: SyntaxTree.Node | undefined
  readonly failures: ReadonlyArray<TypeResolution>
  readonly requirementRowSyntax: SyntaxTree.Node | undefined
  readonly requirements: ReadonlyArray<AppliedRequirement>
  readonly requirementParameters: ReadonlyArray<Type.Parameter>
  readonly rowParameterComponents: ReadonlyArray<DeclaredTypeFact>
  /** The row as one expression, present only when the row subtracts (`Without<R, K>`). */
  readonly requirementExpression: RowExpressionFact | undefined
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

/** Analyzes the failure and requirement arguments shared by Effect and nominal applications. */
const analyzeAppliedRows = (
  source: SourceFile.SourceFile,
  list: SyntaxTree.Node,
  typeParameters: ReadonlyMap<string, Type.Parameter>,
  lifetimeContext?: DeclarationLifetime.Context,
): AppliedRows => {
  const failureRowSyntax = SyntaxTree.directNode(list, 'FailureRow')
  const failureType = failureRowSyntax?.children.find(isDeclaredTypeNode)
  let failureNodes: SyntaxTree.Node[]
  if (failureType?.kind === 'UnionType') {
    failureNodes = failureType.children.filter(isDeclaredTypeNode)
  } else if (failureType === undefined) {
    failureNodes = []
  } else {
    failureNodes = [failureType]
  }
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const failures = failureNodes.flatMap((member): ReadonlyArray<TypeResolution> => {
    const parameter = parameterAtTypePath(source, member, typeParameters)
    if (parameter?.kind !== 'RequirementRow')
      return [analyzeDeclaredType(source, member, typeParameters, false, lifetimeContext)]
    const token = SyntaxTree.directToken(member, 'Identifier')
    if (token !== undefined)
      diagnostics.push(
        Diagnostic.genericParameterKindMismatch(
          spelling(source, token),
          'Value',
          parameter.kind,
          token.span,
        ),
      )
    return []
  })
  const requirementRowSyntax = SyntaxTree.directNode(list, 'RequirementRow')
  const requirements =
    requirementRowSyntax?.children
      .filter(
        (element): element is SyntaxTree.Node =>
          SyntaxTree.isNode(element) && element.kind === 'Requirement',
      )
      .map((requirement) => {
        const capability = requirement.children.find(isDeclaredTypeNode)
        return Object.freeze({
          capability:
            capability === undefined
              ? Object.freeze({
                  fact: Object.freeze({ _tag: 'Unavailable' as const, syntax: requirement }),
                  diagnostics: Object.freeze<ReadonlyArray<Diagnostic.Diagnostic>>([]),
                })
              : analyzeDeclaredType(source, capability, typeParameters, false, lifetimeContext),
          role: collectedRequirementRole(source, requirement),
          access:
            SyntaxTree.directToken(requirement, 'MutKeyword') === undefined
              ? ('Shared' as const)
              : ('Exclusive' as const),
          syntax: requirement,
        })
      }) ?? []
  const parameterPaths =
    requirementRowSyntax?.children.filter(
      (element): element is SyntaxTree.Node =>
        SyntaxTree.isNode(element) && element.kind === 'TypePath',
    ) ?? []
  const requirementParameters = parameterPaths.flatMap((path): ReadonlyArray<Type.Parameter> => {
    const token = SyntaxTree.directToken(path, 'Identifier')
    const parameter = parameterAtTypePath(source, path, typeParameters)
    if (parameter?.kind === 'RequirementRow') return [parameter]
    if (token !== undefined)
      diagnostics.push(
        parameter === undefined
          ? Diagnostic.unknownType(spelling(source, token), token.span)
          : Diagnostic.genericParameterKindMismatch(
              spelling(source, token),
              'RequirementRow',
              parameter.kind,
              token.span,
            ),
      )
    return []
  })
  const rowParameterComponents = parameterPaths.flatMap((path): ReadonlyArray<DeclaredTypeFact> => {
    const token = SyntaxTree.directToken(path, 'Identifier')
    const parameter = parameterAtTypePath(source, path, typeParameters)
    return token === undefined || parameter?.kind !== 'RequirementRow'
      ? []
      : [
          Object.freeze({
            _tag: 'Resolved' as const,
            type: parameter,
            spelling: spelling(source, token),
            token,
            syntax: path,
          }),
        ]
  })
  const rowNodes =
    requirementRowSyntax?.children.filter((element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element),
    ) ?? []
  const requirementExpression =
    requirementRowSyntax !== undefined && rowNodes.some((node) => node.kind === 'RowWithout')
      ? rowExpressionOf(source, requirementRowSyntax, rowNodes, typeParameters)
      : undefined
  return Object.freeze({
    failureRowSyntax,
    failures: Object.freeze(failures),
    requirementRowSyntax,
    requirements: Object.freeze(requirements),
    requirementParameters: Object.freeze(requirementParameters),
    rowParameterComponents: Object.freeze(rowParameterComponents),
    requirementExpression,
    diagnostics: Object.freeze(diagnostics),
  })
}

const unreportedLifetimeDiagnostic = (
  diagnostic: Diagnostic.Diagnostic,
  context: DeclarationLifetime.Context | undefined,
): ReadonlyArray<Diagnostic.Diagnostic> =>
  context?.diagnostics.some(
    (reported) =>
      reported.code === diagnostic.code &&
      reported.span.start === diagnostic.span.start &&
      reported.span.end === diagnostic.span.end,
  )
    ? Object.freeze([])
    : Object.freeze([diagnostic])

export const analyzeDeclaredType = (
  source: SourceFile.SourceFile,
  syntax: SyntaxTree.Node,
  typeParameters: ReadonlyMap<string, Type.Parameter> = new Map(),
  genericArgumentPosition = false,
  lifetimeContext?: DeclarationLifetime.Context,
): TypeResolution => {
  if (syntax.kind === 'LifetimeType') {
    const token = SyntaxTree.directToken(syntax, 'Lifetime')
    const lifetime =
      lifetimeContext?.regions.get(syntax) ??
      (token === undefined ? undefined : DeclarationLifetime.named(source, token, typeParameters))
    if (token !== undefined && lifetime !== undefined && genericArgumentPosition)
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Lifetime',
          lifetime,
          spelling: spelling(source, token),
          token,
          syntax,
        }),
        diagnostics: Object.freeze([]),
      })
    const diagnostic =
      token === undefined
        ? Diagnostic.invalidLifetimeBinder('Expected a lifetime argument', syntax.span)
        : Diagnostic.unknownLifetime(spelling(source, token), token.span)
    return Object.freeze({
      fact: Object.freeze({ _tag: 'Unavailable', syntax, cause: Diagnostic.identity(diagnostic) }),
      diagnostics: unreportedLifetimeDiagnostic(diagnostic, lifetimeContext),
    })
  }
  const lifetimeFor = (node: SyntaxTree.Node): Lifetime.Lifetime | undefined => {
    const token = SyntaxTree.directToken(node, 'Lifetime')
    return (
      lifetimeContext?.regions.get(node) ??
      (token === undefined ? undefined : DeclarationLifetime.named(source, token, typeParameters))
    )
  }
  const missingLifetime = (): TypeResolution => {
    const token = SyntaxTree.directToken(syntax, 'Lifetime')
    const diagnostic =
      token === undefined
        ? Diagnostic.ambiguousLifetimeElision(syntax.span)
        : Diagnostic.unknownLifetime(spelling(source, token), token.span)
    return Object.freeze({
      fact: Object.freeze({ _tag: 'Unavailable', syntax, cause: Diagnostic.identity(diagnostic) }),
      diagnostics: unreportedLifetimeDiagnostic(diagnostic, lifetimeContext),
    })
  }
  if (syntax.kind === 'UnitType') {
    const token = SyntaxTree.directToken(syntax, 'LeftParenthesis')
    if (token === undefined)
      return Object.freeze({
        fact: Object.freeze({ _tag: 'Unavailable', syntax }),
        diagnostics: Object.freeze([]),
      })
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Resolved',
        type: Type.unit,
        spelling: '()',
        token,
        syntax,
      }),
      diagnostics: Object.freeze([]),
    })
  }
  if (syntax.kind === 'CallableType') {
    const lifetimes = lifetimeContext?.callables.get(syntax)
    if (lifetimes === undefined) return missingLifetime()
    const token = SyntaxTree.directToken(syntax, 'FnKeyword')
    const typeNodes = syntax.children.filter(isDeclaredTypeNode)
    const resultSyntax = typeNodes.at(-1)
    if (token === undefined || resultSyntax === undefined) {
      return Object.freeze({
        fact: Object.freeze({ _tag: 'Unavailable', syntax }),
        diagnostics: Object.freeze([]),
      })
    }
    let mode: Type.CallableMode
    if (SyntaxTree.directToken(syntax, 'OnceKeyword') !== undefined) {
      mode = 'Take'
    } else if (SyntaxTree.directToken(syntax, 'MutKeyword') !== undefined) {
      mode = 'Exclusive'
    } else {
      mode = 'Shared'
    }
    const unsafe = SyntaxTree.directToken(syntax, 'UnsafeKeyword') !== undefined
    const analyzed = typeNodes.map((node) =>
      analyzeDeclaredType(source, node, typeParameters, false, lifetimeContext),
    )
    const result = analyzed.at(-1)
    const parameters = analyzed.slice(0, -1)
    const diagnostics = Object.freeze(analyzed.flatMap((entry) => entry.diagnostics))
    if (
      result?.fact._tag === 'Resolved' &&
      parameters.every((entry) => entry.fact._tag === 'Resolved')
    ) {
      const type = Type.callable(
        parameters.flatMap((entry) => (entry.fact._tag === 'Resolved' ? [entry.fact.type] : [])),
        result.fact.type,
        lifetimes,
        mode,
        undefined,
        unsafe,
      )
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Resolved',
          type,
          spelling: Type.encode(type),
          token,
          syntax,
          components: Object.freeze([
            ...parameters.map((parameter) => parameter.fact),
            result.fact,
          ]),
        }),
        diagnostics,
      })
    }
    const resultFact = result?.fact ?? Object.freeze({ _tag: 'Unavailable' as const, syntax })
    const cause = [...parameters.map((entry) => entry.fact), resultFact]
      .flatMap((fact) => ('cause' in fact && fact.cause !== undefined ? [fact.cause] : []))
      .at(-1)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Callable',
        lifetimes,
        unsafe,
        mode,
        parameters: Object.freeze(parameters.map((entry) => entry.fact)),
        result: resultFact,
        spelling: `${unsafe ? 'unsafe ' : ''}${mode === 'Exclusive' ? 'mut ' : ''}${mode === 'Take' ? 'once ' : ''}fn(...)`,
        token,
        syntax,
        ...(cause === undefined ? {} : { cause }),
      }),
      diagnostics,
    })
  }
  if (syntax.kind === 'ForeignFunctionType') {
    const lifetimes = lifetimeContext?.callables.get(syntax) ?? {
      environment: Lifetime.staticLifetime,
      lifetimeBinders: [],
    }
    const token = SyntaxTree.directToken(syntax, 'FnKeyword')
    const abiToken = SyntaxTree.directToken(syntax, 'TextLiteral')
    const typeNodes = syntax.children.filter(isDeclaredTypeNode)
    const resultSyntax = typeNodes.at(-1)
    if (token === undefined || resultSyntax === undefined) {
      return Object.freeze({
        fact: Object.freeze({ _tag: 'Unavailable', syntax }),
        diagnostics: Object.freeze([]),
      })
    }
    const analyzed = typeNodes.map((node) =>
      analyzeDeclaredType(source, node, typeParameters, false, lifetimeContext),
    )
    const result = analyzed.at(-1)
    const parameters = analyzed.slice(0, -1)
    const clauses = DeclarationProperty.clauses(syntax)
    const behavior = ForeignContract.analyze(
      source,
      clauses[0],
      parameters.map((entry, ordinal) => ({
        name: String(ordinal),
        type: entry.fact._tag === 'Resolved' ? entry.fact.type : undefined,
        span: entry.fact.syntax.span,
      })),
      result?.fact._tag === 'Resolved' ? result.fact.type : undefined,
    )
    const diagnostics: Array<Diagnostic.Diagnostic> = analyzed.flatMap((entry) =>
      Array.from(entry.diagnostics),
    )
    diagnostics.push(...behavior.diagnostics)
    for (const clause of clauses.slice(1))
      diagnostics.push(
        Diagnostic.foreignDeclarationRestriction('duplicate foreign contract', clause.span),
      )
    if (abiToken !== undefined) {
      const abi = decodedText(source, abiToken)
      if (abi !== 'C') diagnostics.push(Diagnostic.unsupportedForeignAbi(abi ?? '', abiToken.span))
    }
    if (
      diagnostics.length === 0 &&
      result?.fact._tag === 'Resolved' &&
      parameters.every((entry) => entry.fact._tag === 'Resolved')
    ) {
      const type = Type.foreignFunction(
        parameters.flatMap((entry) => (entry.fact._tag === 'Resolved' ? [entry.fact.type] : [])),
        result.fact.type,
        behavior.contract,
        lifetimes,
      )
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Resolved',
          type,
          spelling: Type.encode(type),
          token,
          syntax,
          components: Object.freeze([
            ...parameters.map((parameter) => parameter.fact),
            result.fact,
          ]),
        }),
        diagnostics: Object.freeze(diagnostics),
      })
    }
    const resultFact = result?.fact ?? Object.freeze({ _tag: 'Unavailable' as const, syntax })
    const cause = diagnostics.at(-1)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'ForeignFunction',
        lifetimes,
        contract: behavior.contract,
        parameters: Object.freeze(parameters.map((entry) => entry.fact)),
        result: resultFact,
        spelling: 'extern "C" fn(...)',
        token,
        syntax,
        ...(cause === undefined ? {} : { cause: Diagnostic.identity(cause) }),
      }),
      diagnostics: Object.freeze(diagnostics),
    })
  }
  if (syntax.kind === 'ParenthesizedType') {
    const inner = syntax.children.find(isDeclaredTypeNode)
    if (inner === undefined)
      return Object.freeze({
        fact: Object.freeze({ _tag: 'Unavailable', syntax }),
        diagnostics: Object.freeze([]),
      })
    const analyzed = analyzeDeclaredType(
      source,
      inner,
      typeParameters,
      genericArgumentPosition,
      lifetimeContext,
    )
    return Object.freeze({
      fact: Object.freeze({ ...analyzed.fact, syntax }),
      diagnostics: analyzed.diagnostics,
    })
  }
  if (syntax.kind === 'UnionType') {
    const members = syntax.children
      .filter(isDeclaredTypeNode)
      .map((member) => analyzeDeclaredType(source, member, typeParameters, false, lifetimeContext))
    const diagnostics: Array<Diagnostic.Diagnostic> = members.flatMap((member) =>
      Array.from(member.diagnostics),
    )
    const facts = Object.freeze(members.map((member) => member.fact))
    const separators = Object.freeze(
      syntax.children.filter(
        (element): element is Token.Token => SyntaxTree.isToken(element) && element.kind === 'Pipe',
      ),
    )
    const firstResolved = facts.find(
      (fact): fact is Extract<DeclaredTypeFact, { readonly _tag: 'Resolved' }> =>
        fact._tag === 'Resolved',
    )
    const firstToken = SyntaxTree.tokens(syntax).find((token) => token.kind === 'Identifier')
    if (firstToken === undefined) {
      return Object.freeze({
        fact: Object.freeze({ _tag: 'Unavailable', syntax }),
        diagnostics: Object.freeze(diagnostics),
      })
    }
    if (facts.every((fact) => fact._tag === 'Resolved')) {
      const resolved = facts.filter(
        (fact): fact is Extract<DeclaredTypeFact, { readonly _tag: 'Resolved' }> =>
          fact._tag === 'Resolved',
      )
      const normalized = Type.union(resolved.map((fact) => fact.type))
      if (normalized._tag === 'Normalized') {
        return Object.freeze({
          fact: Object.freeze({
            _tag: 'Resolved',
            type: normalized.type,
            spelling: Type.encode(normalized.type),
            token: firstResolved?.token ?? firstToken,
            syntax,
            unionSource: Object.freeze({
              _tag: 'UnionSource',
              members: facts,
              separators,
              syntax,
            }),
          }),
          diagnostics: Object.freeze(diagnostics),
        })
      }
      if (normalized._tag === 'InvalidMembers') {
        for (const invalid of normalized.members) {
          const sourceFact = resolved.find((fact) => Type.equals(fact.type, invalid))
          diagnostics.push(
            Diagnostic.invalidUnionMember(
              Type.encode(invalid),
              sourceFact?.syntax.span ?? syntax.span,
            ),
          )
        }
      }
    }
    const cause = diagnostics.at(-1)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Union',
        members: facts,
        separators,
        spelling: facts
          .map((fact) => (fact._tag === 'Resolved' ? Type.encode(fact.type) : 'unavailable'))
          .join(' | '),
        token: firstResolved?.token ?? firstToken,
        syntax,
        ...(cause === undefined ? {} : { cause: Diagnostic.identity(cause) }),
      }),
      diagnostics: Object.freeze(diagnostics),
    })
  }
  if (syntax.kind === 'SliceType') {
    const lifetime = lifetimeFor(syntax)
    if (lifetime === undefined) return missingLifetime()
    const token = SyntaxTree.directToken(syntax, 'Ampersand')
    const elementSyntax = syntax.children.find(isDeclaredTypeNode)
    if (token === undefined || elementSyntax === undefined) {
      return Object.freeze({
        fact: Object.freeze({ _tag: 'Unavailable', syntax }),
        diagnostics: Object.freeze([]),
      })
    }
    const access: Type.Slice['access'] =
      SyntaxTree.directToken(syntax, 'MutKeyword') === undefined ? 'Shared' : 'Exclusive'
    const element = analyzeDeclaredType(
      source,
      elementSyntax,
      typeParameters,
      false,
      lifetimeContext,
    )
    if (element.fact._tag === 'Resolved') {
      const type = Type.slice(access, element.fact.type, lifetime)
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Resolved',
          type,
          spelling: Type.encode(type),
          token,
          syntax,
          components: Object.freeze([element.fact]),
        }),
        diagnostics: element.diagnostics,
      })
    }
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Slice',
        lifetime,
        access,
        element: element.fact,
        spelling: `${access === 'Exclusive' ? '&mut' : '&'}[unavailable]`,
        token,
        syntax,
        ...('cause' in element.fact && element.fact.cause !== undefined
          ? { cause: element.fact.cause }
          : {}),
      }),
      diagnostics: element.diagnostics,
    })
  }
  if (syntax.kind === 'ReferenceType') {
    const lifetime = lifetimeFor(syntax)
    if (lifetime === undefined) return missingLifetime()
    const token = SyntaxTree.directToken(syntax, 'Ampersand')
    const targetSyntax = syntax.children.find(isDeclaredTypeNode)
    if (token === undefined || targetSyntax === undefined) {
      return Object.freeze({
        fact: Object.freeze({ _tag: 'Unavailable', syntax }),
        diagnostics: Object.freeze([]),
      })
    }
    const access: 'Shared' | 'Exclusive' =
      SyntaxTree.directToken(syntax, 'MutKeyword') === undefined ? 'Shared' : 'Exclusive'
    const target = analyzeDeclaredType(source, targetSyntax, typeParameters, false, lifetimeContext)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Reference',
        lifetime,
        access,
        target: target.fact,
        spelling: `${access === 'Exclusive' ? '&mut ' : '&'}unavailable`,
        token,
        syntax,
        ...('cause' in target.fact && target.fact.cause !== undefined
          ? { cause: target.fact.cause }
          : {}),
      }),
      diagnostics: target.diagnostics,
    })
  }
  if (syntax.kind === 'PointerType') {
    const token = SyntaxTree.directToken(syntax, 'Star')
    const pointeeSyntax = syntax.children.find(isDeclaredTypeNode)
    if (token === undefined || pointeeSyntax === undefined) {
      return Object.freeze({
        fact: Object.freeze({ _tag: 'Unavailable', syntax }),
        diagnostics: Object.freeze([]),
      })
    }
    const mutable = SyntaxTree.directToken(syntax, 'MutKeyword') !== undefined
    const nullable = SyntaxTree.directToken(syntax, 'Question') !== undefined
    const extent: Type.Pointer['extent'] =
      SyntaxTree.directToken(syntax, 'LeftBracket') === undefined ? 'Single' : 'Many'
    let alignment: Type.Pointer['alignment'] = 'Natural'
    const seen = new Set<string>()
    const qualifierDiagnostics: Array<Diagnostic.Diagnostic> = []
    for (const qualifier of SyntaxTree.directNodes(syntax, 'PointerQualifier')) {
      const nameToken = SyntaxTree.directToken(qualifier, 'Identifier')
      const valueToken = SyntaxTree.directToken(qualifier, 'DecimalInteger')
      if (nameToken === undefined || valueToken === undefined) continue
      const name = Option.getOrElse(SourceFile.spelling(source, nameToken.span), () => '')
      const spelling = Option.getOrElse(SourceFile.spelling(source, valueToken.span), () => '')
      const value = Number(spelling.replaceAll('_', ''))
      let detail: string | undefined
      if (seen.has(name)) detail = 'qualifier is repeated'
      else if (name === 'align') {
        if (Type.isPointerAlignment(value)) alignment = value
        else detail = 'alignment must be a positive power of two no greater than 536870912'
      } else if (name === 'addrspace' && value !== 0)
        detail = 'only ordinary data address space zero is admitted'
      seen.add(name)
      if (detail !== undefined)
        qualifierDiagnostics.push(Diagnostic.invalidPointerQualifier(name, detail, valueToken.span))
    }
    const qualifiers = { mutable, nullable, extent, alignment, addressSpace: 0 as const }
    const invalid = qualifierDiagnostics[0]
    if (invalid !== undefined)
      return Object.freeze({
        fact: Object.freeze({ _tag: 'Unavailable', syntax, cause: Diagnostic.identity(invalid) }),
        diagnostics: Object.freeze(qualifierDiagnostics),
      })
    const pointee = analyzeDeclaredType(
      source,
      pointeeSyntax,
      typeParameters,
      false,
      lifetimeContext,
    )
    if (pointee.fact._tag === 'Resolved') {
      const type = Type.pointer({ ...qualifiers, pointee: pointee.fact.type })
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Resolved',
          type,
          spelling: Type.encode(type),
          token,
          syntax,
          components: Object.freeze([pointee.fact]),
        }),
        diagnostics: pointee.diagnostics,
      })
    }
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Pointer',
        ...qualifiers,
        pointee: pointee.fact,
        spelling: `${mutable ? '*mut ' : '*const '}unavailable`,
        token,
        syntax,
        ...('cause' in pointee.fact && pointee.fact.cause !== undefined
          ? { cause: pointee.fact.cause }
          : {}),
      }),
      diagnostics: pointee.diagnostics,
    })
  }
  if (syntax.kind === 'FixedArrayType') {
    const arrayToken = SyntaxTree.directToken(syntax, 'LeftBracket')
    const elementSyntax = syntax.children.find(isDeclaredTypeNode)
    const lengthToken = SyntaxTree.directToken(syntax, 'DecimalInteger')
    if (arrayToken === undefined || elementSyntax === undefined) {
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Unavailable',
          syntax: SyntaxTree.unavailableChild(syntax, 'LeftBracket'),
        }),
        diagnostics: Object.freeze([]),
      })
    }
    const element = analyzeDeclaredType(
      source,
      elementSyntax,
      typeParameters,
      false,
      lifetimeContext,
    )
    let length: ArrayLengthFact
    const diagnostics: Array<Diagnostic.Diagnostic> = [...element.diagnostics]
    if (lengthToken === undefined) {
      length = Object.freeze({
        _tag: 'Unavailable',
        syntax: SyntaxTree.unavailableChild(syntax, 'DecimalInteger'),
      })
    } else {
      const lengthSpelling = spelling(source, lengthToken)
      const value = Number(IntegerLiteral.magnitude(lengthSpelling))
      if (!Number.isSafeInteger(value) || value > 2147483647) {
        const diagnostic = Diagnostic.integerOutOfRange(lengthSpelling, lengthToken.span)
        diagnostics.push(diagnostic)
        length = Object.freeze({
          _tag: 'OutOfRange',
          spelling: lengthSpelling,
          token: lengthToken,
          cause: Diagnostic.identity(diagnostic),
        })
      } else {
        length = Object.freeze({
          _tag: 'Available',
          value,
          spelling: lengthSpelling,
          token: lengthToken,
        })
      }
    }
    if (element.fact._tag === 'Resolved' && length._tag === 'Available') {
      const type = Type.fixedArray(element.fact.type, length.value)
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Resolved',
          type,
          spelling: Type.encode(type),
          token: arrayToken,
          syntax,
          components: Object.freeze([element.fact]),
        }),
        diagnostics: Object.freeze(diagnostics),
      })
    }
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'FixedArray',
        element: element.fact,
        length,
        spelling: `Array<${
          element.fact._tag === 'Resolved' ? Type.encode(element.fact.type) : 'unavailable'
        }, ${length._tag === 'Available' ? length.value : 'unavailable'}>`,
        token: arrayToken,
        syntax,
      }),
      diagnostics: Object.freeze(diagnostics),
    })
  }
  if (syntax.kind === 'OpaqueResultType') {
    // The binder is owned by the declaration that carries it, so its representation parameters and
    // family key can only be minted where that canonical identity is known. Until the declaration
    // site supplies it, the result stays deterministically unavailable rather than resolving to a
    // parameter with a fabricated owner.
    return Object.freeze({
      fact: Object.freeze({ _tag: 'Unavailable', syntax }),
      diagnostics: Object.freeze([]),
    })
  }
  if (syntax.kind === 'ExactRepresentationType') {
    const item = syntax.children.find(isDeclaredTypeNode)
    let pathSyntax: SyntaxTree.Node | undefined
    if (item === undefined) {
      pathSyntax = undefined
    } else if (item.kind === 'TypePath') {
      pathSyntax = item
    } else {
      pathSyntax = SyntaxTree.directNode(item, 'TypePath')
    }
    const keyword = SyntaxTree.directToken(syntax, 'Identifier')
    if (item === undefined || pathSyntax === undefined || keyword === undefined)
      return Object.freeze({
        fact: Object.freeze({ _tag: 'Unavailable', syntax }),
        diagnostics: Object.freeze([]),
      })
    const segments = SyntaxTree.tokens(pathSyntax)
      .filter((token) => token.kind === 'Identifier')
      .map((token) => Object.freeze({ spelling: spelling(source, token), token }))
    if (segments.length === 0 || !SyntaxTree.isAvailableSyntax(syntax))
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Unavailable',
          syntax: SyntaxTree.unavailableChild(syntax, 'Identifier'),
        }),
        diagnostics: Object.freeze([]),
      })
    const list =
      item.kind === 'AppliedType' ? SyntaxTree.directNode(item, 'TypeArgumentList') : undefined
    const arguments_ = (
      list?.children.filter(
        (element): element is SyntaxTree.Node =>
          SyntaxTree.isNode(element) &&
          (element.kind === 'LifetimeType' || isDeclaredTypeNode(element)),
      ) ?? []
    ).map((argument) =>
      analyzeDeclaredType(source, argument, typeParameters, true, lifetimeContext),
    )
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'ExactRepresentation',
        item: Object.freeze({
          _tag: 'TypePath',
          spelling: segments.map((segment) => segment.spelling).join('.'),
          segments: Object.freeze(segments),
          syntax: pathSyntax,
        }),
        arguments: Object.freeze(arguments_.map((argument) => argument.fact)),
        spelling: `typeof(${segments.map((segment) => segment.spelling).join('.')})`,
        token: keyword,
        syntax,
      }),
      diagnostics: Object.freeze(arguments_.flatMap((argument) => argument.diagnostics)),
    })
  }
  if (syntax.kind === 'AppliedType') {
    const pathSyntax = SyntaxTree.directNode(syntax, 'TypePath')
    const list = SyntaxTree.directNode(syntax, 'TypeArgumentList')
    const firstToken = SyntaxTree.tokens(syntax).find((token) => token.kind === 'Identifier')
    if (pathSyntax === undefined || list === undefined || firstToken === undefined) {
      return Object.freeze({
        fact: Object.freeze({ _tag: 'Unavailable', syntax }),
        diagnostics: Object.freeze([]),
      })
    }
    const target = analyzeDeclaredType(source, pathSyntax, typeParameters, false, lifetimeContext)
    const arguments_ = list.children
      .filter(
        (element): element is SyntaxTree.Node =>
          SyntaxTree.isNode(element) &&
          (element.kind === 'LifetimeType' || isDeclaredTypeNode(element)),
      )
      .map((argument) =>
        analyzeDeclaredType(source, argument, typeParameters, true, lifetimeContext),
      )
    const pathSegments = SyntaxTree.tokens(pathSyntax)
      .filter((token) => token.kind === 'Identifier')
      .map((token) => spelling(source, token))
    if (pathSegments.length === 1 && pathSegments.at(0) === 'string') {
      const argument = arguments_.at(0)?.fact
      if (arguments_.length !== 1 || argument?._tag !== 'Lifetime') {
        const diagnostic = Diagnostic.invalidLifetimeBinder(
          'string requires exactly one lifetime argument',
          syntax.span,
        )
        return Object.freeze({
          fact: Object.freeze({
            _tag: 'Unavailable',
            syntax,
            cause: Diagnostic.identity(diagnostic),
          }),
          diagnostics: Object.freeze([
            ...arguments_.flatMap((argument) => argument.diagnostics),
            diagnostic,
          ]),
        })
      }
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Resolved',
          type: Type.string(argument.lifetime),
          spelling: `string<${Lifetime.display(argument.lifetime)}>`,
          token: firstToken,
          syntax,
          components: [argument],
        }),
        diagnostics: Object.freeze([]),
      })
    }
    if (pathSegments.length === 1 && pathSegments.at(0) === 'Effect') {
      const environment = lifetimeFor(syntax)
      if (environment === undefined) return missingLifetime()
      const lifetimes: Type.ExecutableLifetimes = Object.freeze({
        environment,
        lifetimeBinders: Object.freeze([]),
      })
      let access: Type.Effect['access']
      if (SyntaxTree.directToken(syntax, 'OnceKeyword') !== undefined) {
        access = 'Take'
      } else if (SyntaxTree.directToken(syntax, 'MutKeyword') !== undefined) {
        access = 'Exclusive'
      } else {
        access = 'Shared'
      }
      const {
        failures,
        requirements,
        requirementParameters,
        rowParameterComponents,
        requirementExpression,
        diagnostics: rowDiagnostics,
      } = analyzeAppliedRows(source, list, typeParameters, lifetimeContext)
      const diagnostics = [
        ...rowDiagnostics,
        ...arguments_.flatMap((argument) => argument.diagnostics),
        ...failures.flatMap((failure) => failure.diagnostics),
        ...requirements.flatMap((requirement) => requirement.capability.diagnostics),
      ]
      const success = arguments_.at(0)?.fact
      if (arguments_.length !== 1) {
        diagnostics.push(
          Diagnostic.typeArgumentArity('Effect', 1, arguments_.length, firstToken.span),
        )
      }
      const resolvedFailures = failures.flatMap((failure) =>
        failure.fact._tag === 'Resolved' && Type.isTypeArgument(failure.fact.type)
          ? [failure.fact.type]
          : [],
      )
      const resolvedRequirements = requirements.flatMap((requirement) =>
        requirement.capability.fact._tag === 'Resolved' &&
        requirementRoleIdentity(requirement.role) !== undefined &&
        (Type.isNominal(requirement.capability.fact.type) ||
          (Type.isParameter(requirement.capability.fact.type) &&
            requirement.capability.fact.type.kind === 'Value'))
          ? [
              Object.freeze({
                capability: requirement.capability.fact.type,
                role: requirementRoleIdentity(requirement.role) ?? RequirementRow.defaultRole,
                access: requirement.access,
              }),
            ]
          : [],
      )
      const failuresAvailable = failures.every(
        (failure) => failure.fact._tag === 'Resolved' && Type.isTypeArgument(failure.fact.type),
      )
      // A subtracting row is resolved with the module graph, where the row algebra runs.
      if (
        arguments_.length === 1 &&
        success?._tag === 'Resolved' &&
        failuresAvailable &&
        requirementExpression === undefined &&
        resolvedRequirements.length === requirements.length
      ) {
        const type = Type.effect(
          success.type,
          resolvedFailures,
          lifetimes,
          access,
          resolvedRequirements,
          requirementParameters,
        )
        return Object.freeze({
          fact: Object.freeze({
            _tag: 'Resolved',
            type,
            spelling: Type.encode(type),
            token: firstToken,
            syntax,
            components: Object.freeze([
              target.fact,
              ...arguments_.map((argument) => argument.fact),
              ...requirements.map((requirement) => requirement.capability.fact),
              ...rowParameterComponents,
            ]),
          }),
          diagnostics: Object.freeze(diagnostics),
        })
      }
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Effect',
          lifetimes,
          access,
          success: success ?? Object.freeze({ _tag: 'Unavailable', syntax: list }),
          failures: Object.freeze(failures.map((failure) => failure.fact)),
          requirements: Object.freeze(
            requirements.map((requirement) =>
              Object.freeze({
                capability: requirement.capability.fact,
                role: requirement.role,
                access: requirement.access,
                syntax: requirement.syntax,
              }),
            ),
          ),
          requirementParameters: Object.freeze(requirementParameters),
          ...(requirementExpression === undefined ? {} : { requirementExpression }),
          spelling: 'Effect',
          token: firstToken,
          syntax,
        }),
        diagnostics: Object.freeze(diagnostics),
      })
    }
    const {
      failures,
      requirementRowSyntax,
      requirements,
      requirementParameters,
      diagnostics: rowDiagnostics,
    } = analyzeAppliedRows(source, list, typeParameters, lifetimeContext)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Applied',
        ...(lifetimeContext?.nominalArguments.get(syntax) === undefined
          ? {}
          : { implicitLifetimeArguments: lifetimeContext.nominalArguments.get(syntax) ?? [] }),
        target: target.fact,
        arguments: Object.freeze(arguments_.map((argument) => argument.fact)),
        ...(requirementRowSyntax === undefined
          ? {}
          : {
              requirementRow: Object.freeze({
                requirements: Object.freeze(
                  requirements.map((requirement) =>
                    Object.freeze({
                      ...requirement,
                      capability: requirement.capability.fact,
                    }),
                  ),
                ),
                parameters: Object.freeze(requirementParameters),
                syntax: requirementRowSyntax,
              }),
            }),
        spelling: SyntaxTree.tokens(syntax)
          .filter(
            (token) =>
              !['Whitespace', 'LineComment', 'DocComment', 'ModuleDocComment'].includes(token.kind),
          )
          .map((token) => spelling(source, token))
          .join(''),
        token: firstToken,
        syntax,
      }),
      diagnostics: Diagnostic.merge(
        target.diagnostics,
        ...arguments_.map((argument) => argument.diagnostics),
        ...failures.map((failure) => failure.diagnostics),
        ...requirements.map((requirement) => requirement.capability.diagnostics),
        rowDiagnostics,
      ),
    })
  }
  const tokens = syntax.children.filter(
    (element): element is Token.Token =>
      SyntaxTree.isToken(element) && element.kind === 'Identifier',
  )
  const segments = tokens.map((token) =>
    Object.freeze({ spelling: spelling(source, token), token }),
  )
  const first = segments.at(0)
  if (first === undefined || !SyntaxTree.isAvailableSyntax(syntax)) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Unavailable',
        syntax: SyntaxTree.unavailableChild(syntax, 'Identifier'),
      }),
      diagnostics: Object.freeze([]),
    })
  }
  const path: TypePathFact = Object.freeze({
    _tag: 'TypePath',
    spelling: segments.map((segment) => segment.spelling).join('.'),
    segments: Object.freeze(segments),
    syntax,
  })
  if (segments.length === 1 && first.spelling === 'string') {
    const lifetime = lifetimeFor(syntax)
    if (lifetime === undefined) return missingLifetime()
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Resolved',
        type: Type.string(lifetime),
        spelling: `string<${Lifetime.display(lifetime)}>`,
        token: first.token,
        syntax,
        path,
      }),
      diagnostics: Object.freeze([]),
    })
  }
  if (segments.length === 1 && (Type.isBuiltin(first.spelling) || first.spelling === 'never')) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Resolved',
        type: first.spelling,
        spelling: first.spelling,
        token: first.token,
        syntax,
        path,
      }),
      diagnostics: Object.freeze([]),
    })
  }
  const intrinsicNominal =
    segments.length === 1 ? Type.intrinsicNominals.get(first.spelling) : undefined
  if (intrinsicNominal !== undefined) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Resolved',
        type: intrinsicNominal,
        spelling: first.spelling,
        token: first.token,
        syntax,
        path,
      }),
      diagnostics: Object.freeze([]),
    })
  }
  const parameterType = segments.length === 1 ? typeParameters.get(first.spelling) : undefined
  if (parameterType !== undefined) {
    if (
      parameterType.kind === 'CallableRepresentation' ||
      parameterType.kind === 'EffectRepresentation'
    ) {
      const bound = parameterType.representationBound
      if (bound === undefined) {
        return Object.freeze({
          fact: Object.freeze({
            _tag: 'RepresentationParameter',
            parameter: parameterType,
            spelling: first.spelling,
            token: first.token,
            syntax,
            path,
          }),
          diagnostics: Object.freeze([]),
        })
      }
      const type = Type.represented(
        bound,
        bound,
        Type.representationParameterArgument(parameterType),
      )
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Resolved',
          type,
          spelling: first.spelling,
          token: first.token,
          syntax,
          path,
        }),
        diagnostics: Object.freeze([]),
      })
    }
    if (parameterType.kind !== 'Value' && !genericArgumentPosition) {
      const diagnostic = Diagnostic.genericParameterKindMismatch(
        first.spelling,
        'Value',
        parameterType.kind,
        first.token.span,
      )
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Unavailable',
          syntax,
          cause: Diagnostic.identity(diagnostic),
        }),
        diagnostics: Object.freeze([diagnostic]),
      })
    }
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Resolved',
        type: parameterType,
        spelling: first.spelling,
        token: first.token,
        syntax,
        path,
      }),
      diagnostics: Object.freeze([]),
    })
  }
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Unresolved',
      ...(lifetimeContext?.nominalArguments.get(syntax) === undefined
        ? {}
        : { implicitLifetimeArguments: lifetimeContext.nominalArguments.get(syntax) ?? [] }),
      spelling: path.spelling,
      token: first.token,
      syntax,
      path,
    }),
    diagnostics: Object.freeze([]),
  })
}

const isSeparator = (element: SyntaxTree.Element, kind: Token.TokenKind): boolean =>
  (SyntaxTree.isToken(element) && element.kind === kind) ||
  (SyntaxTree.isMissingToken(element) && element.expected === kind)

const identifierToken = (elements: ReadonlyArray<SyntaxTree.Element>): Token.Token | undefined =>
  elements.every(SyntaxTree.isAvailableSyntax)
    ? elements.find(
        (element): element is Token.Token =>
          SyntaxTree.isToken(element) && element.kind === 'Identifier',
      )
    : undefined

const analyzeParameter = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  functionId: DeclarationId,
  ordinal: number,
  typeParameters: ReadonlyMap<string, Type.Parameter> = new Map(),
  lifetimeContext?: DeclarationLifetime.Context,
): {
  readonly fact: ParameterFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  const colonIndex = node.children.findIndex((element) => isSeparator(element, 'Colon'))
  const nameElements = colonIndex < 0 ? node.children : node.children.slice(0, colonIndex)
  const nameToken = identifierToken(nameElements)
  const name: DeclaredName =
    nameToken === undefined
      ? Object.freeze({
          _tag: 'Unavailable',
          syntax: SyntaxTree.unavailableElement(nameElements, node),
        })
      : Object.freeze({
          _tag: 'Present',
          spelling: spelling(source, nameToken),
          token: nameToken,
        })
  const type = analyzeDeclaredType(
    source,
    declaredTypeNode(node),
    typeParameters,
    false,
    lifetimeContext,
  )
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'ParameterDeclaration',
      id: Object.freeze({ _tag: 'ParameterId', function: functionId, ordinal }),
      name,
      phase: SyntaxTree.directToken(node, 'StaticKeyword') === undefined ? 'Runtime' : 'Static',
      bindingMutability:
        SyntaxTree.directToken(node, 'MutKeyword') === undefined ? 'Immutable' : 'Mutable',
      declaredType: type.fact,
      syntax: node,
    }),
    diagnostics: type.diagnostics,
  })
}

/** Collects the declaration-shaped contract owned by one anonymous callable occurrence. */
export const collectAnonymousCallableDeclaration = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  id: DeclarationId,
  canonical: CanonicalId,
  inheritedTypeParameters: ReadonlyArray<TypeParameterFact>,
): {
  readonly fact: DeclarationFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  const environment = new Map(
    inheritedTypeParameters.flatMap((parameter) =>
      parameter.name._tag === 'Present' ? [[parameter.name.spelling, parameter.type] as const] : [],
    ),
  )
  const lifetimeContext = DeclarationLifetime.forHeader(source, canonical, node, environment)
  const parameterList = childNode(node, 'ParameterList')
  const parameters = SyntaxTree.directNodes(parameterList, 'ParameterDeclaration').map(
    (parameter, ordinal) =>
      analyzeParameter(source, parameter, id, ordinal, environment, lifetimeContext),
  )
  const returnSyntax = SyntaxTree.directNode(node, 'ReturnType')
  const returnType =
    returnSyntax === undefined
      ? Object.freeze({
          fact: Object.freeze({ _tag: 'Unavailable' as const, syntax: node }),
          diagnostics: Object.freeze<ReadonlyArray<Diagnostic.Diagnostic>>([]),
        })
      : collectReturnType(
          source,
          returnSyntax,
          canonical.name,
          inheritedTypeParameters,
          environment,
          lifetimeContext,
        )
  const failureRow = collectFailureRow(source, node, environment, lifetimeContext)
  const requirementRow = collectRequirementRow(source, node, environment, lifetimeContext)
  const parameterFacts = Object.freeze(parameters.map((parameter) => parameter.fact))
  const diagnostics = Object.freeze([
    ...lifetimeContext.diagnostics,
    ...parameters.flatMap((parameter) => parameter.diagnostics),
    ...duplicateParameterDiagnostics(parameterFacts),
    ...returnType.diagnostics,
    ...failureRow.diagnostics,
    ...requirementRow.diagnostics,
    ...(SyntaxTree.directToken(node, 'EffectKeyword') === undefined &&
    failureRow.fact.syntax !== undefined
      ? [Diagnostic.failureChannelOnOrdinary(failureRow.fact.syntax.span)]
      : []),
  ])
  const fnToken = SyntaxTree.directToken(node, 'FnKeyword')
  const name: DeclaredName =
    fnToken === undefined
      ? Object.freeze({ _tag: 'Unavailable', syntax: node })
      : Object.freeze({
          _tag: 'Present',
          spelling: canonical.name,
          token: fnToken,
        })
  const retainedBody = bodyTemplate(source, node)
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'FunctionDeclaration',
      lifetimeElaboration: lifetimeContext,
      id,
      canonical: Object.freeze({ _tag: 'Canonical', id: canonical }),
      visibility: 'Private',
      phase: 'Runtime',
      functionKind:
        SyntaxTree.directToken(node, 'EffectKeyword') === undefined ? 'Ordinary' : 'Effect',
      unsafe: false,
      typeParameters: Object.freeze([
        ...inheritedTypeParameters,
        ...implicitLifetimeParameters(lifetimeContext, environment),
      ]),
      parameterCount: parameterFacts.length,
      parameters: parameterFacts,
      name,
      returnType: returnType.fact,
      ...('opaqueResult' in returnType && returnType.opaqueResult !== undefined
        ? { opaqueResult: returnType.opaqueResult }
        : {}),
      failureRow: failureRow.fact,
      requirementRow: requirementRow.fact,
      constraints: Object.freeze([]),
      constraintContracts: Object.freeze([]),
      ...(retainedBody === undefined ? {} : { bodyTemplate: retainedBody }),
      syntax: node,
    }),
    diagnostics,
  })
}

const duplicateParameterDiagnostics = (parameters: ReadonlyArray<ParameterFact>) => {
  const first = new Map<string, ReturnType<typeof presentParameterEntries>[number]>()
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  for (const entry of presentParameterEntries(parameters)) {
    const original = first.get(entry.spelling)
    if (original === undefined) first.set(entry.spelling, entry)
    else
      diagnostics.push(
        Diagnostic.duplicateParameterName(entry.spelling, original.token.span, entry.token.span),
      )
  }
  return Object.freeze(diagnostics)
}

const collectFields = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  owner: FieldOwnerId,
  nodeKind: 'StructField' | 'UnionVariantField',
  typeParameters: ReadonlyMap<string, Type.Parameter>,
  lifetimeContext?: DeclarationLifetime.Context,
) => {
  const first = new Map<string, { readonly id: FieldId; readonly token: Token.Token }>()
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const fields = SyntaxTree.directNodes(node, nodeKind).map((fieldNode, ordinal): FieldFact => {
    const id: FieldId = Object.freeze({ _tag: 'FieldId', owner, ordinal })
    const name = presentName(source, fieldNode)
    const type = analyzeDeclaredType(
      source,
      declaredTypeNode(fieldNode),
      typeParameters,
      false,
      lifetimeContext,
    )
    diagnostics.push(...type.diagnostics)
    let state: FieldState
    if (name._tag !== 'Present') state = Object.freeze({ _tag: 'Unidentified' })
    else {
      const original = first.get(name.spelling)
      if (original === undefined) {
        first.set(name.spelling, Object.freeze({ id, token: name.token }))
        state = Object.freeze({ _tag: 'Unique', id })
      } else {
        const diagnostic = Diagnostic.duplicateFieldName(
          name.spelling,
          original.token.span,
          name.token.span,
        )
        diagnostics.push(diagnostic)
        state = Object.freeze({
          _tag: 'Duplicate',
          original: original.id,
          cause: Diagnostic.identity(diagnostic),
        })
      }
    }
    return Object.freeze({
      _tag: 'AggregateField',
      id,
      member: AggregateIdentity.labeled(name._tag === 'Present' ? name.spelling : ''),
      state,
      visibility:
        SyntaxTree.directToken(fieldNode, 'PubKeyword') === undefined ? 'Private' : 'Public',
      name,
      declaredType: type.fact,
      syntax: fieldNode,
    })
  })
  return Object.freeze({ fields: Object.freeze(fields), diagnostics: Object.freeze(diagnostics) })
}

/** Collects declaration-ordered tuple positions without inventing source field spellings. */
const collectPositionalFields = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  owner: FieldOwnerId,
  typeParameters: ReadonlyMap<string, Type.Parameter>,
  lifetimeContext?: DeclarationLifetime.Context,
) => {
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const fields = node.children.filter(isDeclaredTypeNode).map((typeSyntax, ordinal): FieldFact => {
    const id: FieldId = Object.freeze({ _tag: 'FieldId', owner, ordinal })
    const declaredType = analyzeDeclaredType(
      source,
      typeSyntax,
      typeParameters,
      false,
      lifetimeContext,
    )
    diagnostics.push(...declaredType.diagnostics)
    return Object.freeze({
      _tag: 'AggregateField',
      id,
      member: AggregateIdentity.ordinal(ordinal),
      state: Object.freeze({ _tag: 'Unique', id }),
      visibility: 'Public',
      name: Object.freeze({ _tag: 'Unavailable', syntax: typeSyntax }),
      declaredType: declaredType.fact,
      syntax: typeSyntax,
    })
  })
  return Object.freeze({ fields: Object.freeze(fields), diagnostics: Object.freeze(diagnostics) })
}

const compareDiagnostics = (left: Diagnostic.Diagnostic, right: Diagnostic.Diagnostic): number => {
  const spanOrder = left.span.start - right.span.start || left.span.end - right.span.end
  if (spanOrder !== 0) return spanOrder
  if (left.code < right.code) return -1
  if (left.code > right.code) return 1
  return 0
}

const collectTypeParameters = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  ownerName: string,
  ordinalOffset = 0,
  enclosing: ReadonlyArray<TypeParameterFact> = [],
): {
  readonly facts: ReadonlyArray<TypeParameterFact>
  readonly environment: ReadonlyMap<string, Type.Parameter>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly lifetimeContext: DeclarationLifetime.Context
} => {
  const list = SyntaxTree.directNode(node, 'TypeParameterList')
  const environment = new Map<string, Type.Parameter>(
    enclosing.flatMap((parameter) =>
      parameter.name._tag === 'Present' ? [[parameter.name.spelling, parameter.type] as const] : [],
    ),
  )
  const originals = new Map<string, SourceSpan.SourceSpan>(
    enclosing.flatMap((parameter) =>
      parameter.name._tag === 'Present'
        ? [[parameter.name.spelling, parameter.name.token.span] as const]
        : [],
    ),
  )
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const parameterNodes =
    list?.children.filter(
      (child): child is SyntaxTree.Node =>
        SyntaxTree.isNode(child) &&
        (child.kind === 'TypeParameter' || child.kind === 'LifetimeParameter'),
    ) ?? []
  for (const [ordinal, parameterNode] of parameterNodes.entries()) {
    const token = SyntaxTree.directToken(
      parameterNode,
      parameterNode.kind === 'LifetimeParameter' ? 'Lifetime' : 'Identifier',
    )
    if (token !== undefined && !environment.has(spelling(source, token)))
      environment.set(
        spelling(source, token),
        Type.parameter(
          { module: source.id, name: ownerName },
          ordinalOffset + ordinal,
          spelling(source, token),
          parameterNode.kind === 'LifetimeParameter' ? 'Lifetime' : 'Value',
        ),
      )
  }
  const lifetimeContext = DeclarationLifetime.forHeader(
    source,
    { module: source.id, name: ownerName },
    node,
    environment,
  )
  diagnostics.push(...lifetimeContext.diagnostics)
  const facts = parameterNodes.map((parameterNode, ordinal): TypeParameterFact => {
    const lifetimeToken = SyntaxTree.directToken(parameterNode, 'Lifetime')
    const name: DeclaredName =
      lifetimeToken === undefined
        ? presentName(source, parameterNode)
        : Object.freeze({
            _tag: 'Present',
            spelling: spelling(source, lifetimeToken),
            token: lifetimeToken,
          })
    if (lifetimeToken !== undefined && spelling(source, lifetimeToken) === "'static")
      diagnostics.push(
        Diagnostic.invalidLifetimeBinder(
          'static cannot be declared as a lifetime parameter',
          lifetimeToken.span,
        ),
      )
    const lifetimeBounds = parameterNode.children
      .filter(
        (child): child is SyntaxTree.Node =>
          SyntaxTree.isNode(child) && child.kind === 'LifetimeType',
      )
      .flatMap((bound) => {
        const token = SyntaxTree.directToken(bound, 'Lifetime')
        const region =
          token === undefined ? undefined : DeclarationLifetime.named(source, token, environment)
        if (region === undefined && token !== undefined)
          diagnostics.push(
            ...unreportedLifetimeDiagnostic(
              Diagnostic.unknownLifetime(spelling(source, token), token.span),
              lifetimeContext,
            ),
          )
        return region === undefined ? [] : [region]
      })
    // Every direct type node after the colon is one conjunct. Taking only direct children keeps
    // nested type arguments from being mistaken for sibling bounds.
    const boundNodes = parameterNode.children.filter(
      (child): child is SyntaxTree.Node =>
        SyntaxTree.isNode(child) && child.kind !== 'LifetimeType',
    )
    if (lifetimeToken !== undefined && boundNodes.length > 0)
      diagnostics.push(
        Diagnostic.invalidLifetimeBinder(
          'A lifetime parameter accepts only lifetime outlives bounds',
          parameterNode.span,
        ),
      )
    const boundNode = boundNodes.at(0)
    const boundResolution =
      boundNode === undefined
        ? undefined
        : analyzeDeclaredType(source, boundNode, environment, false, lifetimeContext)
    const effectBoundTarget =
      boundNode?.kind === 'AppliedType' ? SyntaxTree.directNode(boundNode, 'TypePath') : undefined
    const effectBoundSegments =
      effectBoundTarget === undefined
        ? []
        : SyntaxTree.tokens(effectBoundTarget).filter((token) => token.kind === 'Identifier')
    const effectBoundSegment = effectBoundSegments.at(0)
    const effectBound =
      effectBoundSegments.length === 1 &&
      effectBoundSegment !== undefined &&
      spelling(source, effectBoundSegment) === 'Effect'
    const staticPropertyOf = (
      candidate: SyntaxTree.Node,
    ): Type.SealedStaticProperty | undefined => {
      const segments = SyntaxTree.tokens(candidate)
        .filter((token) => token.kind === 'Identifier')
        .map((token) => spelling(source, token))
      if (segments.length !== 2 || segments.at(0) !== 'Intrinsic') return undefined
      const property = segments.at(1)
      return property === 'Detached' || property === 'NonParking'
        ? `Intrinsic.${property}`
        : undefined
    }
    let representationKind: Type.ParameterKind | undefined
    if (boundNode?.kind === 'CallableType') {
      representationKind = 'CallableRepresentation'
    } else if (effectBound) {
      representationKind = 'EffectRepresentation'
    } else {
      representationKind = undefined
    }
    const firstStaticProperty = boundNode === undefined ? undefined : staticPropertyOf(boundNode)
    const rawStaticProperties = (
      representationKind === undefined && firstStaticProperty !== undefined
        ? boundNodes
        : boundNodes.slice(1)
    ).map(staticPropertyOf)
    const staticPropertySet = new Set(
      rawStaticProperties.filter(
        (property): property is Type.SealedStaticProperty => property !== undefined,
      ),
    )
    const staticProperties: ReadonlyArray<Type.SealedStaticProperty> = Object.freeze(
      (['Intrinsic.Detached', 'Intrinsic.NonParking'] as const).filter((property) =>
        staticPropertySet.has(property),
      ),
    )
    const representationContract =
      boundResolution?.fact._tag === 'Resolved' &&
      (Type.isCallable(boundResolution.fact.type) || Type.isEffect(boundResolution.fact.type))
        ? boundResolution.fact.type
        : undefined
    if (representationKind !== undefined && boundResolution !== undefined) {
      diagnostics.push(...boundResolution.diagnostics)
      for (const [ordinal, property] of rawStaticProperties.entries()) {
        if (property !== undefined) continue
        const conjunct = boundNodes.at(ordinal + 1)
        const token =
          conjunct === undefined
            ? undefined
            : SyntaxTree.tokens(conjunct).find((candidate) => candidate.kind === 'Identifier')
        if (conjunct !== undefined && token !== undefined)
          diagnostics.push(
            Diagnostic.invalidExecutablePropertyConjunct(spelling(source, token), conjunct.span),
          )
      }
    }
    if (representationKind === undefined && staticProperties.includes('Intrinsic.NonParking')) {
      const propertyNode = boundNodes.find(
        (candidate) => staticPropertyOf(candidate) === 'Intrinsic.NonParking',
      )
      const token = propertyNode
        ? SyntaxTree.tokens(propertyNode).find((candidate) => candidate.kind === 'Identifier')
        : undefined
      if (propertyNode !== undefined && token !== undefined)
        diagnostics.push(
          Diagnostic.invalidExecutablePropertyConjunct(spelling(source, token), propertyNode.span),
        )
    }
    const bounds: ReadonlyArray<BoundFact> =
      representationKind !== undefined ||
      SyntaxTree.directToken(parameterNode, 'Colon') === undefined
        ? Object.freeze([])
        : Object.freeze(
            boundNodes.flatMap((candidate): ReadonlyArray<BoundFact> => {
              if (staticPropertyOf(candidate) !== undefined) return []
              const token = SyntaxTree.tokens(candidate).find((part) => part.kind === 'Identifier')
              if (token === undefined) return []
              const resolution = analyzeDeclaredType(
                source,
                candidate,
                environment,
                false,
                lifetimeContext,
              )
              return [
                Object.freeze({
                  _tag: 'UnresolvedBound' as const,
                  spelling: spelling(source, token),
                  path: Object.freeze({
                    _tag: 'TypePath' as const,
                    spelling: spelling(source, token),
                    segments: Object.freeze([
                      Object.freeze({ spelling: spelling(source, token), token }),
                    ]),
                    syntax: candidate,
                  }),
                  application: resolution.fact,
                }),
              ]
            }),
          )
    const duplicateOf =
      name._tag === 'Present' && originals.has(name.spelling)
        ? environment.get(name.spelling)
        : undefined
    let parameterKind: Type.ParameterKind = representationKind ?? 'Value'
    if (lifetimeToken !== undefined) parameterKind = 'Lifetime'
    else if (SyntaxTree.directToken(parameterNode, 'Question') !== undefined)
      parameterKind = 'RequirementRow'
    const type =
      duplicateOf ??
      Type.parameter(
        { module: source.id, name: ownerName },
        ordinalOffset + ordinal,
        name._tag === 'Present' ? name.spelling : `#${ordinal}`,
        parameterKind,
        representationContract,
        representationKind === undefined
          ? Object.freeze(staticProperties.filter((property) => property === 'Intrinsic.Detached'))
          : staticProperties,
      )
    if (name._tag === 'Present' && duplicateOf === undefined) {
      environment.set(name.spelling, type)
      originals.set(name.spelling, name.token.span)
    } else if (name._tag === 'Present') {
      const originalSpan = originals.get(name.spelling)
      if (originalSpan !== undefined) {
        diagnostics.push(
          Diagnostic.duplicateTypeParameter(name.spelling, originalSpan, name.token.span),
        )
      }
    }
    return Object.freeze({
      _tag: 'TypeParameterDeclaration' as const,
      type,
      name,
      syntax: parameterNode,
      bounds,
      lifetimeBounds: Object.freeze(lifetimeBounds),
      staticProperties:
        representationKind === undefined
          ? Object.freeze(staticProperties.filter((property) => property === 'Intrinsic.Detached'))
          : staticProperties,
      ...(duplicateOf === undefined ? {} : { duplicateOf }),
      ...(representationKind === undefined ||
      boundNode === undefined ||
      boundResolution === undefined
        ? {}
        : {
            representationBound: Object.freeze({
              _tag: 'RepresentationBound' as const,
              kind:
                representationKind === 'CallableRepresentation'
                  ? ('Callable' as const)
                  : ('Effect' as const),
              contract: boundResolution.fact,
              syntax: boundNode,
            }),
          }),
    })
  })
  const implicitFacts = implicitLifetimeParameters(lifetimeContext, environment)
  return Object.freeze({
    facts: Object.freeze([...facts, ...implicitFacts]),
    lifetimeContext: Object.freeze({
      ...lifetimeContext,
      parameters: new Map(
        [...environment].filter(
          ([name]) => !lifetimeContext.implicit.some((binder) => binder.parameter.name === name),
        ),
      ),
    }),
    environment,
    diagnostics: Object.freeze(diagnostics),
  })
}

const implicitLifetimeParameters = (
  lifetimeContext: DeclarationLifetime.Context,
  environment: Map<string, Type.Parameter>,
): ReadonlyArray<TypeParameterFact> =>
  lifetimeContext.implicit.map((binder) => {
    environment.set(binder.parameter.name, binder.parameter)
    return Object.freeze({
      _tag: 'TypeParameterDeclaration',
      type: binder.parameter,
      name: Object.freeze({
        _tag: 'Present',
        spelling: binder.parameter.name,
        token: binder.token,
      }),
      syntax: binder.syntax,
      bounds: Object.freeze([]),
      staticProperties: Object.freeze([]),
      lifetimeBounds: Object.freeze([]),
      implicitLifetime: true,
    })
  })

const collectReturnType = (
  source: SourceFile.SourceFile,
  returnSyntax: SyntaxTree.Node,
  ownerName: string,
  typeParameters: ReadonlyArray<TypeParameterFact>,
  ambientParameters: ReadonlyMap<string, Type.Parameter> = new Map(),
  lifetimeContext?: DeclarationLifetime.Context,
): {
  readonly fact: ReturnTypeFact
  readonly opaqueResult?: OpaqueResultFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  const syntax = declaredTypeNode(returnSyntax)
  if (syntax.kind !== 'OpaqueResultType') {
    const analyzed = analyzeDeclaredType(
      source,
      syntax,
      new Map([
        ...ambientParameters,
        ...typeParameters.flatMap((parameter) =>
          parameter.name._tag === 'Present'
            ? [[parameter.name.spelling, parameter.type] as const]
            : [],
        ),
      ]),
      false,
      lifetimeContext,
    )
    return Object.freeze({ fact: analyzed.fact, diagnostics: analyzed.diagnostics })
  }
  const collected = collectTypeParameters(
    source,
    syntax,
    ownerName,
    typeParameters.length,
    typeParameters,
  )
  const binder = collected.facts.at(0)
  const resultSyntax = syntax.children.find(isDeclaredTypeNode)
  if (binder === undefined || resultSyntax === undefined) {
    return Object.freeze({
      fact: Object.freeze({ _tag: 'Unavailable', syntax }),
      diagnostics: collected.diagnostics,
    })
  }
  const analyzed = analyzeDeclaredType(
    source,
    resultSyntax,
    new Map([...ambientParameters, ...collected.environment]),
    false,
    lifetimeContext,
  )
  return Object.freeze({
    fact: analyzed.fact,
    opaqueResult: Object.freeze({
      _tag: 'OpaqueResult',
      binder,
      family: Object.freeze({
        _tag: 'OpaqueFamilyKey',
        producer: Object.freeze({ module: source.id, name: ownerName }),
        binderOrdinal: 0,
      }),
      publicSignature: Object.freeze({
        bound:
          binder.type.representationBound === undefined
            ? 'unavailable'
            : Type.key(binder.type.representationBound),
        result:
          analyzed.fact._tag === 'Resolved' ? Type.key(analyzed.fact.type) : analyzed.fact._tag,
        enclosingKinds: Object.freeze(typeParameters.map((parameter) => parameter.type.kind)),
      }),
      syntax,
    }),
    diagnostics: Object.freeze([...collected.diagnostics, ...analyzed.diagnostics]),
  })
}

const parameterAtTypePath = (
  source: SourceFile.SourceFile,
  syntax: SyntaxTree.Node,
  typeParameters: ReadonlyMap<string, Type.Parameter>,
): Type.Parameter | undefined => {
  if (syntax.kind !== 'TypePath') return undefined
  const identifiers = SyntaxTree.tokens(syntax).filter((token) => token.kind === 'Identifier')
  const identifier = identifiers.at(0)
  return identifiers.length === 1 && identifier !== undefined
    ? typeParameters.get(spelling(source, identifier))
    : undefined
}

/**
 * `bareKeys` admits a service-role key written without access (`Clock`, `Clock at Primary`), the
 * form a `Without` operand names; elsewhere a bare path is a row parameter.
 */
const collectRowExpression = (
  source: SourceFile.SourceFile,
  syntax: SyntaxTree.Node,
  typeParameters: ReadonlyMap<string, Type.Parameter>,
  leaf: 'Failure' | 'Requirement',
  bareKeys = false,
  lifetimeContext?: DeclarationLifetime.Context,
): {
  readonly fact: RowExpressionFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  if (syntax.kind === 'RowWithout') {
    const operands = syntax.children.filter((element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element),
    )
    const left = operands.at(0)
    const right = operands.at(1)
    if (left === undefined || right === undefined)
      return Object.freeze({
        fact: Object.freeze({ _tag: 'UnavailableRowExpression', syntax }),
        diagnostics: Object.freeze([]),
      })
    const sourceRow = collectRowExpression(
      source,
      left,
      typeParameters,
      leaf,
      true,
      lifetimeContext,
    )
    const selected = collectRowExpression(
      source,
      right,
      typeParameters,
      leaf,
      true,
      lifetimeContext,
    )
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'WithoutRowExpression',
        source: sourceRow.fact,
        selected: selected.fact,
        syntax,
      }),
      diagnostics: Object.freeze([...sourceRow.diagnostics, ...selected.diagnostics]),
    })
  }
  if (syntax.kind === 'UnionType') {
    const collected = syntax.children
      .filter((element): element is SyntaxTree.Node => SyntaxTree.isNode(element))
      .map((operand) =>
        collectRowExpression(source, operand, typeParameters, leaf, bareKeys, lifetimeContext),
      )
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'UnionRowExpression',
        operands: Object.freeze(collected.map((operand) => operand.fact)),
        syntax,
      }),
      diagnostics: Object.freeze(collected.flatMap((operand) => operand.diagnostics)),
    })
  }
  if (leaf === 'Failure') {
    if (!isDeclaredTypeNode(syntax))
      return Object.freeze({
        fact: Object.freeze({ _tag: 'UnavailableRowExpression', syntax }),
        diagnostics: Object.freeze([]),
      })
    const analyzed = analyzeDeclaredType(source, syntax, typeParameters, false, lifetimeContext)
    return Object.freeze({
      fact: Object.freeze({ _tag: 'FailureMemberExpression', member: analyzed.fact, syntax }),
      diagnostics: analyzed.diagnostics,
    })
  }
  const parameter = parameterAtTypePath(source, syntax, typeParameters)
  if (parameter?.kind === 'RequirementRow')
    return Object.freeze({
      fact: Object.freeze({ _tag: 'RowParameterExpression', parameter, syntax }),
      diagnostics: Object.freeze([]),
    })
  if (bareKeys && syntax.kind === 'TypePath') {
    const analyzed = analyzeDeclaredType(source, syntax, typeParameters, false, lifetimeContext)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'RequirementMemberExpression',
        capability: analyzed.fact,
        access: 'Shared',
        role: Object.freeze({ _tag: 'DefaultRole' }),
        syntax,
      }),
      diagnostics: analyzed.diagnostics,
    })
  }
  if (syntax.kind !== 'Requirement' && syntax.kind !== 'ReferenceType')
    return Object.freeze({
      fact: Object.freeze({ _tag: 'UnavailableRowExpression', syntax }),
      diagnostics: Object.freeze([]),
    })
  const capabilitySyntax = syntax.children.find(isDeclaredTypeNode)
  if (capabilitySyntax === undefined)
    return Object.freeze({
      fact: Object.freeze({ _tag: 'UnavailableRowExpression', syntax }),
      diagnostics: Object.freeze([]),
    })
  const analyzed = analyzeDeclaredType(
    source,
    capabilitySyntax,
    typeParameters,
    false,
    lifetimeContext,
  )
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'RequirementMemberExpression',
      capability: analyzed.fact,
      access: SyntaxTree.directToken(syntax, 'MutKeyword') === undefined ? 'Shared' : 'Exclusive',
      role: collectedRequirementRole(source, syntax),
      syntax,
    }),
    diagnostics: analyzed.diagnostics,
  })
}

const emptyRowExpression: RowExpressionFact = Object.freeze({ _tag: 'EmptyRowExpression' })

/** Joins one requirement row's member nodes into a single row expression. */
const rowExpressionOf = (
  source: SourceFile.SourceFile,
  syntax: SyntaxTree.Node,
  rowNodes: ReadonlyArray<SyntaxTree.Node>,
  typeParameters: ReadonlyMap<string, Type.Parameter>,
  lifetimeContext?: DeclarationLifetime.Context,
): RowExpressionFact =>
  rowNodes
    .map((member) =>
      collectRowExpression(source, member, typeParameters, 'Requirement', false, lifetimeContext),
    )
    .reduce<RowExpressionFact>(
      (left, right) =>
        left._tag === 'EmptyRowExpression'
          ? right.fact
          : Object.freeze({
              _tag: 'UnionRowExpression',
              operands: Object.freeze([left, right.fact]),
              syntax,
            }),
      emptyRowExpression,
    )
const emptyFailureRow = RowAlgebra.concrete(Type.failureRowPolicy(), [])
const emptyRequirementRow = RowAlgebra.concrete(Type.requirementRowPolicy(), [])

const collectFailureRow = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  typeParameters: ReadonlyMap<string, Type.Parameter>,
  lifetimeContext?: DeclarationLifetime.Context,
): {
  readonly fact: FailureRowFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  const syntax = SyntaxTree.directNode(node, 'FailureRow')
  if (syntax === undefined)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'FailureRow',
        members: Object.freeze([]),
        parameters: Object.freeze([]),
        failures: Object.freeze([]),
        available: true,
        expression: emptyRowExpression,
        row: emptyFailureRow,
      }),
      diagnostics: Object.freeze([]),
    })
  const declared = syntax.children.find((element): element is SyntaxTree.Node =>
    SyntaxTree.isNode(element),
  )
  if (declared === undefined)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'FailureRow',
        members: Object.freeze([]),
        parameters: Object.freeze([]),
        failures: Object.freeze([]),
        syntax,
        available: false,
        expression: Object.freeze({ _tag: 'UnavailableRowExpression', syntax }),
        row: emptyFailureRow,
      }),
      diagnostics: Object.freeze([]),
    })
  const expression = collectRowExpression(
    source,
    declared,
    typeParameters,
    'Failure',
    false,
    lifetimeContext,
  )
  let syntaxMembers: readonly SyntaxTree.Node[]
  if (declared.kind === 'UnionType') {
    syntaxMembers = declared.children.filter(isDeclaredTypeNode)
  } else if (isDeclaredTypeNode(declared)) {
    syntaxMembers = Object.freeze([declared])
  } else {
    syntaxMembers = Object.freeze([])
  }
  // The legacy member facts remain the single diagnostic owner while the row
  // expression is retained as the semantic shape. Reporting both would emit
  // the same kind/type error twice for one source member.
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const members = syntaxMembers.flatMap((member): ReadonlyArray<DeclaredTypeFact> => {
    const parameter = parameterAtTypePath(source, member, typeParameters)
    if (parameter?.kind === 'RequirementRow') {
      const token = SyntaxTree.directToken(member, 'Identifier')
      if (token !== undefined)
        diagnostics.push(
          Diagnostic.genericParameterKindMismatch(
            spelling(source, token),
            'Value',
            parameter.kind,
            token.span,
          ),
        )
      return []
    }
    const analyzed = analyzeDeclaredType(source, member, typeParameters, false, lifetimeContext)
    diagnostics.push(...analyzed.diagnostics)
    return [analyzed.fact]
  })
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'FailureRow',
      members: Object.freeze(members),
      parameters: Object.freeze([]),
      failures: Object.freeze([]),
      syntax,
      available: false,
      expression: expression.fact,
      row: emptyFailureRow,
    }),
    diagnostics: Object.freeze(diagnostics),
  })
}

const collectRequirementRow = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  typeParameters: ReadonlyMap<string, Type.Parameter>,
  lifetimeContext?: DeclarationLifetime.Context,
): {
  readonly fact: RequirementRowFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  const syntax = SyntaxTree.directNode(node, 'RequirementRow')
  if (syntax === undefined)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'RequirementRow',
        entries: Object.freeze([]),
        parameters: Object.freeze([]),
        requirements: Object.freeze([]),
        available: true,
        expression: emptyRowExpression,
        row: emptyRequirementRow,
      }),
      diagnostics: Object.freeze([]),
    })
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const rowNodes = syntax.children.filter((element): element is SyntaxTree.Node =>
    SyntaxTree.isNode(element),
  )
  // Entry collection below owns source diagnostics. The expression facts are
  // structural and must not duplicate the same diagnostic occurrence.
  const expression = rowExpressionOf(source, syntax, rowNodes, typeParameters, lifetimeContext)
  const entries = SyntaxTree.directNodes(syntax, 'Requirement').map((requirement) => {
    const capabilitySyntax = requirement.children.find(isDeclaredTypeNode)
    const analyzed =
      capabilitySyntax === undefined
        ? Object.freeze({
            fact: Object.freeze({ _tag: 'Unavailable' as const, syntax: requirement }),
            diagnostics: Object.freeze<ReadonlyArray<Diagnostic.Diagnostic>>([]),
          })
        : analyzeDeclaredType(source, capabilitySyntax, typeParameters, false, lifetimeContext)
    diagnostics.push(...analyzed.diagnostics)
    return Object.freeze({
      capability: analyzed.fact,
      role: collectedRequirementRole(source, requirement),
      access:
        SyntaxTree.directToken(requirement, 'MutKeyword') === undefined
          ? ('Shared' as const)
          : ('Exclusive' as const),
      syntax: requirement,
    })
  })
  const parameters = SyntaxTree.directNodes(syntax, 'TypePath').flatMap(
    (path): ReadonlyArray<Type.Parameter> => {
      const token = SyntaxTree.directToken(path, 'Identifier')
      const parameter = parameterAtTypePath(source, path, typeParameters)
      if (parameter?.kind === 'RequirementRow') return [parameter]
      if (token !== undefined) {
        if (parameter === undefined)
          diagnostics.push(Diagnostic.unknownType(spelling(source, token), token.span))
        else
          diagnostics.push(
            Diagnostic.genericParameterKindMismatch(
              spelling(source, token),
              'RequirementRow',
              parameter.kind,
              token.span,
            ),
          )
      }
      return []
    },
  )
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'RequirementRow',
      entries: Object.freeze(entries),
      parameters: Object.freeze(parameters),
      requirements: Object.freeze([]),
      syntax,
      available: false,
      expression,
      row: emptyRequirementRow,
    }),
    diagnostics: Object.freeze(diagnostics),
  })
}

const nestedNodes = (syntax: SyntaxTree.Node): ReadonlyArray<SyntaxTree.Node> =>
  syntax.children.flatMap((element): ReadonlyArray<SyntaxTree.Node> =>
    SyntaxTree.isNode(element) ? [element, ...nestedNodes(element)] : [],
  )

const constraintDomain = (
  source: SourceFile.SourceFile,
  syntax: SyntaxTree.Node,
  typeParameters: ReadonlyMap<string, Type.Parameter>,
): 'Failure' | 'Requirement' => {
  for (const path of [syntax, ...nestedNodes(syntax)]) {
    const parameter = parameterAtTypePath(source, path, typeParameters)
    if (parameter?.kind === 'RequirementRow') return 'Requirement'
  }
  return 'Failure'
}

const collectConstraints = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  typeParameters: ReadonlyMap<string, Type.Parameter>,
): {
  readonly facts: ReadonlyArray<ConstraintFact>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  const where = SyntaxTree.directNode(node, 'WhereClause')
  if (where === undefined)
    return Object.freeze({ facts: Object.freeze([]), diagnostics: Object.freeze([]) })
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const facts = where.children.flatMap((element): ReadonlyArray<ConstraintFact> => {
    if (!SyntaxTree.isNode(element)) return []
    const operands = element.children.filter((child): child is SyntaxTree.Node =>
      SyntaxTree.isNode(child),
    )
    if (element.kind === 'MembershipConstraint') {
      const selectedSyntax = operands.at(0)
      const sourceSyntax = operands.at(1)
      if (selectedSyntax === undefined || sourceSyntax === undefined) return []
      const domain = constraintDomain(source, sourceSyntax, typeParameters)
      const selected =
        domain === 'Requirement'
          ? collectRowExpression(source, selectedSyntax, typeParameters, 'Requirement')
          : collectRowExpression(source, selectedSyntax, typeParameters, 'Failure')
      const sourceRow =
        domain === 'Requirement'
          ? collectRowExpression(source, sourceSyntax, typeParameters, 'Requirement')
          : collectRowExpression(source, sourceSyntax, typeParameters, 'Failure')
      diagnostics.push(...selected.diagnostics, ...sourceRow.diagnostics)
      return [
        Object.freeze({
          _tag: 'MembershipConstraint',
          domain,
          selected: selected.fact,
          source: sourceRow.fact,
          syntax: element,
        }),
      ]
    }
    if (element.kind !== 'ProviderConstraint') return []
    const providerSyntax = operands.at(0)
    const selectedSyntax = operands.at(1)
    const sourceSyntax = operands.at(2)
    if (providerSyntax === undefined || selectedSyntax === undefined || sourceSyntax === undefined)
      return []
    const providerTypeSyntax =
      providerSyntax.kind === 'ReferenceType'
        ? providerSyntax.children.find(isDeclaredTypeNode)
        : providerSyntax
    const provider =
      providerTypeSyntax === undefined
        ? Object.freeze({
            fact: Object.freeze({ _tag: 'Unavailable' as const, syntax: providerSyntax }),
            diagnostics: Object.freeze<ReadonlyArray<Diagnostic.Diagnostic>>([]),
          })
        : analyzeDeclaredType(source, providerTypeSyntax, typeParameters)
    const selected = collectRowExpression(source, selectedSyntax, typeParameters, 'Requirement')
    const sourceRow = collectRowExpression(source, sourceSyntax, typeParameters, 'Requirement')
    diagnostics.push(...provider.diagnostics, ...selected.diagnostics, ...sourceRow.diagnostics)
    let mode: Type.CallableMode
    if (providerSyntax.kind !== 'ReferenceType') mode = 'Take'
    else if (SyntaxTree.directToken(providerSyntax, 'MutKeyword') === undefined) mode = 'Shared'
    else mode = 'Exclusive'
    return [
      Object.freeze({
        _tag: 'ProviderConstraint',
        mode,
        provider: provider.fact,
        selected: selected.fact,
        source: sourceRow.fact,
        syntax: element,
      }),
    ]
  })
  return Object.freeze({ facts: Object.freeze(facts), diagnostics: Object.freeze(diagnostics) })
}

const tightSpan = (syntax: SyntaxTree.Element): SourceSpan.SourceSpan => {
  if (!SyntaxTree.isNode(syntax)) return syntax.span
  const tokens = SyntaxTree.tokens(syntax).filter(
    (token) =>
      token.kind !== 'Whitespace' &&
      token.kind !== 'LineComment' &&
      token.kind !== 'DocComment' &&
      token.kind !== 'ModuleDocComment' &&
      token.kind !== 'EndOfFile',
  )
  const first = tokens.at(0)
  const last = tokens.at(-1)
  return first === undefined || last === undefined
    ? syntax.span
    : (SourceSpan.fromOffsets(syntax.span.sourceId, first.span.start, last.span.end) ?? syntax.span)
}

const enumRepresentation = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  diagnostics: Array<Diagnostic.Diagnostic>,
): EnumRepresentationFact => {
  const explicit = SyntaxTree.directToken(node, 'LeftParenthesis') !== undefined
  if (!explicit)
    return Object.freeze({
      _tag: 'Available',
      scalar: Scalar.defaultEnumRepresentation,
      explicit: false,
      syntax: node,
    })
  const syntax = node.children.find(isDeclaredTypeNode)
  if (syntax === undefined || !SyntaxTree.isAvailableSyntax(syntax))
    return Object.freeze({
      _tag: 'Unavailable',
      explicit: true,
      syntax: syntax ?? node,
    })
  const token = SyntaxTree.tokens(syntax).find((candidate) => candidate.kind === 'Identifier')
  const representationSpelling = token === undefined ? '<unavailable>' : spelling(source, token)
  const scalar = Scalar.enumRepresentation(representationSpelling)
  if (scalar !== undefined)
    return Object.freeze({ _tag: 'Available', scalar, explicit: true, syntax })
  const diagnostic = Diagnostic.unsupportedEnumRepresentation(
    representationSpelling,
    Scalar.enumRepresentations().map((candidate) => candidate.spelling),
    tightSpan(syntax),
  )
  diagnostics.push(diagnostic)
  return Object.freeze({
    _tag: 'Unavailable',
    explicit: true,
    syntax,
    spelling: representationSpelling,
    cause: Diagnostic.identity(diagnostic),
  })
}

const collectEnum = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  id: DeclarationId,
  canonical: CanonicalState,
  visibility: 'Private' | 'Public',
  name: DeclaredName,
  diagnostics: Array<Diagnostic.Diagnostic>,
): EnumFact => {
  const enumDiagnostics: Array<Diagnostic.Diagnostic> = []
  const representation = enumRepresentation(source, node, enumDiagnostics)
  const memberNodes = SyntaxTree.directNodes(node, 'EnumMember')
  if (memberNodes.length === 0) {
    enumDiagnostics.push(
      Diagnostic.emptyEnum(
        name._tag === 'Present' ? name.spelling : '<anonymous>',
        tightSpan(node),
      ),
    )
  }
  const firstNames = new Map<
    string,
    {
      readonly id: EnumMemberId
      readonly canonical?: CanonicalEnumMemberId
      readonly token: Token.Token
    }
  >()
  const firstDiscriminants = new Map<bigint, SyntaxTree.Node>()
  const representationScalar =
    representation._tag === 'Available' ? representation.scalar : undefined
  const range =
    representationScalar === undefined ? undefined : Scalar.range(representationScalar, 64)
  let previous: bigint | undefined
  const members = memberNodes.map((memberNode, ordinal): EnumMemberFact => {
    const memberId: EnumMemberId = Object.freeze({ _tag: 'EnumMemberId', enum: id, ordinal })
    const memberName = presentName(source, memberNode)
    let memberCanonical: EnumMemberFact['canonical'] = Object.freeze({ _tag: 'Unidentified' })
    if (memberName._tag === 'Present') {
      const original = firstNames.get(memberName.spelling)
      if (original === undefined) {
        const canonicalMember =
          canonical._tag === 'Canonical'
            ? Object.freeze({
                _tag: 'CanonicalEnumMemberId' as const,
                enum: canonical.id,
                name: memberName.spelling,
              })
            : undefined
        firstNames.set(
          memberName.spelling,
          Object.freeze({
            id: memberId,
            token: memberName.token,
            ...(canonicalMember === undefined ? {} : { canonical: canonicalMember }),
          }),
        )
        if (canonicalMember !== undefined)
          memberCanonical = Object.freeze({ _tag: 'Canonical', id: canonicalMember })
      } else {
        const diagnostic = Diagnostic.duplicateEnumMemberName(
          memberName.spelling,
          original.token.span,
          memberName.token.span,
        )
        enumDiagnostics.push(diagnostic)
        if (original.canonical !== undefined)
          memberCanonical = Object.freeze({
            _tag: 'Duplicate',
            original: original.canonical,
            cause: Diagnostic.identity(diagnostic),
          })
      }
    }

    const explicitSyntax = SyntaxTree.directNode(memberNode, 'IntegerLiteralExpression')
    const sourceKind: EnumDiscriminantFact['source'] =
      explicitSyntax === undefined ? 'Implicit' : 'Explicit'
    let attempted: bigint | undefined
    if (explicitSyntax !== undefined) {
      const literal = constantLiteral(source, explicitSyntax)
      if (literal._tag === 'IntegerLiteral') attempted = literal.value
    } else if (ordinal === 0) attempted = 0n
    else if (previous !== undefined) attempted = previous + 1n

    let discriminant: EnumDiscriminantFact
    const discriminantSyntax = explicitSyntax ?? memberNode
    if (attempted === undefined || range === undefined || representationScalar === undefined) {
      discriminant = Object.freeze({
        _tag: 'Unavailable',
        source: sourceKind,
        syntax: discriminantSyntax,
        ...(attempted === undefined ? {} : { attempted }),
        ...(representation._tag === 'Unavailable' && representation.cause !== undefined
          ? { cause: representation.cause }
          : {}),
      })
    } else if (
      sourceKind === 'Explicit' &&
      representationScalar.signedness === 'Unsigned' &&
      attempted < 0n
    ) {
      const diagnostic = Diagnostic.unsignedEnumNegativeDiscriminant(
        representationScalar.spelling,
        attempted,
        tightSpan(discriminantSyntax),
      )
      enumDiagnostics.push(diagnostic)
      discriminant = Object.freeze({
        _tag: 'Unavailable',
        source: sourceKind,
        syntax: discriminantSyntax,
        attempted,
        cause: Diagnostic.identity(diagnostic),
      })
    } else if (attempted < range.minimum || attempted > range.maximum) {
      const diagnostic =
        sourceKind === 'Explicit'
          ? Diagnostic.enumDiscriminantOutOfRange(
              representationScalar.spelling,
              attempted,
              range.minimum,
              range.maximum,
              tightSpan(discriminantSyntax),
            )
          : Diagnostic.enumImplicitDiscriminantOverflow(
              representationScalar.spelling,
              previous ?? range.maximum,
              range.maximum,
              tightSpan(memberNode),
            )
      enumDiagnostics.push(diagnostic)
      discriminant = Object.freeze({
        _tag: 'Unavailable',
        source: sourceKind,
        syntax: discriminantSyntax,
        attempted,
        cause: Diagnostic.identity(diagnostic),
      })
    } else {
      const original = firstDiscriminants.get(attempted)
      if (original === undefined) {
        firstDiscriminants.set(attempted, memberNode)
        discriminant = Object.freeze({
          _tag: 'Available',
          value: attempted,
          source: sourceKind,
          syntax: discriminantSyntax,
        })
      } else {
        const diagnostic = Diagnostic.duplicateEnumDiscriminant(
          attempted,
          tightSpan(original),
          tightSpan(memberNode),
        )
        enumDiagnostics.push(diagnostic)
        discriminant = Object.freeze({
          _tag: 'Unavailable',
          source: sourceKind,
          syntax: discriminantSyntax,
          attempted,
          cause: Diagnostic.identity(diagnostic),
        })
      }
    }
    previous =
      attempted !== undefined &&
      range !== undefined &&
      attempted >= range.minimum &&
      attempted <= range.maximum
        ? attempted
        : undefined
    return Object.freeze({
      _tag: 'EnumMember',
      id: memberId,
      canonical: memberCanonical,
      name: memberName,
      discriminant,
      syntax: memberNode,
    })
  })
  diagnostics.push(...enumDiagnostics)
  const associatedOperation = enumValueOperation({ canonical, representation })
  const valid =
    enumDiagnostics.length === 0 &&
    representation._tag === 'Available' &&
    members.every(
      (member) =>
        member.name._tag === 'Present' &&
        member.canonical._tag === 'Canonical' &&
        member.discriminant._tag === 'Available',
    )
  return Object.freeze({
    _tag: 'EnumDeclaration',
    id,
    canonical,
    visibility,
    typeParameters: Object.freeze([]),
    name,
    representation,
    members: Object.freeze(members),
    associatedOperations: Object.freeze(
      associatedOperation === undefined ? [] : [associatedOperation],
    ),
    validity: valid
      ? Object.freeze({ _tag: 'Valid' })
      : Object.freeze({
          _tag: 'Invalid',
          causes: Object.freeze(
            enumDiagnostics.map((diagnostic) => Diagnostic.identity(diagnostic)),
          ),
        }),
    syntax: node,
  })
}

const collectUnion = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  id: DeclarationId,
  canonical: CanonicalState,
  visibility: 'Private' | 'Public',
  name: DeclaredName,
  typeParameters: ReturnType<typeof collectTypeParameters>,
  diagnostics: Array<Diagnostic.Diagnostic>,
): UnionFact => {
  const unionDiagnostics: Array<Diagnostic.Diagnostic> = []
  const variantNodes = SyntaxTree.directNodes(node, 'UnionVariant')
  if (variantNodes.length === 0) {
    unionDiagnostics.push(
      Diagnostic.emptyNominalUnion(
        name._tag === 'Present' ? name.spelling : '<anonymous>',
        tightSpan(node),
      ),
    )
  }
  const first = new Map<
    string,
    {
      readonly id: UnionVariantId
      readonly canonical?: CanonicalUnionVariantId
      readonly token: Token.Token
    }
  >()
  const variants = variantNodes.map((variantNode, ordinal): UnionVariantFact => {
    const variantId: UnionVariantId = Object.freeze({ _tag: 'UnionVariantId', union: id, ordinal })
    const variantName = presentName(source, variantNode)
    let variantCanonical: UnionVariantFact['canonical'] = Object.freeze({
      _tag: 'Unidentified',
    })
    if (variantName._tag === 'Present') {
      const original = first.get(variantName.spelling)
      if (original === undefined) {
        const canonicalVariant =
          canonical._tag === 'Canonical'
            ? Object.freeze({
                _tag: 'CanonicalUnionVariantId' as const,
                union: canonical.id,
                name: variantName.spelling,
              })
            : undefined
        first.set(
          variantName.spelling,
          Object.freeze({
            id: variantId,
            token: variantName.token,
            ...(canonicalVariant === undefined ? {} : { canonical: canonicalVariant }),
          }),
        )
        if (canonicalVariant !== undefined) {
          variantCanonical = Object.freeze({ _tag: 'Canonical', id: canonicalVariant })
        }
      } else {
        const diagnostic = Diagnostic.duplicateUnionVariant(
          variantName.spelling,
          original.token.span,
          variantName.token.span,
        )
        unionDiagnostics.push(diagnostic)
        if (original.canonical !== undefined) {
          variantCanonical = Object.freeze({
            _tag: 'Duplicate',
            original: original.canonical,
            cause: Diagnostic.identity(diagnostic),
          })
        }
      }
    }
    const hasFieldBody = SyntaxTree.directToken(variantNode, 'LeftBrace') !== undefined
    const fieldNodes = SyntaxTree.directNodes(variantNode, 'UnionVariantField')
    if (
      hasFieldBody &&
      fieldNodes.every(
        (fieldNode) =>
          SyntaxTree.tokens(fieldNode).filter(
            (token) =>
              token.kind !== 'Whitespace' &&
              token.kind !== 'LineComment' &&
              token.kind !== 'DocComment' &&
              token.kind !== 'ModuleDocComment',
          ).length === 0,
      )
    ) {
      unionDiagnostics.push(
        Diagnostic.emptyUnionVariant(
          variantName._tag === 'Present' ? variantName.spelling : '<anonymous>',
          tightSpan(variantNode),
        ),
      )
    }
    const collected = collectFields(
      source,
      variantNode,
      Object.freeze({ _tag: 'UnionVariantFieldOwnerId', variant: variantId }),
      'UnionVariantField',
      typeParameters.environment,
      typeParameters.lifetimeContext,
    )
    unionDiagnostics.push(...collected.diagnostics)
    return Object.freeze({
      _tag: 'UnionVariant',
      id: variantId,
      canonical: variantCanonical,
      name: variantName,
      kind: hasFieldBody ? 'Fields' : 'Unit',
      fields: collected.fields,
      syntax: variantNode,
    })
  })
  diagnostics.push(...unionDiagnostics)
  const valid =
    unionDiagnostics.length === 0 &&
    SyntaxTree.isAvailableSyntax(node) &&
    variants.length > 0 &&
    variants.every(
      (variant) =>
        variant.name._tag === 'Present' &&
        variant.canonical._tag === 'Canonical' &&
        variant.fields.every((field) => field.state._tag === 'Unique'),
    )
  return Object.freeze({
    _tag: 'UnionDeclaration',
    lifetimeElaboration: typeParameters.lifetimeContext,
    id,
    canonical,
    visibility,
    typeParameters: typeParameters.facts,
    name,
    variants: Object.freeze(variants),
    dependency: Object.freeze({ _tag: 'Available', types: Object.freeze([]) }),
    validity: valid
      ? Object.freeze({ _tag: 'Valid' })
      : Object.freeze({
          _tag: 'Invalid',
          causes: Object.freeze(
            unionDiagnostics.map((diagnostic) => Diagnostic.identity(diagnostic)),
          ),
        }),
    syntax: node,
  })
}

const decodedText = (source: SourceFile.SourceFile, token: Token.Token): string | undefined => {
  const bytes = Option.getOrUndefined(SourceFile.slice(source, token.span))
  const form = bytes === undefined ? undefined : LiteralForm.recognize(bytes)
  if (bytes === undefined || form === undefined) return undefined
  const decoded = StaticText.decode(Array.from(bytes), form)
  return decoded._tag === 'Decoded'
    ? textDecoder.decode(Uint8Array.from(decoded.data.bytes))
    : undefined
}

/** A malformed or unsupported foreign marker is retained without granting a layout promise. */
const collectStructLayout = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
): {
  readonly fact: StructFact['layout']
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  const marker = SyntaxTree.directToken(node, 'ExternKeyword')
  if (marker === undefined)
    return Object.freeze({
      fact: Object.freeze({ _tag: 'Silk' }),
      diagnostics: Object.freeze([]),
    })
  const abiToken = SyntaxTree.directToken(node, 'TextLiteral')
  if (abiToken === undefined)
    return Object.freeze({
      fact: Object.freeze({ _tag: 'InvalidForeign', abi: undefined, abiSpan: marker.span }),
      diagnostics: Object.freeze([]),
    })
  const abi = decodedText(source, abiToken)
  if (abi === 'C')
    return Object.freeze({
      fact: Object.freeze({ _tag: 'Foreign', abi, abiSpan: abiToken.span }),
      diagnostics: Object.freeze([]),
    })
  return Object.freeze({
    fact: Object.freeze({ _tag: 'InvalidForeign', abi, abiSpan: abiToken.span }),
    diagnostics: Object.freeze([Diagnostic.unsupportedForeignAbi(abi ?? '', abiToken.span)]),
  })
}

const textDecoder = new TextDecoder()

const sharedForeignRestrictions: ReadonlyArray<
  readonly [kind: Token.TokenKind | SyntaxTree.NodeKind, restriction: string]
> = [
  ['StaticKeyword', 'static'],
  ['EffectKeyword', 'effect'],
  ['TypeParameterList', 'type parameters'],
  ['FailureRow', 'failure row'],
  ['RequirementRow', 'requirement row'],
  ['WhereClause', 'where clause'],
]

/** A foreign header has no body; exports permit unsafe only for explicit boundary promises. */
const foreignRestrictions: Record<'Foreign' | 'Export', typeof sharedForeignRestrictions> = {
  Foreign: [...sharedForeignRestrictions, ['Block', 'body']],
  Export: [...sharedForeignRestrictions, ['UnsafeKeyword', 'unsafe']],
}

/**
 * The syntax-level checks shared by `extern` and `export` headers: ABI, the mandatory `unsafe` on
 * a foreign header, retained Silk-only syntax, and the native symbol. Type admission needs
 * resolved types and runs at completion.
 */
const collectForeign = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  name: DeclaredName,
  direction: 'Foreign' | 'Export',
  parameters: ReadonlyArray<ParameterFact>,
): {
  readonly fact: NonNullable<DeclarationFact['foreign']>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const declarationSpan = name._tag === 'Present' ? name.token.span : node.span
  const spellingOf = name._tag === 'Present' ? name.spelling : '#foreign'
  const abiToken = SyntaxTree.directToken(node, 'TextLiteral')
  // A missing ABI literal is already a parser diagnostic.
  if (abiToken !== undefined) {
    const abi = decodedText(source, abiToken)
    if (abi !== 'C') diagnostics.push(Diagnostic.unsupportedForeignAbi(abi ?? '', abiToken.span))
  }
  if (direction === 'Foreign' && SyntaxTree.directToken(node, 'UnsafeKeyword') === undefined)
    diagnostics.push(Diagnostic.foreignFunctionRequiresUnsafe(spellingOf, declarationSpan))
  for (const child of node.children) {
    const kind = SyntaxTree.isMissingToken(child) ? undefined : child.kind
    const restriction = foreignRestrictions[direction].find(([expected]) => expected === kind)?.[1]
    if (
      restriction !== undefined &&
      !(
        direction === 'Export' &&
        SyntaxTree.isNode(child) &&
        child.kind === 'TypeParameterList' &&
        child.children
          .filter(SyntaxTree.isNode)
          .every((parameter) => parameter.kind === 'LifetimeParameter')
      ) &&
      !(
        direction === 'Export' &&
        kind === 'UnsafeKeyword' &&
        (MachineFunction.analyze(source, node).properties !== undefined ||
          DeclarationProperty.clauses(node).some(
            (clause) => DeclarationProperty.owner(source, clause) === 'Intrinsic.foreign',
          ))
      )
    )
      diagnostics.push(Diagnostic.foreignDeclarationRestriction(restriction, child.span))
  }
  const asIndex = node.children.findIndex(
    (child) => SyntaxTree.isToken(child) && child.kind === 'AsKeyword',
  )
  const symbolToken = node.children
    .slice(asIndex + 1)
    .find(
      (child): child is Token.Token => SyntaxTree.isToken(child) && child.kind === 'TextLiteral',
    )
  const renamed =
    asIndex >= 0 && symbolToken !== undefined ? decodedText(source, symbolToken) : undefined
  const symbol = renamed ?? spellingOf
  const symbolSpan =
    renamed === undefined ? declarationSpan : (symbolToken?.span ?? declarationSpan)
  if (!ForeignSymbol.isValidSpelling(symbol))
    diagnostics.push(Diagnostic.invalidForeignSymbol(symbol, symbolSpan))
  else if (ForeignSymbol.isReserved(symbol))
    diagnostics.push(Diagnostic.reservedForeignSymbol(symbol, symbolSpan))
  const behavior = ForeignContract.analyze(
    source,
    DeclarationProperty.clauses(node).find(
      (clause) =>
        !['Intrinsic.native', 'Intrinsic.machine'].includes(
          DeclarationProperty.owner(source, clause),
        ),
    ),
    parameters.map((parameter) => ({
      name: parameter.name._tag === 'Present' ? parameter.name.spelling : '',
      type: undefined,
      span: parameter.syntax.span,
    })),
    undefined,
  )
  if (
    direction === 'Export' &&
    ForeignContract.key(behavior.contract) !== ForeignContract.key(ForeignContract.conservative) &&
    SyntaxTree.directToken(node, 'UnsafeKeyword') === undefined
  )
    diagnostics.push(
      Diagnostic.foreignDeclarationRestriction(
        'stronger exported foreign contracts require unsafe export',
        node.span,
      ),
    )
  diagnostics.push(...behavior.diagnostics)
  return Object.freeze({
    fact: Object.freeze({ abi: 'C' as const, symbol, contract: behavior.contract }),
    diagnostics: Object.freeze(diagnostics),
  })
}

/** Collects the ABI and symbol spelling shared by imported and exported data declarations. */
const collectForeignStatic = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  name: DeclaredName,
): {
  readonly fact: ForeignStaticFact['foreign']
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const declarationSpan = name._tag === 'Present' ? name.token.span : node.span
  const spellingOf = name._tag === 'Present' ? name.spelling : '#foreign-static'
  const abiToken = SyntaxTree.directToken(node, 'TextLiteral')
  if (abiToken !== undefined) {
    const abi = decodedText(source, abiToken)
    if (abi !== 'C') diagnostics.push(Diagnostic.unsupportedForeignAbi(abi ?? '', abiToken.span))
  }
  const asIndex = node.children.findIndex(
    (child) => SyntaxTree.isToken(child) && child.kind === 'AsKeyword',
  )
  const symbolToken = node.children
    .slice(asIndex + 1)
    .find(
      (child): child is Token.Token => SyntaxTree.isToken(child) && child.kind === 'TextLiteral',
    )
  const renamed =
    asIndex >= 0 && symbolToken !== undefined ? decodedText(source, symbolToken) : undefined
  const symbol = renamed ?? spellingOf
  const symbolSpan =
    renamed === undefined ? declarationSpan : (symbolToken?.span ?? declarationSpan)
  if (!ForeignSymbol.isValidSpelling(symbol))
    diagnostics.push(Diagnostic.invalidForeignSymbol(symbol, symbolSpan))
  else if (ForeignSymbol.isReserved(symbol))
    diagnostics.push(Diagnostic.reservedForeignSymbol(symbol, symbolSpan))
  return Object.freeze({
    fact: Object.freeze({ abi: 'C' as const, symbol }),
    diagnostics: Object.freeze(diagnostics),
  })
}

const collectModule = (
  syntax: SyntaxFile.SyntaxFile,
  declarations: ReadonlyArray<SyntaxTree.Node>,
  imports: ModuleClosure.Module['imports'],
): ModuleHeaders => {
  const source = syntax.source
  const nodes = declarations.filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) &&
      (element.kind === 'FunctionDeclaration' ||
        element.kind === 'ForeignFunctionDeclaration' ||
        element.kind === 'StructDeclaration' ||
        element.kind === 'TupleDeclaration' ||
        element.kind === 'EnumDeclaration' ||
        element.kind === 'UnionDeclaration' ||
        element.kind === 'ServiceDeclaration' ||
        element.kind === 'InterfaceDeclaration' ||
        element.kind === 'RoleDeclaration' ||
        element.kind === 'ConstantDeclaration' ||
        element.kind === 'PackageParameterDeclaration' ||
        element.kind === 'ForeignStaticDeclaration' ||
        element.kind === 'ExportStaticDeclaration' ||
        element.kind === 'TypeAliasDeclaration'),
  )
  const first = new Map<string, { readonly id: CanonicalId; readonly token: Token.Token }>()
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const implNodes = declarations.filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'ImplDeclaration',
  )
  const isConformanceImpl = (node: SyntaxTree.Node): boolean =>
    SyntaxTree.directToken(node, 'ForKeyword') !== undefined
  const conformances = implNodes.filter(isConformanceImpl).map((node, ordinal): ConformanceFact => {
    const collected = collectTypeParameters(source, node, `impl#${ordinal}`)
    diagnostics.push(...collected.diagnostics)
    const selfType = Type.parameter({ module: source.id, name: `impl#${ordinal}` }, -1, 'Self')
    const environment = new Map(collected.environment)
    environment.set('Self', selfType)
    const types = node.children.filter(isDeclaredTypeNode)
    const capabilitySyntax = types.at(0)
    const providerSyntax = types.at(1)
    const capability =
      capabilitySyntax === undefined
        ? Object.freeze({ _tag: 'Unavailable' as const, syntax: node })
        : analyzeDeclaredType(source, capabilitySyntax, environment).fact
    const provider =
      providerSyntax === undefined
        ? Object.freeze({ _tag: 'Unavailable' as const, syntax: node })
        : analyzeDeclaredType(source, providerSyntax, environment).fact
    // A binder's bound is re-analyzed here rather than reused from the parameter collection,
    // because a conditional requirement may name any binder the header declares — including the
    // one it bounds — and only the completed environment can resolve those occurrences.
    const requirements = collected.facts.flatMap(
      (parameter): ReadonlyArray<ConformanceRequirementFact> => {
        if (parameter.duplicateOf !== undefined) return []
        const bounds: ReadonlyArray<ConformanceRequirementFact> = parameter.bounds.map((bound) =>
          Object.freeze({
            _tag: 'ConformanceRequirement' as const,
            parameter: parameter.type,
            spelling: bound.spelling,
            capability: analyzeDeclaredType(source, bound.path.syntax, environment).fact,
            syntax: bound.path.syntax,
          }),
        )
        if (SyntaxTree.isAvailableSyntax(parameter.syntax)) return bounds
        // Recovery can retain a binder while its missing bound has no identifier to collect.
        // Keep that damaged header as an unavailable obligation, never as an unbounded witness.
        return [
          ...bounds,
          Object.freeze({
            _tag: 'ConformanceRequirement' as const,
            parameter: parameter.type,
            spelling: Type.encode(parameter.type),
            capability: Object.freeze({ _tag: 'Unavailable' as const, syntax: parameter.syntax }),
            syntax: parameter.syntax,
          }),
        ]
      },
    )
    const mappedOperations = SyntaxTree.directNodes(node, 'ImplOperation').map((operation) => {
      const name = presentName(source, operation)
      const targetSyntax = SyntaxTree.directNode(operation, 'TypePath')
      const target =
        targetSyntax === undefined
          ? Object.freeze({ _tag: 'Unavailable' as const, syntax: operation })
          : (() => {
              const tokens = SyntaxTree.tokens(targetSyntax).filter(
                (token) => token.kind === 'Identifier',
              )
              return tokens.length === 0
                ? Object.freeze({ _tag: 'Unavailable' as const, syntax: targetSyntax })
                : Object.freeze({
                    _tag: 'TypePath' as const,
                    spelling: tokens.map((token) => spelling(source, token)).join('.'),
                    segments: Object.freeze(
                      tokens.map((token) =>
                        Object.freeze({ spelling: spelling(source, token), token }),
                      ),
                    ),
                    syntax: targetSyntax,
                  })
            })()
      return Object.freeze({ name, target, form: 'Mapped' as const, syntax: operation })
    })
    const inlineOperations = SyntaxTree.directNodes(node, 'FunctionDeclaration').flatMap(
      (operation): ReadonlyArray<ConformanceFact['operations'][number]> => {
        if (SyntaxTree.directToken(operation, 'DropKeyword') !== undefined) return []
        const name = presentName(source, operation)
        const providerToken = providerSyntax
          ? SyntaxTree.tokens(providerSyntax).find((token) => token.kind === 'Identifier')
          : undefined
        if (name._tag !== 'Present' || providerToken === undefined)
          return Object.freeze([
            Object.freeze({
              name,
              target: Object.freeze({ _tag: 'Unavailable' as const, syntax: operation }),
              form: 'Inline' as const,
              syntax: operation,
            }),
          ])
        const targetName = `impl@${ordinal}.${name.spelling}`
        return Object.freeze([
          Object.freeze({
            name,
            target: Object.freeze({
              _tag: 'TypePath' as const,
              spelling: `${spelling(source, providerToken)}.${targetName}`,
              segments: Object.freeze([
                Object.freeze({
                  spelling: spelling(source, providerToken),
                  token: providerToken,
                }),
                Object.freeze({ spelling: targetName, token: name.token }),
              ]),
              syntax: operation,
            }),
            form: 'Inline' as const,
            syntax: operation,
          }),
        ])
      },
    )
    const hookSyntax = SyntaxTree.directNodes(node, 'FunctionDeclaration').find(
      (operation) => SyntaxTree.directToken(operation, 'DropKeyword') !== undefined,
    )
    const hook =
      hookSyntax === undefined
        ? undefined
        : (() => {
            const hookLifetimes = DeclarationLifetime.forHeader(
              source,
              { module: source.id, name: `drop@impl#${ordinal}` },
              hookSyntax,
              environment,
            )
            const parameterList = SyntaxTree.directNode(hookSyntax, 'ParameterList')
            const parameters =
              parameterList === undefined
                ? []
                : SyntaxTree.directNodes(parameterList, 'ParameterDeclaration')
            const parameter = parameters.at(0)
            const parameterTypeSyntax =
              parameter === undefined
                ? undefined
                : parameter.children.find((element): element is SyntaxTree.Node =>
                    isDeclaredTypeNode(element),
                  )
            const returnSyntax = SyntaxTree.directNode(hookSyntax, 'ReturnType')
            const returnTypeSyntax = returnSyntax?.children.find(
              (element): element is SyntaxTree.Node => isDeclaredTypeNode(element),
            )
            const failure = collectFailureRow(source, hookSyntax, environment)
            const requirements = collectRequirementRow(source, hookSyntax, environment)
            const hookNameToken = SyntaxTree.directToken(hookSyntax, 'DropKeyword')
            diagnostics.push(...failure.diagnostics, ...requirements.diagnostics)
            return Object.freeze({
              _tag: 'DropHookDeclaration' as const,
              name:
                hookNameToken === undefined
                  ? presentName(source, hookSyntax)
                  : Object.freeze({
                      _tag: 'Present' as const,
                      spelling: 'drop',
                      token: hookNameToken,
                    }),
              functionKind:
                SyntaxTree.directToken(hookSyntax, 'EffectKeyword') === undefined
                  ? ('Ordinary' as const)
                  : ('Effect' as const),
              typeParameterCount:
                SyntaxTree.directNode(hookSyntax, 'TypeParameterList') === undefined
                  ? 0
                  : SyntaxTree.directNodes(
                      childNode(hookSyntax, 'TypeParameterList'),
                      'TypeParameter',
                    ).length,
              parameterCount: parameters.length,
              parameterName:
                parameter === undefined
                  ? Object.freeze({ _tag: 'Unavailable' as const, syntax: hookSyntax })
                  : presentName(source, parameter),
              parameterType:
                parameterTypeSyntax === undefined
                  ? Object.freeze({
                      _tag: 'Unavailable' as const,
                      syntax: parameter ?? hookSyntax,
                    })
                  : analyzeDeclaredType(
                      source,
                      parameterTypeSyntax,
                      environment,
                      false,
                      hookLifetimes,
                    ).fact,
              returnType:
                returnTypeSyntax === undefined
                  ? Object.freeze({
                      _tag: 'Unavailable' as const,
                      syntax: returnSyntax ?? hookSyntax,
                    })
                  : analyzeDeclaredType(source, returnTypeSyntax, environment, false, hookLifetimes)
                      .fact,
              failureRow: failure.fact,
              requirementRow: requirements.fact,
              syntax: hookSyntax,
            })
          })()
    return Object.freeze({
      _tag: 'ConformanceDeclaration',
      lifetimeElaboration: collected.lifetimeContext,
      module: source.id,
      ordinal,
      self: selfType,
      typeParameters: collected.facts,
      requirements: Object.freeze(requirements),
      capability,
      provider,
      visibility: 'Public',
      operations: Object.freeze([...mappedOperations, ...inlineOperations]),
      ...(hook === undefined ? {} : { hook }),
      // Coherence and termination are program-wide questions, so both stay unanswered until
      // every module's headers have resolved.
      coherence: Object.freeze({ _tag: 'Coherent' as const }),
      termination: Object.freeze({ _tag: 'UnavailableTermination' as const }),
      validity: Object.freeze({ _tag: 'UncheckedConformance' as const }),
      syntax: node,
    })
  })
  let nestedDeclarationOrdinal = nodes.length
  const ownMembers = nodes.map((node, ordinal): MemberFact => {
    const id: DeclarationId = Object.freeze({
      _tag: 'DeclarationId',
      sourceId: source.id,
      ordinal,
    })
    const name = presentName(source, node)
    let canonical: CanonicalState
    if (name._tag !== 'Present') canonical = Object.freeze({ _tag: 'Unidentified' })
    else {
      const original = first.get(name.spelling)
      if (original === undefined) {
        const canonicalId: CanonicalId = Object.freeze({
          _tag: 'CanonicalDeclarationId',
          module: source.id,
          name: name.spelling,
        })
        first.set(name.spelling, Object.freeze({ id: canonicalId, token: name.token }))
        canonical = Object.freeze({ _tag: 'Canonical', id: canonicalId })
      } else {
        const diagnostic = Diagnostic.duplicateDeclarationName(
          name.spelling,
          original.token.span,
          name.token.span,
        )
        diagnostics.push(diagnostic)
        canonical = Object.freeze({
          _tag: 'Duplicate',
          original: original.id,
          cause: Diagnostic.identity(diagnostic),
        })
      }
    }
    const visibility: 'Private' | 'Public' =
      SyntaxTree.directToken(node, 'PubKeyword') === undefined ? 'Private' : 'Public'
    const typeParameters = collectTypeParameters(
      source,
      node,
      name._tag === 'Present' ? name.spelling : `#${ordinal}`,
    )
    diagnostics.push(...typeParameters.diagnostics)
    if (node.kind === 'TypeAliasDeclaration') {
      const parameterList = SyntaxTree.directNode(node, 'TypeParameterList')
      const targetSyntax = node.children.find(isDeclaredTypeNode)
      // A parameterized alias is rejected as a whole, so its target is never analyzed: analyzing
      // it would only report the parameters it names as unknown types on top of the rejection.
      const target =
        parameterList !== undefined || targetSyntax === undefined
          ? Object.freeze({
              fact: Object.freeze({ _tag: 'Unavailable' as const, syntax: node }),
              diagnostics: Object.freeze([]),
            })
          : analyzeDeclaredType(source, targetSyntax)
      diagnostics.push(...target.diagnostics)
      return Object.freeze({
        _tag: 'AliasDeclaration',
        id,
        canonical,
        visibility,
        typeParameters: Object.freeze([]),
        name,
        target: target.fact,
        ...(parameterList === undefined ? {} : { parameterList }),
        syntax: node,
      })
    }
    if (node.kind === 'ConstantDeclaration' || node.kind === 'PackageParameterDeclaration') {
      const initializer =
        node.children.find(
          (element): element is SyntaxTree.Node =>
            SyntaxTree.isNode(element) &&
            element.kind !== 'PackageParameterValidation' &&
            !isDeclaredTypeNode(element),
        ) ?? node
      const declaredType = analyzeDeclaredType(source, declaredTypeNode(node))
      diagnostics.push(...declaredType.diagnostics)
      const validation = SyntaxTree.directNode(node, 'PackageParameterValidation')
      const predicate = validation?.children.find(SyntaxTree.isNode)
      const base = {
        id,
        canonical,
        visibility,
        typeParameters: Object.freeze([]),
        name,
        declaredType: declaredType.fact,
        initializerTemplate: staticExpressionTemplate(source, initializer),
        literal:
          node.kind === 'PackageParameterDeclaration'
            ? Object.freeze({ _tag: 'Unavailable' as const, syntax: initializer })
            : constantLiteral(source, initializer),
        initializer,
        syntax: node,
      }
      return node.kind === 'PackageParameterDeclaration'
        ? Object.freeze({
            ...base,
            _tag: 'PackageParameterDeclaration',
            hasDefault: SyntaxTree.directToken(node, 'Equals') !== undefined,
            ...(predicate === undefined
              ? {}
              : { predicate, predicateTemplate: staticExpressionTemplate(source, predicate) }),
          })
        : Object.freeze({ ...base, _tag: 'ConstantDeclaration' })
    }
    if (node.kind === 'ForeignStaticDeclaration' || node.kind === 'ExportStaticDeclaration') {
      const declaredType = analyzeDeclaredType(source, declaredTypeNode(node))
      const foreign = collectForeignStatic(source, node, name)
      diagnostics.push(...declaredType.diagnostics, ...foreign.diagnostics)
      const initializer = node.children.find(
        (element): element is SyntaxTree.Node =>
          SyntaxTree.isNode(element) && !isDeclaredTypeNode(element),
      )
      return Object.freeze({
        _tag: 'ForeignStaticDeclaration',
        id,
        canonical,
        visibility: 'Private',
        typeParameters: Object.freeze([]),
        name,
        direction: node.kind === 'ForeignStaticDeclaration' ? 'Import' : 'Export',
        foreign: foreign.fact,
        declaredType: declaredType.fact,
        ...(initializer === undefined
          ? {}
          : {
              initializerTemplate: staticExpressionTemplate(source, initializer),
              literal: constantLiteral(source, initializer),
              initializer,
            }),
        syntax: node,
      })
    }
    if (node.kind === 'RoleDeclaration') {
      return Object.freeze({
        _tag: 'RoleDeclaration',
        id,
        canonical,
        visibility,
        typeParameters: Object.freeze([]),
        name,
        syntax: node,
      })
    }
    if (node.kind === 'EnumDeclaration')
      return collectEnum(source, node, id, canonical, visibility, name, diagnostics)
    if (node.kind === 'UnionDeclaration')
      return collectUnion(
        source,
        node,
        id,
        canonical,
        visibility,
        name,
        typeParameters,
        diagnostics,
      )
    if (node.kind === 'StructDeclaration') {
      const layout = collectStructLayout(source, node)
      const collected = collectFields(
        source,
        node,
        Object.freeze({ _tag: 'StructFieldOwnerId', declaration: id }),
        'StructField',
        typeParameters.environment,
        typeParameters.lifetimeContext,
      )
      diagnostics.push(...layout.diagnostics, ...collected.diagnostics)
      return Object.freeze({
        _tag: 'StructDeclaration',
        lifetimeElaboration: typeParameters.lifetimeContext,
        id,
        canonical,
        visibility,
        layout: layout.fact,
        typeParameters: typeParameters.facts,
        name,
        ...(name._tag === 'Present'
          ? { identity: AggregateIdentity.source(source.id, name.spelling, 'Named') }
          : {}),
        aggregateKind: 'Named',
        fields: collected.fields,
        dependency: Object.freeze({ _tag: 'Available', types: Object.freeze([]) }),
        syntax: node,
      })
    }
    if (node.kind === 'TupleDeclaration') {
      const collected = collectPositionalFields(
        source,
        node,
        Object.freeze({ _tag: 'StructFieldOwnerId', declaration: id }),
        typeParameters.environment,
        typeParameters.lifetimeContext,
      )
      diagnostics.push(...collected.diagnostics)
      return Object.freeze({
        _tag: 'StructDeclaration',
        lifetimeElaboration: typeParameters.lifetimeContext,
        id,
        canonical,
        visibility,
        layout: Object.freeze({ _tag: 'Silk' }),
        typeParameters: typeParameters.facts,
        name,
        ...(name._tag === 'Present'
          ? { identity: AggregateIdentity.source(source.id, name.spelling, 'Positional') }
          : {}),
        aggregateKind: 'Positional',
        fields: collected.fields,
        dependency: Object.freeze({ _tag: 'Available', types: Object.freeze([]) }),
        syntax: node,
      })
    }
    if (node.kind === 'ServiceDeclaration' || node.kind === 'InterfaceDeclaration') {
      const selfType = Type.parameter(
        {
          module: source.id,
          name: name._tag === 'Present' ? name.spelling : `#${ordinal}`,
        },
        -1,
        'Self',
      )
      const contractEnvironment = new Map(typeParameters.environment)
      contractEnvironment.set('Self', selfType)
      const operationFirst = new Map<
        string,
        { readonly id: ServiceOperationId; readonly token: Token.Token }
      >()
      const operations = SyntaxTree.directNodes(node, 'ServiceOperation').map(
        (operation, operationOrdinal): ServiceOperationFact => {
          const operationId: DeclarationId = Object.freeze({
            _tag: 'DeclarationId',
            sourceId: source.id,
            ordinal: nestedDeclarationOrdinal,
          })
          nestedDeclarationOrdinal += 1
          const operationName = presentName(source, operation)
          let operationState: ServiceOperationState
          if (operationName._tag !== 'Present') {
            operationState = Object.freeze({ _tag: 'Unidentified' })
          } else {
            const original = operationFirst.get(operationName.spelling)
            if (original === undefined) {
              const serviceOperationId: ServiceOperationId = Object.freeze({
                _tag: 'ServiceOperationId',
                service: id,
                name: operationName.spelling,
              })
              operationFirst.set(
                operationName.spelling,
                Object.freeze({ id: serviceOperationId, token: operationName.token }),
              )
              operationState = Object.freeze({ _tag: 'Unique', id: serviceOperationId })
            } else {
              const diagnostic = Diagnostic.duplicateDeclarationName(
                operationName.spelling,
                original.token.span,
                operationName.token.span,
              )
              diagnostics.push(diagnostic)
              operationState = Object.freeze({
                _tag: 'Duplicate',
                original: original.id,
                cause: Diagnostic.identity(diagnostic),
              })
            }
          }
          const operationTypeParameters = collectTypeParameters(
            source,
            operation,
            `${name._tag === 'Present' ? name.spelling : `#${ordinal}`}.$${operationOrdinal}`,
            typeParameters.facts.length,
            typeParameters.facts,
          )
          diagnostics.push(...operationTypeParameters.diagnostics)
          const environment = new Map<string, Type.Parameter>([
            ...contractEnvironment,
            ...operationTypeParameters.environment,
          ])
          const parameterList = childNode(operation, 'ParameterList')
          const parameters = SyntaxTree.directNodes(parameterList, 'ParameterDeclaration').map(
            (parameter, parameterOrdinal) =>
              analyzeParameter(
                source,
                parameter,
                operationId,
                parameterOrdinal,
                environment,
                operationTypeParameters.lifetimeContext,
              ),
          )
          const returnSyntax = SyntaxTree.directNode(operation, 'ReturnType')
          const returnType: {
            readonly fact: ReturnTypeFact
            readonly opaqueResult?: OpaqueResultFact
            readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
          } =
            returnSyntax === undefined
              ? (() => {
                  const token = SyntaxTree.directToken(parameterList, 'RightParenthesis')
                  if (token === undefined)
                    return Object.freeze({
                      fact: Object.freeze({
                        _tag: 'Unavailable' as const,
                        syntax: parameterList,
                      }),
                      diagnostics: Object.freeze<ReadonlyArray<Diagnostic.Diagnostic>>([]),
                    })
                  return Object.freeze({
                    fact: Object.freeze({
                      _tag: 'Resolved' as const,
                      type: Type.unit,
                      spelling: '()',
                      token,
                      syntax: parameterList,
                    }),
                    diagnostics: Object.freeze<ReadonlyArray<Diagnostic.Diagnostic>>([]),
                  })
                })()
              : collectReturnType(
                  source,
                  returnSyntax,
                  `${name._tag === 'Present' ? name.spelling : `#${ordinal}`}.$${operationOrdinal}`,
                  [...typeParameters.facts, ...operationTypeParameters.facts],
                  contractEnvironment,
                  operationTypeParameters.lifetimeContext,
                )
          const failureRow = collectFailureRow(
            source,
            operation,
            environment,
            operationTypeParameters.lifetimeContext,
          )
          const requirementRow = collectRequirementRow(
            source,
            operation,
            environment,
            operationTypeParameters.lifetimeContext,
          )
          const constraints = collectConstraints(source, operation, environment)
          const body = SyntaxTree.directNode(operation, 'Block')
          const parameterFacts = Object.freeze(parameters.map((parameter) => parameter.fact))
          const operatorSyntax = SyntaxTree.directNode(operation, 'OperatorMarker')
          const operatorToken = operatorSyntax?.children.find(
            (element): element is Token.Token =>
              SyntaxTree.isToken(element) && Operator.isDeclarationToken(element.kind),
          )
          const selectedOperator =
            operatorToken === undefined
              ? undefined
              : Operator.declaration(operatorToken.kind, parameterFacts.length)
          diagnostics.push(
            ...parameters.flatMap((parameter) => parameter.diagnostics),
            ...duplicateParameterDiagnostics(parameterFacts),
            ...returnType.diagnostics,
            ...failureRow.diagnostics,
            ...requirementRow.diagnostics,
            ...constraints.diagnostics,
          )
          if (operatorSyntax !== undefined) {
            let detail: string | undefined
            if (node.kind !== 'InterfaceDeclaration') {
              detail = 'only interface operations may declare an operator'
            } else if (
              operationTypeParameters.facts.some((parameter) => parameter.type.kind !== 'Lifetime')
            ) {
              detail = 'operator operations cannot declare operation-local value or row parameters'
            } else if (selectedOperator === undefined) {
              detail = `${operatorToken === undefined ? 'the marker' : Token.describe(operatorToken.kind)} is not an eligible ${parameterFacts.length}-operand operator`
            } else {
              detail = undefined
            }
            if (detail !== undefined)
              diagnostics.push(Diagnostic.invalidOperatorContract(detail, operatorSyntax.span))
          }
          if (body !== undefined)
            diagnostics.push(
              Diagnostic.invalidServiceDeclaration(
                'service operations declare contracts and cannot contain bodies',
                body.span,
              ),
            )
          if (
            SyntaxTree.directToken(operation, 'EffectKeyword') === undefined &&
            failureRow.fact.syntax !== undefined
          )
            diagnostics.push(Diagnostic.failureChannelOnOrdinary(failureRow.fact.syntax.span))
          return Object.freeze({
            _tag: 'ServiceOperation',
            id: operationId,
            state: operationState,
            functionKind:
              SyntaxTree.directToken(operation, 'EffectKeyword') === undefined
                ? 'Ordinary'
                : 'Effect',
            unsafe: SyntaxTree.directToken(operation, 'UnsafeKeyword') !== undefined,
            typeParameters: operationTypeParameters.facts,
            lifetimeElaboration: Object.freeze({
              ...operationTypeParameters.lifetimeContext,
              parameters: new Map([
                ...operationTypeParameters.lifetimeContext.parameters,
                ...contractEnvironment,
              ]),
            }),
            parameterCount: parameterFacts.length,
            parameters: parameterFacts,
            ...(operatorSyntax !== undefined &&
            operatorToken !== undefined &&
            selectedOperator !== undefined &&
            node.kind === 'InterfaceDeclaration' &&
            operationTypeParameters.facts.every((parameter) => parameter.type.kind === 'Lifetime')
              ? {
                  operator: Object.freeze({
                    operator: selectedOperator,
                    token: operatorToken,
                    syntax: operatorSyntax,
                  }),
                }
              : {}),
            name: operationName,
            returnType: returnType.fact,
            ...(returnType.opaqueResult === undefined
              ? {}
              : { opaqueResult: returnType.opaqueResult }),
            failureRow: failureRow.fact,
            requirementRow: requirementRow.fact,
            constraints: constraints.facts,
            constraintContracts: Object.freeze([]),
            syntax: operation,
          })
        },
      )
      const shared = {
        id,
        canonical,
        visibility,
        self: selfType,
        typeParameters: typeParameters.facts,
        name,
        operations: Object.freeze(operations),
        syntax: node,
      }
      const contract =
        node.kind === 'InterfaceDeclaration'
          ? Object.freeze({
              _tag: 'InterfaceDeclaration' as const,
              dependencyEligible: false as const,
              ...shared,
            })
          : Object.freeze({
              _tag: 'ServiceDeclaration' as const,
              dependencyEligible: true as const,
              ...shared,
            })
      return Object.freeze({
        ...contract,
        operationContracts: interfaceOperationContracts(contract, operations),
      })
    }
    const parameterList = childNode(node, 'ParameterList')
    const parameters = SyntaxTree.directNodes(parameterList, 'ParameterDeclaration').map(
      (parameter, parameterOrdinal) =>
        analyzeParameter(
          source,
          parameter,
          id,
          parameterOrdinal,
          typeParameters.environment,
          typeParameters.lifetimeContext,
        ),
    )
    const returnSyntax = SyntaxTree.directNode(node, 'ReturnType')
    const returnType: {
      readonly fact: ReturnTypeFact
      readonly opaqueResult?: OpaqueResultFact
      readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
    } =
      returnSyntax === undefined
        ? (() => {
            const parameterList = childNode(node, 'ParameterList')
            const token = SyntaxTree.directToken(parameterList, 'RightParenthesis')
            if (token === undefined)
              return Object.freeze({
                fact: Object.freeze({
                  _tag: 'Unavailable' as const,
                  syntax: parameterList,
                }),
                diagnostics: Object.freeze<ReadonlyArray<Diagnostic.Diagnostic>>([]),
              })
            return Object.freeze({
              fact: Object.freeze({
                _tag: 'Resolved' as const,
                type: Type.unit,
                spelling: '()',
                token,
                syntax: parameterList,
              }),
              diagnostics: Object.freeze<ReadonlyArray<Diagnostic.Diagnostic>>([]),
            })
          })()
        : collectReturnType(
            source,
            returnSyntax,
            name._tag === 'Present' ? name.spelling : `#${ordinal}`,
            typeParameters.facts,
            new Map(),
            typeParameters.lifetimeContext,
          )
    const functionKind =
      SyntaxTree.directToken(node, 'EffectKeyword') === undefined ? 'Ordinary' : 'Effect'
    const failureRow = collectFailureRow(
      source,
      node,
      typeParameters.environment,
      typeParameters.lifetimeContext,
    )
    const requirementRow = collectRequirementRow(
      source,
      node,
      typeParameters.environment,
      typeParameters.lifetimeContext,
    )
    const constraints = collectConstraints(source, node, typeParameters.environment)
    const staticFunction = SyntaxTree.directToken(node, 'StaticKeyword') !== undefined
    const facts = Object.freeze(
      parameters.map((parameter) =>
        staticFunction && parameter.fact.phase !== 'Static'
          ? Object.freeze({ ...parameter.fact, phase: 'Static' as const })
          : parameter.fact,
      ),
    )
    diagnostics.push(
      ...parameters.flatMap((parameter) => parameter.diagnostics),
      ...duplicateParameterDiagnostics(facts),
      ...returnType.diagnostics,
      ...failureRow.diagnostics,
      ...requirementRow.diagnostics,
      ...constraints.diagnostics,
    )
    const foreign =
      node.kind === 'ForeignFunctionDeclaration'
        ? collectForeign(source, node, name, 'Foreign', facts)
        : undefined
    const foreignExport =
      node.kind === 'FunctionDeclaration' &&
      SyntaxTree.directToken(node, 'ExportKeyword') !== undefined
        ? collectForeign(source, node, name, 'Export', facts)
        : undefined
    const machine = MachineFunction.analyze(source, node)
    diagnostics.push(...machine.diagnostics)
    if (foreign !== undefined && machine.properties !== undefined)
      diagnostics.push(
        Diagnostic.foreignDeclarationRestriction(
          'machine property on a foreign import',
          machine.properties.span,
        ),
      )
    const properties = DeclarationProperty.clauses(node)
    const behavior = properties.filter(
      (clause) =>
        DeclarationProperty.owner(source, clause) !== 'Intrinsic.native' &&
        DeclarationProperty.owner(source, clause) !== 'Intrinsic.machine',
    )
    for (const property of behavior.slice(1))
      diagnostics.push(
        Diagnostic.foreignDeclarationRestriction('duplicate foreign contract', property.span),
      )
    for (const property of properties)
      if (
        foreign === undefined &&
        foreignExport === undefined &&
        DeclarationProperty.owner(source, property) !== 'Intrinsic.machine'
      )
        diagnostics.push(
          Diagnostic.foreignDeclarationRestriction(
            'foreign contract on a non-foreign function',
            property.span,
          ),
        )
    const native = foreign ?? foreignExport
    if (native === undefined && functionKind === 'Ordinary' && failureRow.fact.syntax !== undefined)
      diagnostics.push(Diagnostic.failureChannelOnOrdinary(failureRow.fact.syntax.span))
    if (native !== undefined) diagnostics.push(...native.diagnostics)
    const retainedBody = foreign === undefined ? bodyTemplate(source, node) : undefined
    return Object.freeze({
      _tag: 'FunctionDeclaration',
      lifetimeElaboration: typeParameters.lifetimeContext,
      id,
      canonical,
      visibility,
      phase: foreign === undefined && staticFunction ? 'Static' : 'Runtime',
      functionKind: foreign === undefined ? functionKind : 'Ordinary',
      unsafe: SyntaxTree.directToken(node, 'UnsafeKeyword') !== undefined,
      ...(machine.properties === undefined ? {} : { machine: machine.properties }),
      ...(foreign === undefined ? {} : { foreign: foreign.fact }),
      // A rejected export publishes no symbol, so discovery never roots it.
      ...(foreignExport === undefined || foreignExport.diagnostics.length > 0
        ? {}
        : { foreignExport: foreignExport.fact }),
      typeParameters: typeParameters.facts,
      parameterCount: facts.length,
      parameters: facts,
      name,
      // A rejected foreign or exported header withholds its result so no callable contract is
      // published and call sites get the ordinary unavailable-contract behavior instead of
      // repeated errors.
      returnType:
        native !== undefined && native.diagnostics.length > 0
          ? Object.freeze({ _tag: 'Unavailable' as const, syntax: returnType.fact.syntax })
          : returnType.fact,
      ...(returnType.opaqueResult === undefined ? {} : { opaqueResult: returnType.opaqueResult }),
      failureRow: failureRow.fact,
      requirementRow: requirementRow.fact,
      constraints: constraints.facts,
      constraintContracts: Object.freeze([]),
      ...(retainedBody === undefined ? {} : { bodyTemplate: retainedBody }),
      syntax: node,
    })
  })
  // Owner binders precede the member's own, so `Option.map<i32, i64>` reads T then U; a binder
  // the member refined carries the member's bounds under the head's identity.
  // An owner binder joins the member's generic sequence only when the member mentions it (or
  // names `Self`, which stands for the whole applied owner); a member that never mentions one
  // (`Fiber.cancel(canceller: CompletionCanceller)`) would otherwise carry a binder no call
  // could ever infer.
  const joinedTypeParameters = (
    member: SyntaxTree.Node,
    headBinders: ReadonlyArray<TypeParameterFact>,
    built: Pick<DeclarationFact, 'typeParameters'>,
    refinedBinders: ReadonlyMap<string, TypeParameterFact>,
  ): ReadonlyArray<TypeParameterFact> => {
    const mentioned = new Set(
      SyntaxTree.tokens(member)
        .filter((token) => token.kind === 'Identifier' || token.kind === 'Lifetime')
        .map((token) => spelling(source, token)),
    )
    const namesSelf = mentioned.has('Self')
    return Object.freeze([
      ...headBinders
        .filter(
          (parameter) =>
            parameter.duplicateOf === undefined &&
            (namesSelf ||
              (parameter.name._tag === 'Present' && mentioned.has(parameter.name.spelling))),
        )
        .map((parameter) => {
          const refined =
            parameter.name._tag === 'Present'
              ? refinedBinders.get(parameter.name.spelling)
              : undefined
          return refined === undefined
            ? parameter
            : Object.freeze({
                ...parameter,
                bounds: Object.freeze([...parameter.bounds, ...refined.bounds]),
              })
        }),
      ...built.typeParameters,
    ])
  }
  // A function declared inside an impl block, conformance or inherent, elaborates and lowers as an
  // ordinary declaration carrying the impl's binders ahead of its own and `Self` bound to the
  // head's binder. The caller supplies identity, visibility, and the membership back-reference.
  const implMember = (
    node: SyntaxTree.Node,
    id: DeclarationId,
    ownerName: string,
    headBinders: ReadonlyArray<TypeParameterFact>,
    self: Type.Parameter,
  ): Omit<DeclarationFact, 'canonical' | 'visibility' | 'name' | 'id'> & {
    readonly refinedBinders: ReadonlyMap<string, TypeParameterFact>
  } => {
    const targetName = ownerName
    // `Self` is in scope for the member's own binder bounds (`U: Like<Self>`) exactly as the
    // head binders are, so it rides along as a synthetic enclosing binder.
    const selfToken = SyntaxTree.tokens(node).find((token) => token.kind === 'FnKeyword')
    const selfBinder: ReadonlyArray<TypeParameterFact> =
      selfToken === undefined
        ? []
        : [
            Object.freeze({
              _tag: 'TypeParameterDeclaration' as const,
              type: self,
              name: Object.freeze({ _tag: 'Present' as const, spelling: 'Self', token: selfToken }),
              syntax: node,
              bounds: Object.freeze([]),
              staticProperties: Object.freeze([]),
            }),
          ]
    const collected = collectTypeParameters(source, node, targetName, headBinders.length, [
      ...headBinders,
      ...selfBinder,
    ])
    // A member may redeclare an owner binder to refine its bounds for that member alone
    // (`fn get<K: HashKey + Copy, V: Copy>` inside `impl<K, V> HashMap<K, V>`): the binder keeps
    // the head's identity and gains the member's bounds, and no duplicate is reported.
    const refinedNames = new Set(
      collected.facts.flatMap((fact) =>
        fact.duplicateOf !== undefined &&
        fact.name._tag === 'Present' &&
        headBinders.some((binder) => binder.type === fact.duplicateOf)
          ? [fact.name.spelling]
          : [],
      ),
    )
    diagnostics.push(
      ...collected.diagnostics.filter(
        (diagnostic) =>
          !(
            diagnostic.reason._tag === 'DuplicateTypeParameter' &&
            refinedNames.has(diagnostic.reason.spelling)
          ),
      ),
    )
    const refinedBinders = new Map(
      collected.facts.flatMap((fact) =>
        fact.duplicateOf !== undefined &&
        fact.name._tag === 'Present' &&
        refinedNames.has(fact.name.spelling)
          ? [[fact.name.spelling, fact] as const]
          : [],
      ),
    )
    const ownFacts = collected.facts.filter(
      (fact) => !(fact.name._tag === 'Present' && refinedNames.has(fact.name.spelling)),
    )
    const environment = new Map(collected.environment)
    environment.set('Self', self)
    const parameterList = childNode(node, 'ParameterList')
    const parameters = SyntaxTree.directNodes(parameterList, 'ParameterDeclaration').map(
      (parameter, ordinal) =>
        analyzeParameter(source, parameter, id, ordinal, environment, collected.lifetimeContext),
    )
    const returnSyntax = SyntaxTree.directNode(node, 'ReturnType')
    const returnType =
      returnSyntax === undefined
        ? (() => {
            const token = SyntaxTree.directToken(parameterList, 'RightParenthesis')
            return token === undefined
              ? Object.freeze({
                  fact: Object.freeze({
                    _tag: 'Unavailable' as const,
                    syntax: parameterList,
                  }),
                  diagnostics: Object.freeze<ReadonlyArray<Diagnostic.Diagnostic>>([]),
                })
              : Object.freeze({
                  fact: Object.freeze({
                    _tag: 'Resolved' as const,
                    type: Type.unit,
                    spelling: '()',
                    token,
                    syntax: parameterList,
                  }),
                  diagnostics: Object.freeze<ReadonlyArray<Diagnostic.Diagnostic>>([]),
                })
          })()
        : collectReturnType(
            source,
            returnSyntax,
            targetName,
            collected.facts,
            environment,
            collected.lifetimeContext,
          )
    const failureRow = collectFailureRow(source, node, environment, collected.lifetimeContext)
    const requirementRow = collectRequirementRow(
      source,
      node,
      environment,
      collected.lifetimeContext,
    )
    const constraints = collectConstraints(source, node, environment)
    const staticFunction = SyntaxTree.directToken(node, 'StaticKeyword') !== undefined
    const parameterFacts = Object.freeze(
      parameters.map((parameter) =>
        staticFunction && parameter.fact.phase !== 'Static'
          ? Object.freeze({ ...parameter.fact, phase: 'Static' as const })
          : parameter.fact,
      ),
    )
    diagnostics.push(
      ...parameters.flatMap((parameter) => parameter.diagnostics),
      ...duplicateParameterDiagnostics(parameterFacts),
      ...returnType.diagnostics,
      ...failureRow.diagnostics,
      ...requirementRow.diagnostics,
      ...constraints.diagnostics,
    )
    const retainedBody = bodyTemplate(source, node)
    return Object.freeze({
      _tag: 'FunctionDeclaration' as const,
      phase: staticFunction ? ('Static' as const) : ('Runtime' as const),
      functionKind:
        SyntaxTree.directToken(node, 'EffectKeyword') === undefined
          ? ('Ordinary' as const)
          : ('Effect' as const),
      unsafe: SyntaxTree.directToken(node, 'UnsafeKeyword') !== undefined,
      typeParameters: ownFacts,
      lifetimeElaboration: collected.lifetimeContext,
      refinedBinders,
      parameterCount: parameterFacts.length,
      parameters: parameterFacts,
      returnType: returnType.fact,
      ...('opaqueResult' in returnType && returnType.opaqueResult !== undefined
        ? { opaqueResult: returnType.opaqueResult }
        : {}),
      failureRow: failureRow.fact,
      requirementRow: requirementRow.fact,
      constraints: constraints.facts,
      constraintContracts: Object.freeze([]),
      ...(retainedBody === undefined ? {} : { bodyTemplate: retainedBody }),
      syntax: node,
    })
  }
  // Inline conformance operations elaborate and lower as private ordinary declarations. Their
  // canonical names are implementation identities, not source-visible actor members.
  const inlineMembers = conformances.flatMap(
    (conformance, conformanceIndex): ReadonlyArray<MemberFact> =>
      conformance.operations.flatMap((operation, operationIndex): ReadonlyArray<MemberFact> => {
        if (operation.form !== 'Inline' || operation.target._tag !== 'TypePath') return []
        const targetName = operation.target.segments.at(1)?.spelling
        const targetToken = operation.target.segments.at(1)?.token
        if (targetName === undefined || targetToken === undefined) return []
        const node = operation.syntax
        const id: DeclarationId = Object.freeze({
          _tag: 'DeclarationId',
          sourceId: source.id,
          ordinal: nestedDeclarationOrdinal + conformanceIndex * 1024 + operationIndex,
        })
        const {
          bodyTemplate: _retained,
          refinedBinders,
          ...built
        } = implMember(node, id, targetName, conformance.typeParameters, conformance.self)
        return [
          Object.freeze({
            ...built,
            // A generic conformance's inline body is generic in the header's binders exactly as a
            // mapped witness is; the proof that selects the witness supplies their arguments.
            typeParameters: joinedTypeParameters(
              node,
              conformance.typeParameters,
              built,
              refinedBinders,
            ),
            id,
            canonical: Object.freeze({
              _tag: 'Canonical' as const,
              id: Object.freeze({
                _tag: 'CanonicalDeclarationId' as const,
                module: source.id,
                name: targetName,
              }),
            }),
            visibility: 'Private' as const,
            name: Object.freeze({
              _tag: 'Present' as const,
              spelling: targetName,
              token: operation.name._tag === 'Present' ? operation.name.token : targetToken,
            }),
            conformanceImplementation: Object.freeze({
              ordinal: conformance.ordinal,
              operation: operation.name._tag === 'Present' ? operation.name.spelling : targetName,
              self: conformance.self,
            }),
          }),
        ]
      }),
  )
  // Inherent impls: `impl [<Binders>] Owner { fn ... }`. The head is validated syntactically here
  // (whole-family arguments, unbounded binders); ownership and collisions need the resolved owner
  // and are decided at declaration completion.
  const inherentNodes = implNodes.filter((node) => !isConformanceImpl(node))
  const inherentImpls: Array<InherentImplFact> = []
  const inherentMembers: Array<DeclarationFact> = []
  inherentNodes.forEach((node, ordinal) => {
    const headName = `inherent#${ordinal}`
    const collected = collectTypeParameters(source, node, headName)
    diagnostics.push(...collected.diagnostics)
    const selfType = Type.parameter({ module: source.id, name: headName }, -1, 'Self')
    const environment = new Map(collected.environment)
    environment.set('Self', selfType)
    const ownerSyntax = node.children.find(isDeclaredTypeNode)
    const owner: DeclaredTypeFact =
      ownerSyntax === undefined
        ? Object.freeze({ _tag: 'Unavailable' as const, syntax: node })
        : analyzeDeclaredType(source, ownerSyntax, environment).fact
    const ownerPath = ownerPathOf(ownerSyntax)
    const ownerTokens =
      ownerPath === undefined
        ? []
        : SyntaxTree.tokens(ownerPath).filter((token) => token.kind === 'Identifier')
    const ownerSpelling = ownerTokens.map((token) => spelling(source, token)).join('.')
    const headSpan = ownerTokens.at(0)?.span ?? ownerSyntax?.span ?? node.span
    // Whole-family check: the applied arguments must be exactly this impl's binders, in order.
    const binders = collected.facts.filter((parameter) => parameter.duplicateOf === undefined)
    const argumentNodes =
      ownerSyntax?.kind === 'AppliedType'
        ? childNode(ownerSyntax, 'TypeArgumentList').children.filter(
            (child): child is SyntaxTree.Node =>
              SyntaxTree.isNode(child) &&
              (child.kind === 'LifetimeType' || isDeclaredTypeNode(child)),
          )
        : []
    const argumentSpellings = argumentNodes.map((argument) =>
      argument.kind === 'TypePath' || argument.kind === 'LifetimeType'
        ? SyntaxTree.tokens(argument)
            .filter((token) => token.kind === 'Identifier' || token.kind === 'Lifetime')
            .map((token) => spelling(source, token))
            .join('.')
        : undefined,
    )
    // Completed owner arity includes omitted lifetimes and is checked during completion.
    const wholeFamily =
      argumentSpellings.length === binders.length &&
      binders.every(
        (binder, index) =>
          binder.name._tag === 'Present' && argumentSpellings[index] === binder.name.spelling,
      )
    const bounded = binders.some(
      (binder) => binder.bounds.length > 0 || binder.representationBound !== undefined,
    )
    let headDiagnostic: Diagnostic.Diagnostic | undefined
    if (ownerPath === undefined || ownerTokens.length !== 1) {
      headDiagnostic = Diagnostic.invalidInherentHead(ownerSpelling || '?', 'NotNominal', headSpan)
    } else if (!wholeFamily) {
      headDiagnostic = Diagnostic.invalidInherentHead(ownerSpelling, 'Specialized', headSpan)
    } else if (bounded) {
      headDiagnostic = Diagnostic.invalidInherentHead(ownerSpelling, 'Bounded', headSpan)
    } else {
      headDiagnostic = undefined
    }
    if (headDiagnostic !== undefined) diagnostics.push(headDiagnostic)
    inherentImpls.push(
      Object.freeze({
        _tag: 'InherentImplDeclaration' as const,
        lifetimeElaboration: collected.lifetimeContext,
        module: source.id,
        ordinal,
        self: selfType,
        typeParameters: collected.facts,
        ownerSpelling,
        owner,
        validity:
          headDiagnostic === undefined
            ? Object.freeze({ _tag: 'Valid' as const })
            : Object.freeze({
                _tag: 'Invalid' as const,
                cause: Diagnostic.identity(headDiagnostic),
              }),
        syntax: node,
      }),
    )
    for (const mapped of SyntaxTree.directNodes(node, 'ImplOperation')) {
      const name = presentName(source, mapped)
      diagnostics.push(
        Diagnostic.invalidInherentMember(
          ownerSpelling,
          name._tag === 'Present' ? name.spelling : '?',
          'MappedOperation',
          mapped.span,
        ),
      )
    }
    SyntaxTree.directNodes(node, 'FunctionDeclaration').forEach((member, memberIndex) => {
      const name = presentName(source, member)
      if (SyntaxTree.directToken(member, 'DropKeyword') !== undefined) {
        diagnostics.push(
          Diagnostic.invalidInherentMember(ownerSpelling, 'drop', 'DropHook', member.span),
        )
        return
      }
      const id: DeclarationId = Object.freeze({
        _tag: 'DeclarationId',
        sourceId: source.id,
        ordinal: nestedDeclarationOrdinal + (conformances.length + ordinal) * 1024 + memberIndex,
      })
      const memberName = name._tag === 'Present' ? name.spelling : `member#${memberIndex}`
      // The member's own binders are minted under the member's identity, not the owner's, so two
      // members' `?R` binders never share one key and one member can call another with inference.
      const { refinedBinders, ...built } = implMember(
        member,
        id,
        `${ownerSpelling}.${memberName}`,
        collected.facts,
        selfType,
      )
      const shared = Object.freeze({
        ...built,
        typeParameters: joinedTypeParameters(member, collected.facts, built, refinedBinders),
      })
      const receiverParameter = shared.parameters.at(0)
      const receiver =
        receiverParameter !== undefined &&
        receiverParameter.name._tag === 'Present' &&
        receiverParameter.name.spelling === 'self' &&
        declaredTypeNamesOwner(receiverParameter.declaredType, selfType, ownerSpelling)
      const canonicalName = `${ownerSpelling}.${memberName}`
      const fact: DeclarationFact = Object.freeze({
        ...shared,
        id,
        canonical:
          headDiagnostic === undefined && name._tag === 'Present'
            ? Object.freeze({
                _tag: 'Canonical' as const,
                id: Object.freeze({
                  _tag: 'CanonicalDeclarationId' as const,
                  module: source.id,
                  name: canonicalName,
                }),
              })
            : Object.freeze({ _tag: 'Unidentified' as const }),
        visibility:
          SyntaxTree.directToken(member, 'PubKeyword') === undefined
            ? ('Private' as const)
            : ('Public' as const),
        name,
        associatedMember: Object.freeze({
          ordinal,
          ownerSpelling,
          name: memberName,
          self: selfType,
          receiver,
        }),
      })
      inherentMembers.push(fact)
    })
  })
  // A name declared twice for one owner has no winner: both facts become duplicates of the shared
  // identity, so neither is reachable and both sites are diagnosed with the other related.
  const inherentCounts = new Map<string, number>()
  for (const member of inherentMembers)
    if (member.canonical._tag === 'Canonical')
      inherentCounts.set(
        member.canonical.id.name,
        (inherentCounts.get(member.canonical.id.name) ?? 0) + 1,
      )
  const dedupedInherentMembers: ReadonlyArray<MemberFact> = inherentMembers.map((member) => {
    if (member.canonical._tag !== 'Canonical') return member
    if ((inherentCounts.get(member.canonical.id.name) ?? 0) < 2) return member
    const association = member.associatedMember
    const others = inherentMembers.filter(
      (candidate) =>
        candidate !== member &&
        candidate.canonical._tag === 'Canonical' &&
        member.canonical._tag === 'Canonical' &&
        candidate.canonical.id.name === member.canonical.id.name,
    )
    const otherSpan = nameSpanOf(others.at(0) ?? member)
    const diagnostic = Diagnostic.duplicateInherentMember(
      association?.ownerSpelling ?? '?',
      association?.name ?? '?',
      member.name._tag === 'Present' ? member.name.token.span : member.syntax.span,
      otherSpan,
    )
    diagnostics.push(diagnostic)
    return Object.freeze({
      ...member,
      canonical: Object.freeze({
        _tag: 'Duplicate' as const,
        original: member.canonical.id,
        cause: Diagnostic.identity(diagnostic),
      }),
    })
  })
  // Drop hook bodies elaborate as hidden generic functions: each accepted hook joins the member
  // list under a non-identifier canonical name, carrying the impl's type parameters, so ordinary
  // elaboration, ownership, and lowering machinery compile it without a hook-shaped special case.
  const hookMembers = conformances.flatMap((conformance, hookIndex): ReadonlyArray<MemberFact> => {
    const hook = conformance.hook
    if (hook === undefined) return []
    const node = hook.syntax
    const id: DeclarationId = Object.freeze({
      _tag: 'DeclarationId',
      sourceId: source.id,
      ordinal: nestedDeclarationOrdinal + inlineMembers.length + hookIndex,
    })
    const environment = new Map<string, Type.Parameter>(
      conformance.typeParameters.flatMap((parameter) =>
        parameter.duplicateOf === undefined && parameter.name._tag === 'Present'
          ? [[parameter.name.spelling, parameter.type] as const]
          : [],
      ),
    )
    const lifetimeContext = DeclarationLifetime.forHeader(
      source,
      { module: source.id, name: `drop@impl#${conformance.ordinal}` },
      node,
      environment,
    )
    diagnostics.push(...lifetimeContext.diagnostics)
    const parameterList = childNode(node, 'ParameterList')
    const parameters = SyntaxTree.directNodes(parameterList, 'ParameterDeclaration').map(
      (parameter, parameterOrdinal) =>
        analyzeParameter(source, parameter, id, parameterOrdinal, environment, lifetimeContext),
    )
    const returnType = analyzeDeclaredType(
      source,
      declaredTypeNode(childNode(node, 'ReturnType')),
      environment,
      false,
      lifetimeContext,
    )
    const facts = Object.freeze(parameters.map((parameter) => parameter.fact))
    return [
      Object.freeze({
        _tag: 'FunctionDeclaration' as const,
        id,
        canonical: Object.freeze({
          _tag: 'Canonical' as const,
          id: Object.freeze({
            _tag: 'CanonicalDeclarationId' as const,
            module: source.id,
            name: `drop@impl#${conformance.ordinal}`,
          }),
        }),
        visibility: 'Private' as const,
        phase: 'Runtime' as const,
        functionKind: 'Ordinary' as const,
        unsafe: false,
        typeParameters: conformance.typeParameters,
        parameterCount: facts.length,
        parameters: facts,
        name: hook.name,
        returnType: returnType.fact,
        failureRow: hook.failureRow,
        requirementRow: hook.requirementRow,
        constraints: Object.freeze([]),
        constraintContracts: Object.freeze([]),
        syntax: node,
      }),
    ]
  })
  const nativeRequirements: Array<NativeRequirement.NativeRequirement> = []
  for (const declaration of declarations) {
    for (const clause of DeclarationProperty.clauses(declaration)) {
      const moduleClause = declaration.kind === 'ModulePropertyDeclaration'
      if (!moduleClause && DeclarationProperty.owner(source, clause) !== 'Intrinsic.native')
        continue
      if (
        !moduleClause &&
        declaration.kind !== 'ForeignFunctionDeclaration' &&
        declaration.kind !== 'ForeignStaticDeclaration'
      )
        continue
      const token = SyntaxTree.directToken(declaration, 'Identifier')
      const member = ownMembers.find((entry) => entry.syntax === declaration)
      const canonical =
        member?.canonical._tag === 'Canonical' ? member.canonical.id.name : undefined
      const scope: NativeRequirement.Scope = moduleClause
        ? { kind: 'module', module: source.id }
        : {
            kind: 'declaration',
            module: source.id,
            declaration: canonical ?? (token === undefined ? '' : spelling(source, token)),
          }
      const analyzed = NativeRequirement.analyze(source, clause, scope)
      diagnostics.push(...analyzed.diagnostics)
      if (analyzed.requirement !== undefined) nativeRequirements.push(analyzed.requirement)
    }
  }
  const members: ReadonlyArray<MemberFact> = [
    ...ownMembers,
    ...inlineMembers,
    ...dedupedInherentMembers,
    ...hookMembers,
  ]
  return Object.freeze({
    _tag: 'ModuleHeaders',
    module: source.id,
    nativeRequirements: Object.freeze(nativeRequirements),
    publications: Object.freeze(
      declarations.flatMap((declaration): ModuleHeaders['publications'] => {
        if (
          declaration.kind !== 'ImportDeclaration' ||
          SyntaxTree.directToken(declaration, 'PubKeyword') === undefined
        )
          return []
        const path = SyntaxTree.directNode(declaration, 'ImportPath')
        const list = SyntaxTree.directNode(declaration, 'ImportMemberList')
        const module =
          path === undefined
            ? undefined
            : (imports.find((imported) => imported.syntax === declaration)?.canonicalTarget ??
              ImportPath.canonicalTarget(source, path))
        if (module === undefined || list === undefined) return []
        return SyntaxTree.directNodes(list, 'ImportMember').flatMap((member) => {
          const original = SyntaxTree.directToken(member, 'Identifier')
          if (original === undefined || !SyntaxTree.isAvailableSyntax(member)) return []
          const alias = SyntaxTree.directNode(member, 'ImportAlias')
          const token =
            (alias === undefined ? undefined : SyntaxTree.directToken(alias, 'Identifier')) ??
            original
          return [
            Object.freeze({
              module,
              original: spelling(source, original),
              spelling: spelling(source, token),
              syntax: member,
              token,
            }),
          ]
        })
      }),
    ),
    members: Object.freeze(members),
    declarations: Object.freeze(
      members.filter((member): member is DeclarationFact => member._tag === 'FunctionDeclaration'),
    ),
    structs: Object.freeze(
      members.filter((member): member is StructFact => member._tag === 'StructDeclaration'),
    ),
    enums: Object.freeze(
      members.filter((member): member is EnumFact => member._tag === 'EnumDeclaration'),
    ),
    unions: Object.freeze(
      members.filter((member): member is UnionFact => member._tag === 'UnionDeclaration'),
    ),
    services: Object.freeze(
      members.filter((member): member is ServiceFact => member._tag === 'ServiceDeclaration'),
    ),
    interfaces: Object.freeze(
      members.filter((member): member is InterfaceFact => member._tag === 'InterfaceDeclaration'),
    ),
    constants: Object.freeze(
      members.filter(
        (member): member is ConstantFact =>
          member._tag === 'ConstantDeclaration' || member._tag === 'PackageParameterDeclaration',
      ),
    ),
    conformances: Object.freeze(conformances),
    inherentImpls: Object.freeze(inherentImpls),
    diagnostics: Object.freeze(diagnostics.sort(compareDiagnostics)),
  })
}

/** The type path of an inherent impl head: bare or applied. */
const ownerPathOf = (ownerSyntax: SyntaxTree.Node | undefined): SyntaxTree.Node | undefined => {
  if (ownerSyntax === undefined) return undefined
  if (ownerSyntax.kind === 'AppliedType') return SyntaxTree.directNode(ownerSyntax, 'TypePath')
  return ownerSyntax.kind === 'TypePath' ? ownerSyntax : undefined
}

/** The span a declaration is reported at: its name when present, else its whole syntax. */
const nameSpanOf = (member: DeclarationFact): SourceSpan.SourceSpan =>
  member.name._tag === 'Present' ? member.name.token.span : member.syntax.span

/**
 * Whether a receiver parameter's declared type names the owner: `Self`, `&Self`, `&mut Self`, or
 * the owner's own spelling with any arguments, so `self: once Effect<A ! E ? R>` on a zero-data
 * `Effect` owner does not count while `self: &Counter` on `impl Counter` does.
 */
const declaredTypeNamesOwner = (
  declared: DeclaredTypeFact,
  self: Type.Parameter,
  ownerSpelling: string,
): boolean => {
  if (declared._tag === 'Reference')
    return declaredTypeNamesOwner(declared.target, self, ownerSpelling)
  if (declared._tag === 'Applied')
    return declaredTypeNamesOwner(declared.target, self, ownerSpelling)
  if (declared._tag === 'Resolved')
    return Type.isParameter(declared.type) && Type.key(declared.type) === Type.key(self)
  if (declared._tag === 'Unresolved') return declared.path.spelling === ownerSpelling
  return false
}

/** Collects identities and raw type paths for the complete closure before scope resolution. */
export const collect = (closure: ModuleClosure.Facts): DeclarationIndex.Index => {
  const modules = Object.freeze(
    closure.modules.map((module) =>
      collectModule(module.syntax, module.declarations, module.imports),
    ),
  )
  return DeclarationIndex.make(
    'Collected',
    modules,
    Diagnostic.merge(...modules.map((module) => module.diagnostics)),
  )
}

/**
 * Resolves one `typeof` item to the exact representation of a named callable declaration.
 *
 * The item must resolve to exactly one callable declaration whose generic parameters are all
 * supplied, because an exact representation names one construction, not a family. The resulting
 * identity is built from the declaration's canonical module and name plus its canonical argument
 * keys, so it never depends on spelling, span, or source path.
 */

/** Replays header elision after nominal declarations have published their lifetime arity. */
export const finalizeLifetimeHeader = (
  member:
    | DeclarationFact
    | ServiceOperationFact
    | StructFact
    | UnionFact
    | InherentImplFact
    | ConformanceFact,
  nominalParameters: (path: TypePathFact) => ReadonlyArray<Type.Parameter> | undefined,
  implHead?: InherentImplFact | ConformanceFact,
):
  | DeclarationFact
  | ServiceOperationFact
  | StructFact
  | UnionFact
  | InherentImplFact
  | ConformanceFact => {
  const prior = member.lifetimeElaboration
  if (prior === undefined) return member
  // Owner lifetimes stay lexical while the member elaborates its own input/result relationship.
  // Inherent members retain otherwise unmentioned owner binders only through Self. Conformance
  // witnesses retain the entire head contract because selection supplies all its binders.
  const ambient = implHead?.typeParameters.filter((parameter) => parameter.implicitLifetime) ?? []
  const environment = new Map(prior.parameters)
  for (const parameter of ambient) environment.set(parameter.type.name, parameter.type)
  const retainsOwner =
    ambient.length > 0 &&
    (implHead?._tag === 'ConformanceDeclaration' ||
      SyntaxTree.tokens(prior.syntax).some(
        (token) => token.kind === 'Identifier' && spelling(prior.source, token) === 'Self',
      ))
  const context = DeclarationLifetime.forHeader(
    prior.source,
    prior.owner,
    prior.syntax,
    environment,
    undefined,
    (path) => {
      const analyzed = analyzeDeclaredType(prior.source, path, environment, true, prior)
      if (analyzed.fact._tag === 'Resolved' && Type.isNominal(analyzed.fact.type))
        return (
          (member._tag === 'InherentImplDeclaration' && analyzed.fact.path !== undefined
            ? nominalParameters(analyzed.fact.path)
            : undefined) ?? Type.intrinsicNominalParameters(analyzed.fact.type)
        )
      return analyzed.fact._tag === 'Unresolved' ? nominalParameters(analyzed.fact.path) : undefined
    },
  )
  const parameters = new Map(environment)
  const implicit: ReadonlyArray<TypeParameterFact> = context.implicit.map((binder) => {
    parameters.set(binder.parameter.name, binder.parameter)
    return Object.freeze({
      _tag: 'TypeParameterDeclaration',
      type: binder.parameter,
      name: Object.freeze({
        _tag: 'Present',
        spelling: binder.parameter.name,
        token: binder.token,
      }),
      syntax: binder.syntax,
      bounds: Object.freeze([]),
      staticProperties: Object.freeze([]),
      lifetimeBounds: Object.freeze([]),
      implicitLifetime: true,
    })
  })
  const typeParameters = Object.freeze([
    ...member.typeParameters.filter((parameter) => !parameter.implicitLifetime),
    ...(retainsOwner ? ambient : []),
    ...implicit,
  ])
  const declared = (fact: DeclaredTypeFact): DeclaredTypeFact => {
    if (
      fact._tag === 'Unavailable' &&
      fact.cause?.code !== Diagnostic.ambiguousLifetimeElisionCode &&
      fact.cause?.code !== Diagnostic.unknownLifetimeCode
    )
      return fact
    return SyntaxTree.isNode(fact.syntax) && isDeclaredTypeNode(fact.syntax)
      ? analyzeDeclaredType(prior.source, fact.syntax, parameters, false, context).fact
      : fact
  }
  const fields = (values: ReadonlyArray<FieldFact>): ReadonlyArray<FieldFact> =>
    Object.freeze(
      values.map((field) =>
        Object.freeze({ ...field, declaredType: declared(field.declaredType) }),
      ),
    )
  if (member._tag === 'ConformanceDeclaration')
    return Object.freeze({
      ...member,
      typeParameters,
      lifetimeElaboration: context,
      capability: declared(member.capability),
      provider: declared(member.provider),
      requirements: Object.freeze(
        member.requirements.map((requirement) =>
          Object.freeze({ ...requirement, capability: declared(requirement.capability) }),
        ),
      ),
    })
  if (member._tag === 'InherentImplDeclaration')
    return Object.freeze({
      ...member,
      typeParameters,
      lifetimeElaboration: context,
      owner: declared(member.owner),
    })
  if (member._tag === 'StructDeclaration')
    return Object.freeze({
      ...member,
      typeParameters,
      lifetimeElaboration: context,
      fields: fields(member.fields),
    })
  if (member._tag === 'UnionDeclaration')
    return Object.freeze({
      ...member,
      typeParameters,
      lifetimeElaboration: context,
      variants: Object.freeze(
        member.variants.map((variant) =>
          Object.freeze({ ...variant, fields: fields(variant.fields) }),
        ),
      ),
    })
  const returnSyntax = SyntaxTree.directNode(prior.syntax, 'ReturnType')
  const opaqueReturn =
    member.opaqueResult === undefined || returnSyntax === undefined
      ? undefined
      : collectReturnType(
          prior.source,
          returnSyntax,
          prior.owner.name,
          typeParameters,
          parameters,
          context,
        )
  return Object.freeze({
    ...member,
    typeParameters,
    lifetimeElaboration: context,
    parameters: Object.freeze(
      member.parameters.map((parameter) =>
        Object.freeze({ ...parameter, declaredType: declared(parameter.declaredType) }),
      ),
    ),
    returnType: opaqueReturn?.fact ?? declared(member.returnType),
    ...(opaqueReturn?.opaqueResult === undefined
      ? {}
      : { opaqueResult: opaqueReturn.opaqueResult }),
    failureRow: collectFailureRow(prior.source, prior.syntax, parameters, context).fact,
    requirementRow: collectRequirementRow(prior.source, prior.syntax, parameters, context).fact,
  })
}
