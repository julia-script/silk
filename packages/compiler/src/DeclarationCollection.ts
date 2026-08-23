import * as Option from 'effect/Option'
import type {
  ArrayLengthFact,
  BoundFact,
  CanonicalId,
  CanonicalState,
  ConformanceFact,
  ConformanceRequirementFact,
  ConstantFact,
  ConstantLiteralFact,
  ConstraintFact,
  DeclarationFact,
  DeclarationId,
  DeclaredName,
  DeclaredTypeFact,
  FailureRowFact,
  FieldFact,
  FieldId,
  FieldState,
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
  StructFact,
  TypeParameterFact,
  TypePathFact,
  TypeResolution,
} from './DeclarationFacts.js'
import {
  interfaceOperationContracts,
  presentParameterEntries,
  requirementRoleIdentity,
} from './DeclarationFacts.js'
import * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as DigitSeparator from './internal/DigitSeparator.js'
import * as IntegerLiteral from './internal/IntegerLiteral.js'
import * as LiteralForm from './LiteralForm.js'
import type * as ModuleClosure from './ModuleClosure.js'
import * as Operator from './Operator.js'
import * as RequirementRow from './RequirementRow.js'
import * as RowAlgebra from './RowAlgebra.js'
import * as SourceFile from './SourceFile.js'
import type * as SourceSpan from './SourceSpan.js'
import * as StaticText from './StaticText.js'
import type * as SyntaxFile from './SyntaxFile.js'
import * as SyntaxTree from './SyntaxTree.js'
import * as TargetConstant from './TargetConstant.js'
import * as Token from './Token.js'
import * as Type from './Type.js'

export const spelling = (source: SourceFile.SourceFile, token: Token.Token): string =>
  Option.getOrThrowWith(
    SourceFile.spelling(source, token.span),
    () => new RangeError(`Header token span does not belong to source ${source.id}`),
  )

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
    element.kind === 'CallableType' ||
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
  const target = targetConstant(source, initializer)
  if (target !== undefined) return target
  return Object.freeze({ _tag: 'Unavailable', syntax: initializer })
}

/**
 * Recognizes `Target.<fact>`, the one initializer form that is not a source literal. The projection
 * is matched on syntax alone — `Target` names no declaration and resolves to nothing — so a form
 * that is already rejected today is the only form whose meaning changes.
 */
const targetConstant = (
  source: SourceFile.SourceFile,
  initializer: SyntaxTree.Node,
): ConstantLiteralFact | undefined => {
  if (initializer.kind !== 'FieldProjectionExpression') return undefined
  const base = SyntaxTree.directNode(initializer, 'IdentifierExpression')
  const baseToken = base === undefined ? undefined : SyntaxTree.directToken(base, 'Identifier')
  if (baseToken === undefined || spelling(source, baseToken) !== TargetConstant.root)
    return undefined
  const member = SyntaxTree.directToken(initializer, 'Identifier')
  if (member === undefined) return Object.freeze({ _tag: 'Unavailable', syntax: initializer })
  const memberSpelling = spelling(source, member)
  const selector = TargetConstant.find(memberSpelling)
  return selector === undefined
    ? Object.freeze({
        _tag: 'Malformed',
        detail: `${TargetConstant.root}.${memberSpelling} names no target fact; the target facts are ${TargetConstant.all.map((candidate) => `${TargetConstant.root}.${candidate}`).join(', ')}`,
        syntax: initializer,
      })
    : Object.freeze({ _tag: 'TargetConstant', selector, token: member })
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
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

/** Analyzes the failure and requirement arguments shared by Effect and nominal applications. */
const analyzeAppliedRows = (
  source: SourceFile.SourceFile,
  list: SyntaxTree.Node,
  typeParameters: ReadonlyMap<string, Type.Parameter>,
): AppliedRows => {
  const failureRowSyntax = SyntaxTree.directNode(list, 'FailureRow')
  const failureType = failureRowSyntax?.children.find(isDeclaredTypeNode)
  const failureNodes =
    failureType?.kind === 'UnionType'
      ? failureType.children.filter(isDeclaredTypeNode)
      : failureType === undefined
        ? []
        : [failureType]
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const failures = failureNodes.flatMap((member): ReadonlyArray<TypeResolution> => {
    const parameter = parameterAtTypePath(source, member, typeParameters)
    if (parameter?.kind !== 'RequirementRow')
      return [analyzeDeclaredType(source, member, typeParameters)]
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
              : analyzeDeclaredType(source, capability, typeParameters),
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
  return Object.freeze({
    failureRowSyntax,
    failures: Object.freeze(failures),
    requirementRowSyntax,
    requirements: Object.freeze(requirements),
    requirementParameters: Object.freeze(requirementParameters),
    rowParameterComponents: Object.freeze(rowParameterComponents),
    diagnostics: Object.freeze(diagnostics),
  })
}

export const analyzeDeclaredType = (
  source: SourceFile.SourceFile,
  syntax: SyntaxTree.Node,
  typeParameters: ReadonlyMap<string, Type.Parameter> = new Map(),
  genericArgumentPosition = false,
): TypeResolution => {
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
    const token = SyntaxTree.directToken(syntax, 'FnKeyword')
    const typeNodes = syntax.children.filter(isDeclaredTypeNode)
    const resultSyntax = typeNodes.at(-1)
    if (token === undefined || resultSyntax === undefined) {
      return Object.freeze({
        fact: Object.freeze({ _tag: 'Unavailable', syntax }),
        diagnostics: Object.freeze([]),
      })
    }
    const mode: Type.CallableMode =
      SyntaxTree.directToken(syntax, 'OnceKeyword') !== undefined
        ? 'Take'
        : SyntaxTree.directToken(syntax, 'MutKeyword') !== undefined
          ? 'Exclusive'
          : 'Shared'
    const unsafe = SyntaxTree.directToken(syntax, 'UnsafeKeyword') !== undefined
    const analyzed = typeNodes.map((node) => analyzeDeclaredType(source, node, typeParameters))
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
        unsafe,
        mode,
        parameters: Object.freeze(parameters.map((entry) => entry.fact)),
        result: resultFact,
        spelling: `${unsafe ? 'unsafe ' : ''}${mode === 'Exclusive' ? 'mut ' : mode === 'Take' ? 'once ' : ''}fn(...)`,
        token,
        syntax,
        ...(cause === undefined ? {} : { cause }),
      }),
      diagnostics,
    })
  }
  if (syntax.kind === 'ParenthesizedType') {
    const inner = syntax.children.find(isDeclaredTypeNode)
    if (inner === undefined)
      return Object.freeze({
        fact: Object.freeze({ _tag: 'Unavailable', syntax }),
        diagnostics: Object.freeze([]),
      })
    const analyzed = analyzeDeclaredType(source, inner, typeParameters, genericArgumentPosition)
    return Object.freeze({
      fact: Object.freeze({ ...analyzed.fact, syntax }),
      diagnostics: analyzed.diagnostics,
    })
  }
  if (syntax.kind === 'UnionType') {
    const members = syntax.children
      .filter(isDeclaredTypeNode)
      .map((member) => analyzeDeclaredType(source, member, typeParameters))
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
    const element = analyzeDeclaredType(source, elementSyntax, typeParameters)
    if (element.fact._tag === 'Resolved') {
      const type = Type.slice(access, element.fact.type)
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
    const target = analyzeDeclaredType(source, targetSyntax, typeParameters)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Reference',
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
    const element = analyzeDeclaredType(source, elementSyntax, typeParameters)
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
    const pathSyntax =
      item === undefined
        ? undefined
        : item.kind === 'TypePath'
          ? item
          : SyntaxTree.directNode(item, 'TypePath')
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
    const arguments_ = (list?.children.filter(isDeclaredTypeNode) ?? []).map((argument) =>
      analyzeDeclaredType(source, argument, typeParameters, true),
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
    const target = analyzeDeclaredType(source, pathSyntax, typeParameters)
    const arguments_ = list.children
      .filter(isDeclaredTypeNode)
      .map((argument) => analyzeDeclaredType(source, argument, typeParameters, true))
    const pathSegments = SyntaxTree.tokens(pathSyntax)
      .filter((token) => token.kind === 'Identifier')
      .map((token) => spelling(source, token))
    if (pathSegments.length === 1 && pathSegments.at(0) === 'Effect') {
      const access: Type.Effect['access'] =
        SyntaxTree.directToken(syntax, 'OnceKeyword') !== undefined
          ? 'Take'
          : SyntaxTree.directToken(syntax, 'MutKeyword') !== undefined
            ? 'Exclusive'
            : 'Shared'
      const {
        failures,
        requirements,
        requirementParameters,
        rowParameterComponents,
        diagnostics: rowDiagnostics,
      } = analyzeAppliedRows(source, list, typeParameters)
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
      if (
        arguments_.length === 1 &&
        success?._tag === 'Resolved' &&
        failuresAvailable &&
        resolvedRequirements.length === requirements.length
      ) {
        const type = Type.effect(
          success.type,
          resolvedFailures,
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
    } = analyzeAppliedRows(source, list, typeParameters)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Applied',
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
  if (
    segments.length === 1 &&
    (Type.isBuiltin(first.spelling) || Type.isString(first.spelling) || first.spelling === 'never')
  ) {
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
  const type = analyzeDeclaredType(source, declaredTypeNode(node), typeParameters)
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'ParameterDeclaration',
      id: Object.freeze({ _tag: 'ParameterId', function: functionId, ordinal }),
      name,
      declaredType: type.fact,
      syntax: node,
    }),
    diagnostics: type.diagnostics,
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
  structId: DeclarationId,
  typeParameters: ReadonlyMap<string, Type.Parameter>,
) => {
  const first = new Map<string, { readonly id: FieldId; readonly token: Token.Token }>()
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const fields = SyntaxTree.directNodes(node, 'StructField').map(
    (fieldNode, ordinal): FieldFact => {
      const id: FieldId = Object.freeze({ _tag: 'FieldId', struct: structId, ordinal })
      const name = presentName(source, fieldNode)
      const type = analyzeDeclaredType(source, declaredTypeNode(fieldNode), typeParameters)
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
        _tag: 'StructField',
        id,
        state,
        visibility:
          SyntaxTree.directToken(fieldNode, 'PubKeyword') === undefined ? 'Private' : 'Public',
        name,
        declaredType: type.fact,
        syntax: fieldNode,
      })
    },
  )
  return Object.freeze({ fields: Object.freeze(fields), diagnostics: Object.freeze(diagnostics) })
}

const compareDiagnostics = (left: Diagnostic.Diagnostic, right: Diagnostic.Diagnostic): number =>
  left.span.start - right.span.start ||
  left.span.end - right.span.end ||
  (left.code < right.code ? -1 : left.code > right.code ? 1 : 0)

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
} => {
  const list = SyntaxTree.directNode(node, 'TypeParameterList')
  if (list === undefined) {
    return Object.freeze({
      facts: Object.freeze([]),
      environment: new Map(),
      diagnostics: Object.freeze([]),
    })
  }
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
  const facts = SyntaxTree.directNodes(list, 'TypeParameter').map((parameterNode, ordinal) => {
    const name = presentName(source, parameterNode)
    // Every direct type node after the colon is one conjunct. Taking only direct children keeps
    // nested type arguments from being mistaken for sibling bounds.
    const boundNodes = parameterNode.children.filter(SyntaxTree.isNode)
    const boundNode = boundNodes.at(0)
    const boundResolution =
      boundNode === undefined ? undefined : analyzeDeclaredType(source, boundNode, environment)
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
    const rawStaticProperties = boundNodes.slice(1).map(staticPropertyOf)
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
    const representationKind: Type.ParameterKind | undefined =
      boundNode?.kind === 'CallableType'
        ? 'CallableRepresentation'
        : effectBound
          ? 'EffectRepresentation'
          : undefined
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
    const bounds: ReadonlyArray<BoundFact> =
      representationKind !== undefined ||
      SyntaxTree.directToken(parameterNode, 'Colon') === undefined
        ? Object.freeze([])
        : Object.freeze(
            boundNodes.flatMap((candidate): ReadonlyArray<BoundFact> => {
              const token = SyntaxTree.tokens(candidate).find((part) => part.kind === 'Identifier')
              if (token === undefined) return []
              const resolution = analyzeDeclaredType(source, candidate, environment)
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
    const duplicateOf = name._tag === 'Present' ? environment.get(name.spelling) : undefined
    const type =
      duplicateOf ??
      Type.parameter(
        { module: source.id, name: ownerName },
        ordinalOffset + ordinal,
        name._tag === 'Present' ? name.spelling : `#${ordinal}`,
        SyntaxTree.directToken(parameterNode, 'Question') !== undefined
          ? 'RequirementRow'
          : (representationKind ?? 'Value'),
        representationContract,
        representationKind === undefined ? Object.freeze([]) : staticProperties,
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
      staticProperties: representationKind === undefined ? Object.freeze([]) : staticProperties,
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
  return Object.freeze({
    facts: Object.freeze(facts),
    environment,
    diagnostics: Object.freeze(diagnostics),
  })
}

const collectReturnType = (
  source: SourceFile.SourceFile,
  returnSyntax: SyntaxTree.Node,
  ownerName: string,
  typeParameters: ReadonlyArray<TypeParameterFact>,
  ambientParameters: ReadonlyMap<string, Type.Parameter> = new Map(),
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

const collectRowExpression = (
  source: SourceFile.SourceFile,
  syntax: SyntaxTree.Node,
  typeParameters: ReadonlyMap<string, Type.Parameter>,
  leaf: 'Failure' | 'Requirement',
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
    const sourceRow = collectRowExpression(source, left, typeParameters, leaf)
    const selected = collectRowExpression(source, right, typeParameters, leaf)
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
      .map((operand) => collectRowExpression(source, operand, typeParameters, leaf))
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
    const analyzed = analyzeDeclaredType(source, syntax, typeParameters)
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
  const analyzed = analyzeDeclaredType(source, capabilitySyntax, typeParameters)
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
const emptyFailureRow = RowAlgebra.concrete(Type.failureRowPolicy(), [])
const emptyRequirementRow = RowAlgebra.concrete(Type.requirementRowPolicy(), [])

const collectFailureRow = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  typeParameters: ReadonlyMap<string, Type.Parameter>,
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
  const expression = collectRowExpression(source, declared, typeParameters, 'Failure')
  const syntaxMembers =
    declared.kind === 'UnionType'
      ? declared.children.filter(isDeclaredTypeNode)
      : isDeclaredTypeNode(declared)
        ? Object.freeze([declared])
        : Object.freeze([])
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
    const analyzed = analyzeDeclaredType(source, member, typeParameters)
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
  const expressions = rowNodes.map((member) =>
    collectRowExpression(source, member, typeParameters, 'Requirement'),
  )
  // Entry collection below owns source diagnostics. The expression facts are
  // structural and must not duplicate the same diagnostic occurrence.
  const expression = expressions.reduce<RowExpressionFact>(
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
  const entries = SyntaxTree.directNodes(syntax, 'Requirement').map((requirement) => {
    const capabilitySyntax = requirement.children.find(isDeclaredTypeNode)
    const analyzed =
      capabilitySyntax === undefined
        ? Object.freeze({
            fact: Object.freeze({ _tag: 'Unavailable' as const, syntax: requirement }),
            diagnostics: Object.freeze<ReadonlyArray<Diagnostic.Diagnostic>>([]),
          })
        : analyzeDeclaredType(source, capabilitySyntax, typeParameters)
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
  syntax.children.flatMap(
    (element): ReadonlyArray<SyntaxTree.Node> =>
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
    return [
      Object.freeze({
        _tag: 'ProviderConstraint',
        mode:
          providerSyntax.kind !== 'ReferenceType'
            ? 'Take'
            : SyntaxTree.directToken(providerSyntax, 'MutKeyword') === undefined
              ? 'Shared'
              : 'Exclusive',
        provider: provider.fact,
        selected: selected.fact,
        source: sourceRow.fact,
        syntax: element,
      }),
    ]
  })
  return Object.freeze({ facts: Object.freeze(facts), diagnostics: Object.freeze(diagnostics) })
}

const collectModule = (syntax: SyntaxFile.SyntaxFile): ModuleHeaders => {
  const source = syntax.source
  const nodes = syntax.root.children.filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) &&
      (element.kind === 'FunctionDeclaration' ||
        element.kind === 'StructDeclaration' ||
        element.kind === 'ServiceDeclaration' ||
        element.kind === 'InterfaceDeclaration' ||
        element.kind === 'RoleDeclaration' ||
        element.kind === 'ConstantDeclaration'),
  )
  const first = new Map<string, { readonly id: CanonicalId; readonly token: Token.Token }>()
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const conformances = syntax.root.children
    .filter(
      (element): element is SyntaxTree.Node =>
        SyntaxTree.isNode(element) && element.kind === 'ImplDeclaration',
    )
    .map((node, ordinal): ConformanceFact => {
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
        (parameter): ReadonlyArray<ConformanceRequirementFact> =>
          parameter.duplicateOf !== undefined
            ? []
            : parameter.bounds.map((bound) =>
                Object.freeze({
                  _tag: 'ConformanceRequirement' as const,
                  parameter: parameter.type,
                  spelling: bound.spelling,
                  capability: analyzeDeclaredType(source, bound.path.syntax, environment).fact,
                  syntax: bound.path.syntax,
                }),
              ),
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
                    : analyzeDeclaredType(source, parameterTypeSyntax, environment).fact,
                returnType:
                  returnTypeSyntax === undefined
                    ? Object.freeze({
                        _tag: 'Unavailable' as const,
                        syntax: returnSyntax ?? hookSyntax,
                      })
                    : analyzeDeclaredType(source, returnTypeSyntax, environment).fact,
                failureRow: failure.fact,
                requirementRow: requirements.fact,
                syntax: hookSyntax,
              })
            })()
      return Object.freeze({
        _tag: 'ConformanceDeclaration',
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
    if (node.kind === 'ConstantDeclaration') {
      const initializer =
        node.children.find(
          (element): element is SyntaxTree.Node =>
            SyntaxTree.isNode(element) && !isDeclaredTypeNode(element),
        ) ?? node
      const declaredType = analyzeDeclaredType(source, declaredTypeNode(node))
      diagnostics.push(...declaredType.diagnostics)
      return Object.freeze({
        _tag: 'ConstantDeclaration',
        id,
        canonical,
        visibility,
        typeParameters: Object.freeze([]),
        name,
        declaredType: declaredType.fact,
        literal: constantLiteral(source, initializer),
        initializer,
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
    if (node.kind === 'StructDeclaration') {
      const collected = collectFields(source, node, id, typeParameters.environment)
      diagnostics.push(...collected.diagnostics)
      return Object.freeze({
        _tag: 'StructDeclaration',
        id,
        canonical,
        visibility,
        typeParameters: typeParameters.facts,
        name,
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
          )
          diagnostics.push(...operationTypeParameters.diagnostics)
          const environment = new Map<string, Type.Parameter>([
            ...contractEnvironment,
            ...operationTypeParameters.environment,
          ])
          const parameterList = childNode(operation, 'ParameterList')
          const parameters = SyntaxTree.directNodes(parameterList, 'ParameterDeclaration').map(
            (parameter, parameterOrdinal) =>
              analyzeParameter(source, parameter, operationId, parameterOrdinal, environment),
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
                )
          const failureRow = collectFailureRow(source, operation, environment)
          const requirementRow = collectRequirementRow(source, operation, environment)
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
            const detail =
              node.kind !== 'InterfaceDeclaration'
                ? 'only interface operations may declare an operator'
                : operationTypeParameters.facts.length > 0
                  ? 'operator operations cannot declare operation-local type parameters'
                  : selectedOperator === undefined
                    ? `${operatorToken === undefined ? 'the marker' : Token.describe(operatorToken.kind)} is not an eligible ${parameterFacts.length}-operand operator`
                    : undefined
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
            parameterCount: parameterFacts.length,
            parameters: parameterFacts,
            ...(operatorSyntax !== undefined &&
            operatorToken !== undefined &&
            selectedOperator !== undefined &&
            node.kind === 'InterfaceDeclaration' &&
            operationTypeParameters.facts.length === 0
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
        analyzeParameter(source, parameter, id, parameterOrdinal, typeParameters.environment),
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
          )
    const functionKind =
      SyntaxTree.directToken(node, 'EffectKeyword') === undefined ? 'Ordinary' : 'Effect'
    const failureRow = collectFailureRow(source, node, typeParameters.environment)
    const requirementRow = collectRequirementRow(source, node, typeParameters.environment)
    const constraints = collectConstraints(source, node, typeParameters.environment)
    const facts = Object.freeze(parameters.map((parameter) => parameter.fact))
    diagnostics.push(
      ...parameters.flatMap((parameter) => parameter.diagnostics),
      ...duplicateParameterDiagnostics(facts),
      ...returnType.diagnostics,
      ...failureRow.diagnostics,
      ...requirementRow.diagnostics,
      ...constraints.diagnostics,
    )
    if (functionKind === 'Ordinary' && failureRow.fact.syntax !== undefined)
      diagnostics.push(Diagnostic.failureChannelOnOrdinary(failureRow.fact.syntax.span))
    return Object.freeze({
      _tag: 'FunctionDeclaration',
      id,
      canonical,
      visibility,
      functionKind,
      unsafe: SyntaxTree.directToken(node, 'UnsafeKeyword') !== undefined,
      typeParameters: typeParameters.facts,
      parameterCount: facts.length,
      parameters: facts,
      name,
      returnType: returnType.fact,
      ...(returnType.opaqueResult === undefined ? {} : { opaqueResult: returnType.opaqueResult }),
      failureRow: failureRow.fact,
      requirementRow: requirementRow.fact,
      constraints: constraints.facts,
      constraintContracts: Object.freeze([]),
      syntax: node,
    })
  })
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
        const collected = collectTypeParameters(
          source,
          node,
          targetName,
          conformance.typeParameters.length,
          conformance.typeParameters,
        )
        diagnostics.push(...collected.diagnostics)
        const environment = new Map(collected.environment)
        environment.set('Self', conformance.self)
        const parameterList = childNode(node, 'ParameterList')
        const parameters = SyntaxTree.directNodes(parameterList, 'ParameterDeclaration').map(
          (parameter, ordinal) => analyzeParameter(source, parameter, id, ordinal, environment),
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
            : collectReturnType(source, returnSyntax, targetName, collected.facts, environment)
        const failureRow = collectFailureRow(source, node, environment)
        const requirementRow = collectRequirementRow(source, node, environment)
        const constraints = collectConstraints(source, node, environment)
        const parameterFacts = Object.freeze(parameters.map((parameter) => parameter.fact))
        diagnostics.push(
          ...parameters.flatMap((parameter) => parameter.diagnostics),
          ...duplicateParameterDiagnostics(parameterFacts),
          ...returnType.diagnostics,
          ...failureRow.diagnostics,
          ...requirementRow.diagnostics,
          ...constraints.diagnostics,
        )
        return [
          Object.freeze({
            _tag: 'FunctionDeclaration' as const,
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
            functionKind:
              SyntaxTree.directToken(node, 'EffectKeyword') === undefined
                ? ('Ordinary' as const)
                : ('Effect' as const),
            unsafe: SyntaxTree.directToken(node, 'UnsafeKeyword') !== undefined,
            typeParameters: collected.facts,
            parameterCount: parameterFacts.length,
            parameters: parameterFacts,
            name: Object.freeze({
              _tag: 'Present' as const,
              spelling: targetName,
              token: operation.name._tag === 'Present' ? operation.name.token : targetToken,
            }),
            returnType: returnType.fact,
            ...('opaqueResult' in returnType && returnType.opaqueResult !== undefined
              ? { opaqueResult: returnType.opaqueResult }
              : {}),
            failureRow: failureRow.fact,
            requirementRow: requirementRow.fact,
            constraints: constraints.facts,
            constraintContracts: Object.freeze([]),
            conformanceImplementation: Object.freeze({
              ordinal: conformance.ordinal,
              operation: operation.name._tag === 'Present' ? operation.name.spelling : targetName,
              self: conformance.self,
            }),
            syntax: node,
          }),
        ]
      }),
  )
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
    const parameterList = childNode(node, 'ParameterList')
    const parameters = SyntaxTree.directNodes(parameterList, 'ParameterDeclaration').map(
      (parameter, parameterOrdinal) =>
        analyzeParameter(source, parameter, id, parameterOrdinal, environment),
    )
    const returnType = analyzeDeclaredType(
      source,
      declaredTypeNode(childNode(node, 'ReturnType')),
      environment,
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
  const members = [...ownMembers, ...inlineMembers, ...hookMembers]
  return Object.freeze({
    _tag: 'ModuleHeaders',
    module: source.id,
    members: Object.freeze(members),
    declarations: Object.freeze(
      members.filter((member): member is DeclarationFact => member._tag === 'FunctionDeclaration'),
    ),
    structs: Object.freeze(
      members.filter((member): member is StructFact => member._tag === 'StructDeclaration'),
    ),
    services: Object.freeze(
      members.filter((member): member is ServiceFact => member._tag === 'ServiceDeclaration'),
    ),
    interfaces: Object.freeze(
      members.filter((member): member is InterfaceFact => member._tag === 'InterfaceDeclaration'),
    ),
    constants: Object.freeze(
      members.filter((member): member is ConstantFact => member._tag === 'ConstantDeclaration'),
    ),
    conformances: Object.freeze(conformances),
    diagnostics: Object.freeze(diagnostics.sort(compareDiagnostics)),
  })
}

/** Collects identities and raw type paths for the complete closure before scope resolution. */
export const collect = (closure: ModuleClosure.Facts): DeclarationIndex.Index => {
  const modules = Object.freeze(closure.modules.map((module) => collectModule(module.syntax)))
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
