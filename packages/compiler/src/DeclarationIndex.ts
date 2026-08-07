import * as Option from 'effect/Option'
import * as Diagnostic from './Diagnostic.js'
import type * as ModuleClosure from './ModuleClosure.js'
import * as SourceFile from './SourceFile.js'
import type * as SourceSpan from './SourceSpan.js'
import type * as SyntaxFile from './SyntaxFile.js'
import * as SyntaxTree from './SyntaxTree.js'
import type * as Token from './Token.js'
import * as Type from './Type.js'

/** The semantic types recognized in declaration and executable analysis. */
export type SemanticType = Type.Type

/** A deterministic declaration identity local to one analyzed source snapshot. */
export interface DeclarationId {
  readonly _tag: 'DeclarationId'
  readonly sourceId: string
  readonly ordinal: number
}

/** A deterministic parameter identity nested under its owning function declaration. */
export interface ParameterId {
  readonly _tag: 'ParameterId'
  readonly function: DeclarationId
  readonly ordinal: number
}

/** A deterministic field identity nested under its owning struct declaration. */
export interface FieldId {
  readonly _tag: 'FieldId'
  readonly struct: DeclarationId
  readonly ordinal: number
}

/** The canonical identity of one declaration: canonical module identity plus name. */
export interface CanonicalId {
  readonly _tag: 'CanonicalDeclarationId'
  readonly module: string
  readonly name: string
}

/** One ordered, declaration-owned generic type parameter with exact source provenance. */
export interface TypeParameterFact {
  readonly _tag: 'TypeParameterDeclaration'
  readonly type: Type.Parameter
  readonly name: DeclaredName
  readonly syntax: SyntaxTree.Node
  readonly duplicateOf?: Type.Parameter
}

/** The canonical, duplicate, or unidentified canonical-identity state of one header. */
export type CanonicalState =
  | { readonly _tag: 'Canonical'; readonly id: CanonicalId }
  | {
      readonly _tag: 'Duplicate'
      readonly original: CanonicalId
      readonly cause: Diagnostic.Identity
    }
  | { readonly _tag: 'Unidentified' }

/** A declaration or field name supplied by syntax or explicitly unavailable after recovery. */
export type DeclaredName =
  | { readonly _tag: 'Present'; readonly spelling: string; readonly token: Token.Token }
  | { readonly _tag: 'Unavailable'; readonly syntax: SyntaxTree.Element }

/** The exact one- or two-segment syntax retained for a declared type lookup. */
export interface TypePathFact {
  readonly _tag: 'TypePath'
  readonly spelling: string
  readonly segments: ReadonlyArray<{ readonly spelling: string; readonly token: Token.Token }>
  readonly syntax: SyntaxTree.Node
}

/** The normalized or unavailable decimal length retained by fixed-array type syntax. */
export type ArrayLengthFact =
  | {
      readonly _tag: 'Available'
      readonly value: number
      readonly spelling: string
      readonly token: Token.Token
    }
  | {
      readonly _tag: 'OutOfRange'
      readonly spelling: string
      readonly token: Token.Token
      readonly cause: Diagnostic.Identity
    }
  | { readonly _tag: 'Unavailable'; readonly syntax: SyntaxTree.Element }

/** The resolved, unresolved, or syntax-unavailable declared type. */
export type DeclaredTypeFact =
  | {
      readonly _tag: 'Resolved'
      readonly type: SemanticType
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Element
      readonly exposureCause?: Diagnostic.Identity
      readonly unionSource?: UnionSourceFact
    }
  | {
      readonly _tag: 'Unresolved'
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Element
      readonly path: TypePathFact
      readonly cause?: Diagnostic.Identity
      readonly candidate?: Type.Nominal
    }
  | {
      readonly _tag: 'FixedArray'
      readonly element: DeclaredTypeFact
      readonly length: ArrayLengthFact
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'Slice'
      readonly access: Type.Slice['access']
      readonly element: DeclaredTypeFact
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Node
      readonly cause?: Diagnostic.Identity
    }
  | {
      readonly _tag: 'Applied'
      readonly target: DeclaredTypeFact
      readonly arguments: ReadonlyArray<DeclaredTypeFact>
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Node
      readonly cause?: Diagnostic.Identity
    }
  | {
      readonly _tag: 'Effect'
      readonly success: DeclaredTypeFact
      readonly failures: ReadonlyArray<DeclaredTypeFact>
      readonly requirements: ReadonlyArray<{
        readonly capability: DeclaredTypeFact
        readonly role: string
        readonly access: Type.Requirement['access']
        readonly syntax: SyntaxTree.Node
      }>
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Node
      readonly cause?: Diagnostic.Identity
    }
  | {
      readonly _tag: 'Union'
      readonly members: ReadonlyArray<DeclaredTypeFact>
      readonly separators: ReadonlyArray<Token.Token>
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Node
      readonly cause?: Diagnostic.Identity
    }
  | {
      readonly _tag: 'Unavailable'
      readonly syntax: SyntaxTree.Element
      readonly cause?: Diagnostic.Identity
    }

/** Source-ordered union syntax retained beside one normalized resolved outcome. */
export interface UnionSourceFact {
  readonly _tag: 'UnionSource'
  readonly members: ReadonlyArray<DeclaredTypeFact>
  readonly separators: ReadonlyArray<Token.Token>
  readonly syntax: SyntaxTree.Node
}

export type ReturnTypeFact = DeclaredTypeFact

/** A source-retained and canonically normalized effect failure row. */
export interface FailureRowFact {
  readonly _tag: 'FailureRow'
  readonly members: ReadonlyArray<DeclaredTypeFact>
  readonly failures: ReadonlyArray<Type.Nominal>
  readonly syntax?: SyntaxTree.Node
  readonly available: boolean
}

/** A source-retained and canonically normalized Effect capability requirement row. */
export interface RequirementRowFact {
  readonly _tag: 'RequirementRow'
  readonly entries: ReadonlyArray<{
    readonly capability: DeclaredTypeFact
    readonly role: string
    readonly access: Type.Requirement['access']
    readonly syntax: SyntaxTree.Node
  }>
  readonly requirements: ReadonlyArray<Type.Requirement>
  readonly syntax?: SyntaxTree.Node
  readonly available: boolean
}

/** One ordered parameter declaration with exact concrete provenance. */
export interface ParameterFact {
  readonly _tag: 'ParameterDeclaration'
  readonly id: ParameterId
  readonly name: DeclaredName
  readonly declaredType: DeclaredTypeFact
  readonly syntax: SyntaxTree.Node
}

/** One function declaration header and its syntax-owned semantic facts. */
export interface DeclarationFact {
  readonly _tag: 'FunctionDeclaration'
  readonly id: DeclarationId
  readonly canonical: CanonicalState
  readonly visibility: 'Public' | 'Private'
  readonly functionKind: 'Ordinary' | 'Effect'
  readonly typeParameters: ReadonlyArray<TypeParameterFact>
  readonly parameterCount: number
  readonly parameters: ReadonlyArray<ParameterFact>
  readonly name: DeclaredName
  readonly returnType: ReturnTypeFact
  readonly failureRow: FailureRowFact
  readonly requirementRow: RequirementRowFact
  readonly syntax: SyntaxTree.Node
}

/** The unique, duplicate, or unidentified state of one field name. */
export type FieldState =
  | { readonly _tag: 'Unique'; readonly id: FieldId }
  | { readonly _tag: 'Duplicate'; readonly original: FieldId; readonly cause: Diagnostic.Identity }
  | { readonly _tag: 'Unidentified' }

/** One ordered nominal struct field header. */
export interface FieldFact {
  readonly _tag: 'StructField'
  readonly id: FieldId
  readonly state: FieldState
  readonly visibility: 'Public' | 'Private'
  readonly name: DeclaredName
  readonly declaredType: DeclaredTypeFact
  readonly syntax: SyntaxTree.Node
}

/** The finite dependency state of one nominal struct declaration. */
export type StructDependency =
  | { readonly _tag: 'Available'; readonly types: ReadonlyArray<Type.Nominal> }
  | {
      readonly _tag: 'Unavailable'
      readonly types: ReadonlyArray<Type.Nominal>
      readonly cause?: Diagnostic.Identity
    }

/** One nominal struct declaration header and its ordered fields. */
export interface StructFact {
  readonly _tag: 'StructDeclaration'
  readonly id: DeclarationId
  readonly canonical: CanonicalState
  readonly visibility: 'Public' | 'Private'
  readonly typeParameters: ReadonlyArray<TypeParameterFact>
  readonly name: DeclaredName
  readonly fields: ReadonlyArray<FieldFact>
  readonly dependency: StructDependency
  readonly syntax: SyntaxTree.Node
}

/** Any declaration kind occupying the shared module-level namespace. */
export type MemberFact = DeclarationFact | StructFact

export type ParameterLookup =
  | { readonly _tag: 'Resolved'; readonly spelling: string; readonly parameter: ParameterFact }
  | { readonly _tag: 'Missing'; readonly spelling: string }
  | {
      readonly _tag: 'Ambiguous'
      readonly spelling: string
      readonly parameters: ReadonlyArray<ParameterFact>
    }

export type DeclarationLookup =
  | { readonly _tag: 'Resolved'; readonly spelling: string; readonly declaration: DeclarationFact }
  | { readonly _tag: 'Missing'; readonly spelling: string }
  | {
      readonly _tag: 'Ambiguous'
      readonly spelling: string
      readonly declarations: ReadonlyArray<DeclarationFact>
    }

export type MemberLookup =
  | { readonly _tag: 'Resolved'; readonly spelling: string; readonly declaration: MemberFact }
  | { readonly _tag: 'Missing'; readonly spelling: string }
  | {
      readonly _tag: 'Ambiguous'
      readonly spelling: string
      readonly declarations: ReadonlyArray<MemberFact>
    }

export type StructLookup =
  | { readonly _tag: 'Resolved'; readonly spelling: string; readonly declaration: StructFact }
  | { readonly _tag: 'Missing'; readonly spelling: string }
  | {
      readonly _tag: 'Ambiguous'
      readonly spelling: string
      readonly declarations: ReadonlyArray<StructFact>
    }

export type FieldLookup =
  | { readonly _tag: 'Resolved'; readonly spelling: string; readonly field: FieldFact }
  | { readonly _tag: 'Missing'; readonly spelling: string }
  | {
      readonly _tag: 'Ambiguous'
      readonly spelling: string
      readonly fields: ReadonlyArray<FieldFact>
    }

/** One module's collected headers with their header-level diagnostics. */
export interface ModuleHeaders {
  readonly _tag: 'ModuleHeaders'
  readonly module: string
  readonly members: ReadonlyArray<MemberFact>
  readonly declarations: ReadonlyArray<DeclarationFact>
  readonly structs: ReadonlyArray<StructFact>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

/** The immutable declaration index of one loaded closure. */
export interface Index {
  readonly _tag: 'DeclarationIndex'
  readonly stage: 'Collected' | 'Complete'
  readonly modules: ReadonlyArray<ModuleHeaders>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

export interface TypeResolution {
  readonly fact: DeclaredTypeFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

export type TypeResolver = (module: string, path: TypePathFact) => TypeResolution

const spelling = (source: SourceFile.SourceFile, token: Token.Token): string =>
  Option.getOrThrowWith(
    SourceFile.spelling(source, token.span),
    () => new RangeError(`Header token span does not belong to source ${source.id}`),
  )

const childNode = (parent: SyntaxTree.Node, kind: SyntaxTree.NodeKind): SyntaxTree.Node => {
  const child = SyntaxTree.directNode(parent, kind)
  if (child === undefined)
    throw new RangeError(`Header collection expected ${kind} below ${parent.kind}`)
  return child
}

const isDeclaredTypeNode = (element: SyntaxTree.Element): element is SyntaxTree.Node =>
  SyntaxTree.isNode(element) &&
  (element.kind === 'TypePath' ||
    element.kind === 'AppliedType' ||
    element.kind === 'FixedArrayType' ||
    element.kind === 'SliceType' ||
    element.kind === 'ParenthesizedType' ||
    element.kind === 'UnionType')

const declaredTypeNode = (parent: SyntaxTree.Node): SyntaxTree.Node => {
  const child = parent.children.find((element): element is SyntaxTree.Node =>
    isDeclaredTypeNode(element),
  )
  if (child === undefined) throw new RangeError(`Header collection expected a declared type`)
  return child
}

const presentName = (source: SourceFile.SourceFile, node: SyntaxTree.Node): DeclaredName => {
  const token = SyntaxTree.directToken(node, 'Identifier')
  return token === undefined
    ? Object.freeze({
        _tag: 'Unavailable',
        syntax: SyntaxTree.unavailableChild(node, 'Identifier'),
      })
    : Object.freeze({ _tag: 'Present', spelling: spelling(source, token), token })
}

/** Retains and initially resolves one concrete type path against built-in types only. */
export const analyzeDeclaredType = (
  source: SourceFile.SourceFile,
  syntax: SyntaxTree.Node,
  typeParameters: ReadonlyMap<string, Type.Parameter> = new Map(),
): TypeResolution => {
  if (syntax.kind === 'ParenthesizedType') {
    const inner = syntax.children.find(isDeclaredTypeNode)
    if (inner === undefined)
      return Object.freeze({
        fact: Object.freeze({ _tag: 'Unavailable', syntax }),
        diagnostics: Object.freeze([]),
      })
    const analyzed = analyzeDeclaredType(source, inner, typeParameters)
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
      const value = Number(lengthSpelling)
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
      .map((argument) => analyzeDeclaredType(source, argument, typeParameters))
    const pathSegments = SyntaxTree.tokens(pathSyntax)
      .filter((token) => token.kind === 'Identifier')
      .map((token) => spelling(source, token))
    if (pathSegments.length === 1 && pathSegments.at(0) === 'Effect') {
      const failureRow = SyntaxTree.directNode(list, 'FailureRow')
      const failureType =
        failureRow === undefined ? undefined : failureRow.children.find(isDeclaredTypeNode)
      const failureNodes =
        failureType?.kind === 'UnionType'
          ? failureType.children.filter(isDeclaredTypeNode)
          : failureType === undefined
            ? []
            : [failureType]
      const failures = failureNodes.map((member) =>
        analyzeDeclaredType(source, member, typeParameters),
      )
      const requirements =
        SyntaxTree.directNode(list, 'RequirementRow')
          ?.children.filter(
            (element): element is SyntaxTree.Node =>
              SyntaxTree.isNode(element) && element.kind === 'Requirement',
          )
          .map((requirement) => {
            const capability = SyntaxTree.directNode(requirement, 'TypePath')
            const analyzed =
              capability === undefined
                ? Object.freeze({
                    fact: Object.freeze({ _tag: 'Unavailable' as const, syntax: requirement }),
                    diagnostics: Object.freeze<ReadonlyArray<Diagnostic.Diagnostic>>([]),
                  })
                : analyzeDeclaredType(source, capability, typeParameters)
            const role = SyntaxTree.directToken(requirement, 'Identifier')
            return Object.freeze({
              capability: analyzed,
              role: role === undefined ? 'DefaultRole' : spelling(source, role),
              access:
                SyntaxTree.directToken(requirement, 'MutKeyword') === undefined
                  ? ('Shared' as const)
                  : ('Exclusive' as const),
              syntax: requirement,
            })
          }) ?? []
      const diagnostics = [
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
        failure.fact._tag === 'Resolved' && Type.isNominal(failure.fact.type)
          ? [failure.fact.type]
          : [],
      )
      const resolvedRequirements = requirements.flatMap((requirement) =>
        requirement.capability.fact._tag === 'Resolved' &&
        Type.isNominal(requirement.capability.fact.type)
          ? [
              Object.freeze({
                capability: requirement.capability.fact.type,
                role: requirement.role,
                access: requirement.access,
              }),
            ]
          : [],
      )
      const failuresAvailable = failures.every(
        (failure) =>
          failure.fact._tag === 'Resolved' &&
          (Type.isNominal(failure.fact.type) || Type.isNever(failure.fact.type)),
      )
      if (
        arguments_.length === 1 &&
        success?._tag === 'Resolved' &&
        failuresAvailable &&
        resolvedRequirements.length === requirements.length
      ) {
        const type = Type.effect(success.type, resolvedFailures, 'Shared', resolvedRequirements)
        return Object.freeze({
          fact: Object.freeze({
            _tag: 'Resolved',
            type,
            spelling: Type.encode(type),
            token: firstToken,
            syntax,
          }),
          diagnostics: Object.freeze(diagnostics),
        })
      }
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Effect',
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
          spelling: 'Effect',
          token: firstToken,
          syntax,
        }),
        diagnostics: Object.freeze(diagnostics),
      })
    }
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Applied',
        target: target.fact,
        arguments: Object.freeze(arguments_.map((argument) => argument.fact)),
        spelling: SyntaxTree.tokens(syntax)
          .filter((token) => !['Whitespace', 'LineComment', 'DocComment'].includes(token.kind))
          .map((token) => spelling(source, token))
          .join(''),
        token: firstToken,
        syntax,
      }),
      diagnostics: Diagnostic.merge(
        target.diagnostics,
        ...arguments_.map((argument) => argument.diagnostics),
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
    (first.spelling === 'I32' ||
      first.spelling === 'Usize' ||
      first.spelling === 'Bool' ||
      first.spelling === 'Never')
  ) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Resolved',
        type: first.spelling,
        spelling: first.spelling,
        token: first.token,
        syntax,
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
      }),
      diagnostics: Object.freeze([]),
    })
  }
  const parameterType = segments.length === 1 ? typeParameters.get(first.spelling) : undefined
  if (parameterType !== undefined) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Resolved',
        type: parameterType,
        spelling: first.spelling,
        token: first.token,
        syntax,
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
): { readonly fact: ParameterFact; readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic> } => {
  const colonIndex = node.children.findIndex((element) => isSeparator(element, 'Colon'))
  const nameElements = colonIndex < 0 ? node.children : node.children.slice(0, colonIndex)
  const nameToken = identifierToken(nameElements)
  const name: DeclaredName =
    nameToken === undefined
      ? Object.freeze({
          _tag: 'Unavailable',
          syntax: SyntaxTree.unavailableElement(nameElements, node),
        })
      : Object.freeze({ _tag: 'Present', spelling: spelling(source, nameToken), token: nameToken })
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

const presentParameterEntries = (parameters: ReadonlyArray<ParameterFact>) =>
  parameters.flatMap((parameter) =>
    parameter.name._tag === 'Present'
      ? [
          Object.freeze({
            spelling: parameter.name.spelling,
            token: parameter.name.token,
            parameter,
          }),
        ]
      : [],
  )

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
  const environment = new Map<string, Type.Parameter>()
  const originals = new Map<string, SourceSpan.SourceSpan>()
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const facts = SyntaxTree.directNodes(list, 'TypeParameter').map((parameterNode, ordinal) => {
    const name = presentName(source, parameterNode)
    const duplicateOf = name._tag === 'Present' ? environment.get(name.spelling) : undefined
    const type =
      duplicateOf ??
      Type.parameter(
        { module: source.id, name: ownerName },
        ordinal,
        name._tag === 'Present' ? name.spelling : `#${ordinal}`,
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
      ...(duplicateOf === undefined ? {} : { duplicateOf }),
    })
  })
  return Object.freeze({
    facts: Object.freeze(facts),
    environment,
    diagnostics: Object.freeze(diagnostics),
  })
}

const failureMembers = (fact: DeclaredTypeFact): ReadonlyArray<DeclaredTypeFact> => {
  if (fact._tag === 'Resolved' && fact.unionSource !== undefined) return fact.unionSource.members
  if (fact._tag === 'Union') return fact.members
  return Object.freeze([fact])
}

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
        failures: Object.freeze([]),
        available: true,
      }),
      diagnostics: Object.freeze([]),
    })
  const analyzed = analyzeDeclaredType(source, declaredTypeNode(syntax), typeParameters)
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'FailureRow',
      members: Object.freeze(failureMembers(analyzed.fact)),
      failures: Object.freeze([]),
      syntax,
      available: false,
    }),
    diagnostics: analyzed.diagnostics,
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
        requirements: Object.freeze([]),
        available: true,
      }),
      diagnostics: Object.freeze([]),
    })
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const entries = SyntaxTree.directNodes(syntax, 'Requirement').map((requirement) => {
    const capabilitySyntax = SyntaxTree.directNode(requirement, 'TypePath')
    const analyzed =
      capabilitySyntax === undefined
        ? Object.freeze({
            fact: Object.freeze({ _tag: 'Unavailable' as const, syntax: requirement }),
            diagnostics: Object.freeze<ReadonlyArray<Diagnostic.Diagnostic>>([]),
          })
        : analyzeDeclaredType(source, capabilitySyntax, typeParameters)
    diagnostics.push(...analyzed.diagnostics)
    const roleToken = SyntaxTree.directToken(requirement, 'Identifier')
    return Object.freeze({
      capability: analyzed.fact,
      role: roleToken === undefined ? 'DefaultRole' : spelling(source, roleToken),
      access:
        SyntaxTree.directToken(requirement, 'MutKeyword') === undefined
          ? ('Shared' as const)
          : ('Exclusive' as const),
      syntax: requirement,
    })
  })
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'RequirementRow',
      entries: Object.freeze(entries),
      requirements: Object.freeze([]),
      syntax,
      available: false,
    }),
    diagnostics: Object.freeze(diagnostics),
  })
}

const collectModule = (syntax: SyntaxFile.SyntaxFile): ModuleHeaders => {
  const source = syntax.source
  const nodes = syntax.root.children.filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) &&
      (element.kind === 'FunctionDeclaration' || element.kind === 'StructDeclaration'),
  )
  const first = new Map<string, { readonly id: CanonicalId; readonly token: Token.Token }>()
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const members = nodes.map((node, ordinal): MemberFact => {
    const id: DeclarationId = Object.freeze({ _tag: 'DeclarationId', sourceId: source.id, ordinal })
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
    const visibility =
      SyntaxTree.directToken(node, 'PubKeyword') === undefined ? 'Private' : 'Public'
    const typeParameters = collectTypeParameters(
      source,
      node,
      name._tag === 'Present' ? name.spelling : `#${ordinal}`,
    )
    diagnostics.push(...typeParameters.diagnostics)
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
    const parameterList = childNode(node, 'ParameterList')
    const parameters = SyntaxTree.directNodes(parameterList, 'ParameterDeclaration').map(
      (parameter, parameterOrdinal) =>
        analyzeParameter(source, parameter, id, parameterOrdinal, typeParameters.environment),
    )
    const returnType = analyzeDeclaredType(
      source,
      declaredTypeNode(childNode(node, 'ReturnType')),
      typeParameters.environment,
    )
    const functionKind =
      SyntaxTree.directToken(node, 'EffectKeyword') === undefined ? 'Ordinary' : 'Effect'
    const failureRow = collectFailureRow(source, node, typeParameters.environment)
    const requirementRow = collectRequirementRow(source, node, typeParameters.environment)
    const facts = Object.freeze(parameters.map((parameter) => parameter.fact))
    diagnostics.push(
      ...parameters.flatMap((parameter) => parameter.diagnostics),
      ...duplicateParameterDiagnostics(facts),
      ...returnType.diagnostics,
      ...failureRow.diagnostics,
      ...requirementRow.diagnostics,
    )
    if (functionKind === 'Ordinary' && failureRow.fact.syntax !== undefined)
      diagnostics.push(Diagnostic.failureRowOnOrdinary(failureRow.fact.syntax.span))
    return Object.freeze({
      _tag: 'FunctionDeclaration',
      id,
      canonical,
      visibility,
      functionKind,
      typeParameters: typeParameters.facts,
      parameterCount: facts.length,
      parameters: facts,
      name,
      returnType: returnType.fact,
      failureRow: failureRow.fact,
      requirementRow: requirementRow.fact,
      syntax: node,
    })
  })
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
    diagnostics: Object.freeze(diagnostics.sort(compareDiagnostics)),
  })
}

/** Collects identities and raw type paths for the complete closure before scope resolution. */
export const collect = (closure: ModuleClosure.Closure): Index => {
  const modules = Object.freeze(closure.modules.map((module) => collectModule(module.syntax)))
  return Object.freeze({
    _tag: 'DeclarationIndex',
    stage: 'Collected',
    modules,
    diagnostics: Diagnostic.merge(...modules.map((module) => module.diagnostics)),
  })
}

const resolveDeclaredType = (
  module: string,
  fact: DeclaredTypeFact,
  resolver: TypeResolver,
  modules: ReadonlyArray<ModuleHeaders>,
): TypeResolution => {
  if (fact._tag === 'Unresolved') {
    const resolved = resolver(module, fact.path)
    if (resolved.fact._tag !== 'Resolved' || !Type.isNominal(resolved.fact.type)) return resolved
    const declaration = memberByNominal(modules, resolved.fact.type)
    if (declaration === undefined || declaration.typeParameters.length === 0) return resolved
    const diagnostic = Diagnostic.typeArgumentArity(
      fact.spelling,
      declaration.typeParameters.length,
      0,
      fact.token.span,
    )
    return Object.freeze({
      fact: Object.freeze({
        ...fact,
        cause: Diagnostic.identity(diagnostic),
        candidate: resolved.fact.type,
      }),
      diagnostics: Object.freeze([diagnostic]),
    })
  }
  if (fact._tag === 'Effect') {
    const success = resolveDeclaredType(module, fact.success, resolver, modules)
    const failures = fact.failures.map((failure) =>
      resolveDeclaredType(module, failure, resolver, modules),
    )
    const requirements = fact.requirements.map((requirement) =>
      Object.freeze({
        ...requirement,
        capability: resolveDeclaredType(module, requirement.capability, resolver, modules),
      }),
    )
    const diagnostics: Array<Diagnostic.Diagnostic> = [
      ...success.diagnostics,
      ...failures.flatMap((failure) => failure.diagnostics),
      ...requirements.flatMap((requirement) => requirement.capability.diagnostics),
    ]
    const failureTypes: Array<Type.Nominal> = []
    let failuresAvailable = true
    for (const failure of failures) {
      if (failure.fact._tag === 'Resolved' && Type.isNominal(failure.fact.type)) {
        failureTypes.push(failure.fact.type)
      } else if (!(failure.fact._tag === 'Resolved' && Type.isNever(failure.fact.type))) {
        failuresAvailable = false
        if (failure.fact._tag === 'Resolved')
          diagnostics.push(
            Diagnostic.invalidFailureType(Type.encode(failure.fact.type), failure.fact.syntax.span),
          )
      }
    }
    const requirementTypes: Array<Type.Requirement> = []
    let requirementsAvailable = true
    for (const requirement of requirements) {
      if (
        requirement.capability.fact._tag === 'Resolved' &&
        Type.isNominal(requirement.capability.fact.type) &&
        Type.isConcrete(requirement.capability.fact.type)
      ) {
        requirementTypes.push(
          Object.freeze({
            capability: requirement.capability.fact.type,
            role: requirement.role,
            access: requirement.access,
          }),
        )
      } else {
        requirementsAvailable = false
        if (requirement.capability.fact._tag === 'Resolved')
          diagnostics.push(
            Diagnostic.invalidRequirementType(
              Type.encode(requirement.capability.fact.type),
              requirement.syntax.span,
            ),
          )
      }
    }
    if (success.fact._tag === 'Resolved' && failuresAvailable && requirementsAvailable) {
      const type = Type.effect(success.fact.type, failureTypes, 'Shared', requirementTypes)
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Resolved',
          type,
          spelling: Type.encode(type),
          token: fact.token,
          syntax: fact.syntax,
        }),
        diagnostics: Object.freeze(diagnostics),
      })
    }
    const cause = diagnostics.at(-1)
    return Object.freeze({
      fact: Object.freeze({
        ...fact,
        success: success.fact,
        failures: Object.freeze(failures.map((failure) => failure.fact)),
        requirements: Object.freeze(
          requirements.map((requirement) =>
            Object.freeze({ ...requirement, capability: requirement.capability.fact }),
          ),
        ),
        ...(cause === undefined ? {} : { cause: Diagnostic.identity(cause) }),
      }),
      diagnostics: Object.freeze(diagnostics),
    })
  }
  if (fact._tag === 'Applied') {
    const target =
      fact.target._tag === 'Unresolved'
        ? resolver(module, fact.target.path)
        : resolveDeclaredType(module, fact.target, resolver, modules)
    const arguments_ = fact.arguments.map((argument) =>
      resolveDeclaredType(module, argument, resolver, modules),
    )
    const diagnostics = [
      ...target.diagnostics,
      ...arguments_.flatMap((argument) => argument.diagnostics),
    ]
    if (target.fact._tag === 'Resolved' && Type.isNominal(target.fact.type)) {
      const declaration = memberByNominal(modules, target.fact.type)
      const expected = declaration?.typeParameters.length ?? 0
      const available = arguments_.map((argument) =>
        argument.fact._tag === 'Resolved' ? argument.fact.type : undefined,
      )
      if (expected === arguments_.length && available.every((argument) => argument !== undefined)) {
        const type = Type.nominal(
          target.fact.type.module,
          target.fact.type.name,
          available.filter((argument): argument is Type.Type => argument !== undefined),
        )
        return Object.freeze({
          fact: Object.freeze({
            _tag: 'Resolved',
            type,
            spelling: Type.encode(type),
            token: fact.token,
            syntax: fact.syntax,
          }),
          diagnostics: Object.freeze(diagnostics),
        })
      }
      if (expected === arguments_.length) {
        const unavailable = arguments_.find((argument) => argument.fact._tag !== 'Resolved')
        const cause =
          unavailable !== undefined && 'cause' in unavailable.fact
            ? unavailable.fact.cause
            : undefined
        return Object.freeze({
          fact: Object.freeze({ ...fact, ...(cause === undefined ? {} : { cause }) }),
          diagnostics: Object.freeze(diagnostics),
        })
      }
      const diagnostic = Diagnostic.typeArgumentArity(
        fact.spelling,
        expected,
        arguments_.length,
        fact.token.span,
      )
      diagnostics.push(diagnostic)
      return Object.freeze({
        fact: Object.freeze({ ...fact, cause: Diagnostic.identity(diagnostic) }),
        diagnostics: Object.freeze(diagnostics),
      })
    }
    return Object.freeze({ fact, diagnostics: Object.freeze(diagnostics) })
  }
  if (fact._tag === 'Union') {
    const resolvedMembers = fact.members.map((member) =>
      resolveDeclaredType(module, member, resolver, modules),
    )
    const diagnostics: Array<Diagnostic.Diagnostic> = resolvedMembers.flatMap((member) =>
      Array.from(member.diagnostics),
    )
    const members = Object.freeze(resolvedMembers.map((member) => member.fact))
    if (members.every((member) => member._tag === 'Resolved')) {
      const available = members.filter(
        (member): member is Extract<DeclaredTypeFact, { readonly _tag: 'Resolved' }> =>
          member._tag === 'Resolved',
      )
      const normalized = Type.union(available.map((member) => member.type))
      if (normalized._tag === 'Normalized') {
        return Object.freeze({
          fact: Object.freeze({
            _tag: 'Resolved' as const,
            type: normalized.type,
            spelling: Type.encode(normalized.type),
            token: fact.token,
            syntax: fact.syntax,
            unionSource: Object.freeze({
              _tag: 'UnionSource' as const,
              members,
              separators: fact.separators,
              syntax: fact.syntax,
            }),
          }),
          diagnostics: Object.freeze(diagnostics),
        })
      }
      for (const invalid of normalized.members) {
        const sourceFact = available.find((member) => Type.equals(member.type, invalid))
        diagnostics.push(
          Diagnostic.invalidUnionMember(
            Type.encode(invalid),
            sourceFact?.syntax.span ?? fact.syntax.span,
          ),
        )
      }
    }
    const cause = diagnostics.at(-1)
    return Object.freeze({
      fact: Object.freeze({
        ...fact,
        members,
        ...(cause === undefined ? {} : { cause: Diagnostic.identity(cause) }),
      }),
      diagnostics: Object.freeze(diagnostics),
    })
  }
  if (fact._tag === 'Slice') {
    const element = resolveDeclaredType(module, fact.element, resolver, modules)
    if (element.fact._tag === 'Resolved') {
      const type = Type.slice(fact.access, element.fact.type)
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Resolved',
          type,
          spelling: Type.encode(type),
          token: fact.token,
          syntax: fact.syntax,
          ...(element.fact.exposureCause === undefined
            ? {}
            : { exposureCause: element.fact.exposureCause }),
        }),
        diagnostics: element.diagnostics,
      })
    }
    const cause = 'cause' in element.fact ? element.fact.cause : undefined
    return Object.freeze({
      fact: Object.freeze({
        ...fact,
        element: element.fact,
        ...(cause === undefined ? {} : { cause }),
      }),
      diagnostics: element.diagnostics,
    })
  }
  if (fact._tag !== 'FixedArray') return Object.freeze({ fact, diagnostics: Object.freeze([]) })
  return (() => {
    const element = resolveDeclaredType(module, fact.element, resolver, modules)
    if (fact.length._tag !== 'Available') {
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Unavailable' as const,
          syntax: fact.syntax,
          ...(fact.length._tag === 'OutOfRange' ? { cause: fact.length.cause } : {}),
        }),
        diagnostics: element.diagnostics,
      })
    }
    if (element.fact._tag === 'Resolved') {
      const type = Type.fixedArray(element.fact.type, fact.length.value)
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Resolved' as const,
          type,
          spelling: Type.encode(type),
          token: fact.token,
          syntax: fact.syntax,
          ...(element.fact.exposureCause === undefined
            ? {}
            : { exposureCause: element.fact.exposureCause }),
        }),
        diagnostics: element.diagnostics,
      })
    }
    if (element.fact._tag === 'Unresolved') {
      return Object.freeze({
        fact: Object.freeze({
          ...element.fact,
          spelling: fact.spelling,
          token: fact.token,
          syntax: fact.syntax,
        }),
        diagnostics: element.diagnostics,
      })
    }
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Unavailable' as const,
        syntax: fact.syntax,
        ...(element.fact._tag === 'Unavailable' && element.fact.cause !== undefined
          ? { cause: element.fact.cause }
          : {}),
      }),
      diagnostics: element.diagnostics,
    })
  })()
}

const canonicalKey = (id: CanonicalId): string => `${id.module}.${id.name}`

const memberByNominal = (
  modules: ReadonlyArray<ModuleHeaders>,
  type: Type.Nominal,
): StructFact | undefined =>
  modules
    .find((module) => module.module === type.module)
    ?.structs.find(
      (struct) => struct.canonical._tag === 'Canonical' && struct.canonical.id.name === type.name,
    )

/** Resolves one retained type fact through a supplied module resolver and complete index. */
export const resolveTypeFact = (
  index: Index,
  module: string,
  fact: DeclaredTypeFact,
  resolver: TypeResolver,
): TypeResolution => resolveDeclaredType(module, fact, resolver, index.modules)

const resolveFailureRow = (
  module: string,
  row: FailureRowFact,
  resolver: TypeResolver,
  modules: ReadonlyArray<ModuleHeaders>,
): {
  readonly fact: FailureRowFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  if (row.syntax === undefined) return Object.freeze({ fact: row, diagnostics: Object.freeze([]) })
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const members = row.members.map((member) => {
    const resolved = resolveDeclaredType(module, member, resolver, modules)
    diagnostics.push(...resolved.diagnostics)
    return resolved.fact
  })
  const failures = new Map<string, Type.Nominal>()
  let available = true
  for (const member of members) {
    if (
      member._tag !== 'Resolved' ||
      !Type.isNominal(member.type) ||
      !Type.isConcrete(member.type)
    ) {
      available = false
      if (member._tag === 'Resolved')
        diagnostics.push(
          Diagnostic.invalidFailureType(Type.encode(member.type), member.syntax.span),
        )
      continue
    }
    failures.set(Type.key(member.type), member.type)
  }
  return Object.freeze({
    fact: Object.freeze({
      ...row,
      members: Object.freeze(members),
      failures: Object.freeze([...failures.values()].sort(Type.compare)),
      available,
    }),
    diagnostics: Object.freeze(diagnostics),
  })
}

const resolveRequirementRow = (
  module: string,
  row: RequirementRowFact,
  resolver: TypeResolver,
  modules: ReadonlyArray<ModuleHeaders>,
): {
  readonly fact: RequirementRowFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  if (row.syntax === undefined) return Object.freeze({ fact: row, diagnostics: Object.freeze([]) })
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const entries = row.entries.map((entry) => {
    const capability = resolveDeclaredType(module, entry.capability, resolver, modules)
    diagnostics.push(...capability.diagnostics)
    return Object.freeze({ ...entry, capability: capability.fact })
  })
  const requirements: Array<Type.Requirement> = []
  let available = true
  for (const entry of entries) {
    if (
      entry.capability._tag === 'Resolved' &&
      Type.isNominal(entry.capability.type) &&
      Type.isConcrete(entry.capability.type)
    ) {
      requirements.push(
        Object.freeze({
          capability: entry.capability.type,
          role: entry.role,
          access: entry.access,
        }),
      )
    } else {
      available = false
      if (entry.capability._tag === 'Resolved')
        diagnostics.push(
          Diagnostic.invalidRequirementType(Type.encode(entry.capability.type), entry.syntax.span),
        )
    }
  }
  const normalized = Type.effect('Never', [], 'Shared', requirements).requirements
  return Object.freeze({
    fact: Object.freeze({
      ...row,
      entries: Object.freeze(entries),
      requirements: normalized,
      available,
    }),
    diagnostics: Object.freeze(diagnostics),
  })
}

const attachExposure = (
  fact: DeclaredTypeFact,
  modules: ReadonlyArray<ModuleHeaders>,
  diagnostics: Array<Diagnostic.Diagnostic>,
): DeclaredTypeFact => {
  if (fact._tag !== 'Resolved') return fact
  const nominal = Type.nominals(fact.type).find(
    (candidate) => memberByNominal(modules, candidate)?.visibility === 'Private',
  )
  if (nominal === undefined) return fact
  const target = memberByNominal(modules, nominal)
  if (target?.visibility !== 'Private') return fact
  const diagnostic = Diagnostic.privateTypeExposure(Type.encode(nominal), fact.token.span)
  diagnostics.push(diagnostic)
  return Object.freeze({ ...fact, exposureCause: Diagnostic.identity(diagnostic) })
}

const stronglyConnected = (
  structs: ReadonlyArray<StructFact>,
): ReadonlyArray<ReadonlyArray<StructFact>> => {
  const canonical = structs
    .filter((struct) => struct.canonical._tag === 'Canonical')
    .sort((left, right) => {
      const leftId = left.canonical._tag === 'Canonical' ? left.canonical.id : undefined
      const rightId = right.canonical._tag === 'Canonical' ? right.canonical.id : undefined
      return leftId === undefined || rightId === undefined
        ? 0
        : canonicalKey(leftId).localeCompare(canonicalKey(rightId))
    })
  const byKey = new Map(
    canonical.flatMap((struct) =>
      struct.canonical._tag === 'Canonical'
        ? [[canonicalKey(struct.canonical.id), struct] as const]
        : [],
    ),
  )
  let nextIndex = 0
  const indices = new Map<string, number>()
  const lows = new Map<string, number>()
  const stack: Array<string> = []
  const stacked = new Set<string>()
  const components: Array<ReadonlyArray<StructFact>> = []
  const visit = (key: string): void => {
    indices.set(key, nextIndex)
    lows.set(key, nextIndex)
    nextIndex += 1
    stack.push(key)
    stacked.add(key)
    const struct = byKey.get(key)
    const neighbors = (struct?.fields ?? [])
      .flatMap((field) =>
        field.declaredType._tag === 'Resolved'
          ? Type.nominals(field.declaredType.type).map((type) => `${type.module}.${type.name}`)
          : [],
      )
      .filter((neighbor) => byKey.has(neighbor))
      .sort()
    for (const neighbor of neighbors) {
      if (!indices.has(neighbor)) {
        visit(neighbor)
        lows.set(key, Math.min(lows.get(key) ?? 0, lows.get(neighbor) ?? 0))
      } else if (stacked.has(neighbor)) {
        lows.set(key, Math.min(lows.get(key) ?? 0, indices.get(neighbor) ?? 0))
      }
    }
    if (lows.get(key) !== indices.get(key)) return
    const component: Array<StructFact> = []
    for (;;) {
      const memberKey = stack.pop()
      if (memberKey === undefined) break
      stacked.delete(memberKey)
      const member = byKey.get(memberKey)
      if (member !== undefined) component.push(member)
      if (memberKey === key) break
    }
    components.push(
      Object.freeze(
        component.sort((left, right) => {
          if (left.canonical._tag !== 'Canonical' || right.canonical._tag !== 'Canonical') return 0
          return canonicalKey(left.canonical.id).localeCompare(canonicalKey(right.canonical.id))
        }),
      ),
    )
  }
  for (const key of byKey.keys()) if (!indices.has(key)) visit(key)
  return Object.freeze(components)
}

/** Resolves all retained type paths and validates public exposure and inline dependencies. */
export const complete = (self: Index, resolver: TypeResolver): Index => {
  const diagnostics: Array<Diagnostic.Diagnostic> = [...self.diagnostics]
  let modules = self.modules.map((module): ModuleHeaders => {
    const members = module.members.map((member): MemberFact => {
      if (member._tag === 'FunctionDeclaration') {
        const parameters = member.parameters.map((parameter) => {
          const resolved = resolveDeclaredType(
            module.module,
            parameter.declaredType,
            resolver,
            self.modules,
          )
          diagnostics.push(...resolved.diagnostics)
          return Object.freeze({ ...parameter, declaredType: resolved.fact })
        })
        const result = resolveDeclaredType(module.module, member.returnType, resolver, self.modules)
        diagnostics.push(...result.diagnostics)
        const failureRow = resolveFailureRow(
          module.module,
          member.failureRow,
          resolver,
          self.modules,
        )
        diagnostics.push(...failureRow.diagnostics)
        const requirementRow = resolveRequirementRow(
          module.module,
          member.requirementRow,
          resolver,
          self.modules,
        )
        diagnostics.push(...requirementRow.diagnostics)
        return Object.freeze({
          ...member,
          parameters: Object.freeze(parameters),
          returnType: result.fact,
          failureRow: failureRow.fact,
          requirementRow: requirementRow.fact,
        })
      }
      const fields = member.fields.map((field) => {
        const resolved = resolveDeclaredType(
          module.module,
          field.declaredType,
          resolver,
          self.modules,
        )
        diagnostics.push(...resolved.diagnostics)
        return Object.freeze({ ...field, declaredType: resolved.fact })
      })
      return Object.freeze({ ...member, fields: Object.freeze(fields) })
    })
    return Object.freeze({
      ...module,
      members: Object.freeze(members),
      declarations: Object.freeze(
        members.filter(
          (member): member is DeclarationFact => member._tag === 'FunctionDeclaration',
        ),
      ),
      structs: Object.freeze(
        members.filter((member): member is StructFact => member._tag === 'StructDeclaration'),
      ),
    })
  })

  for (const module of modules) {
    for (const member of module.members) {
      if (member._tag === 'FunctionDeclaration') {
        for (const parameter of member.parameters) {
          if (
            parameter.declaredType._tag === 'Resolved' &&
            Type.containsBorrow(parameter.declaredType.type) &&
            (!Type.isSlice(parameter.declaredType.type) ||
              Type.containsBorrow(parameter.declaredType.type.element))
          ) {
            diagnostics.push(
              Diagnostic.sliceTypePosition('parameter', parameter.declaredType.syntax.span),
            )
          }
        }
        if (member.returnType._tag === 'Resolved' && Type.containsBorrow(member.returnType.type)) {
          diagnostics.push(Diagnostic.sliceTypePosition('return', member.returnType.syntax.span))
        }
        continue
      }
      for (const field of member.fields) {
        if (
          field.declaredType._tag === 'Resolved' &&
          Type.containsBorrow(field.declaredType.type)
        ) {
          diagnostics.push(Diagnostic.sliceTypePosition('field', field.declaredType.syntax.span))
        }
      }
    }
  }

  modules = modules.map((module): ModuleHeaders => {
    const members = module.members.map((member): MemberFact => {
      if (member.visibility !== 'Public') return member
      if (member._tag === 'FunctionDeclaration') {
        const parameters = member.parameters.map((parameter) =>
          Object.freeze({
            ...parameter,
            declaredType: attachExposure(parameter.declaredType, modules, diagnostics),
          }),
        )
        return Object.freeze({
          ...member,
          parameters: Object.freeze(parameters),
          returnType: attachExposure(member.returnType, modules, diagnostics),
          failureRow: Object.freeze({
            ...member.failureRow,
            members: Object.freeze(
              member.failureRow.members.map((failure) =>
                attachExposure(failure, modules, diagnostics),
              ),
            ),
          }),
        })
      }
      const fields = member.fields.map((field) =>
        field.visibility === 'Public'
          ? Object.freeze({
              ...field,
              declaredType: attachExposure(field.declaredType, modules, diagnostics),
            })
          : field,
      )
      return Object.freeze({ ...member, fields: Object.freeze(fields) })
    })
    return Object.freeze({
      ...module,
      members: Object.freeze(members),
      declarations: Object.freeze(
        members.filter(
          (member): member is DeclarationFact => member._tag === 'FunctionDeclaration',
        ),
      ),
      structs: Object.freeze(
        members.filter((member): member is StructFact => member._tag === 'StructDeclaration'),
      ),
    })
  })

  const structs = modules.flatMap((module) => module.structs)
  const cycleCause = new Map<string, Diagnostic.Identity>()
  for (const component of stronglyConnected(structs)) {
    const first = component.at(0)
    if (first === undefined) continue
    const keys = component.flatMap((struct) =>
      struct.canonical._tag === 'Canonical' ? [canonicalKey(struct.canonical.id)] : [],
    )
    const selfEdge =
      keys.length === 1 &&
      first.fields.some(
        (field) =>
          field.declaredType._tag === 'Resolved' &&
          Type.nominals(field.declaredType.type).some(
            (type) => `${type.module}.${type.name}` === keys[0],
          ),
      )
    if (keys.length < 2 && !selfEdge) continue
    const diagnostic = Diagnostic.inlineRecursiveStruct(
      Object.freeze(keys),
      first.name._tag === 'Present' ? first.name.token.span : first.syntax.span,
    )
    diagnostics.push(diagnostic)
    const cause = Diagnostic.identity(diagnostic)
    for (const key of keys) cycleCause.set(key, cause)
  }

  modules = modules.map((module): ModuleHeaders => {
    const members = module.members.map((member): MemberFact => {
      if (member._tag !== 'StructDeclaration') return member
      const dependencyMap = new Map<string, Type.Nominal>()
      for (const field of member.fields) {
        if (field.declaredType._tag === 'Resolved') {
          for (const type of Type.nominals(field.declaredType.type)) {
            dependencyMap.set(Type.key(type), type)
          }
        }
      }
      const dependencies = [...dependencyMap.values()].sort(Type.compare)
      const fieldCause = member.fields.find(
        (field) =>
          (field.declaredType._tag === 'Unresolved' && field.declaredType.cause !== undefined) ||
          (field.declaredType._tag === 'Resolved' &&
            field.declaredType.exposureCause !== undefined),
      )
      const key =
        member.canonical._tag === 'Canonical' ? canonicalKey(member.canonical.id) : undefined
      const cause =
        (key === undefined ? undefined : cycleCause.get(key)) ??
        (fieldCause?.declaredType._tag === 'Unresolved'
          ? fieldCause.declaredType.cause
          : fieldCause?.declaredType._tag === 'Resolved'
            ? fieldCause.declaredType.exposureCause
            : undefined)
      return Object.freeze({
        ...member,
        dependency: Object.freeze(
          cause === undefined
            ? { _tag: 'Available', types: Object.freeze(dependencies) }
            : { _tag: 'Unavailable', types: Object.freeze(dependencies), cause },
        ),
      })
    })
    const moduleDiagnostics = diagnostics.filter(
      (diagnostic) => diagnostic.span.sourceId === module.module,
    )
    return Object.freeze({
      ...module,
      members: Object.freeze(members),
      declarations: Object.freeze(
        members.filter(
          (member): member is DeclarationFact => member._tag === 'FunctionDeclaration',
        ),
      ),
      structs: Object.freeze(
        members.filter((member): member is StructFact => member._tag === 'StructDeclaration'),
      ),
      diagnostics: Diagnostic.merge(moduleDiagnostics),
    })
  })

  return Object.freeze({
    _tag: 'DeclarationIndex',
    stage: 'Complete',
    modules: Object.freeze(modules),
    diagnostics: Diagnostic.merge(diagnostics),
  })
}

const presentParameterNameEntries = (parameters: ReadonlyArray<ParameterFact>) =>
  presentParameterEntries(parameters)

export const lookupParameter = (
  parameters: ReadonlyArray<ParameterFact>,
  name: string,
): ParameterLookup => {
  const matches = presentParameterNameEntries(parameters)
    .filter((entry) => entry.spelling === name)
    .map((entry) => entry.parameter)
  const first = matches.at(0)
  if (first === undefined) return Object.freeze({ _tag: 'Missing', spelling: name })
  return matches.length === 1
    ? Object.freeze({ _tag: 'Resolved', spelling: name, parameter: first })
    : Object.freeze({ _tag: 'Ambiguous', spelling: name, parameters: Object.freeze(matches) })
}

const presentMemberEntries = (members: ReadonlyArray<MemberFact>) =>
  members.flatMap((declaration) =>
    declaration.name._tag === 'Present'
      ? [
          Object.freeze({
            spelling: declaration.name.spelling,
            token: declaration.name.token,
            declaration,
          }),
        ]
      : [],
  )

export const lookupMember = (members: ReadonlyArray<MemberFact>, name: string): MemberLookup => {
  const matches = presentMemberEntries(members)
    .filter((entry) => entry.spelling === name)
    .map((entry) => entry.declaration)
  const first = matches.at(0)
  if (first === undefined) return Object.freeze({ _tag: 'Missing', spelling: name })
  return matches.length === 1
    ? Object.freeze({ _tag: 'Resolved', spelling: name, declaration: first })
    : Object.freeze({ _tag: 'Ambiguous', spelling: name, declarations: Object.freeze(matches) })
}

export const lookupDeclaration = (
  declarations: ReadonlyArray<DeclarationFact>,
  name: string,
): DeclarationLookup => {
  const matches = declarations.filter(
    (declaration) => declaration.name._tag === 'Present' && declaration.name.spelling === name,
  )
  const first = matches.at(0)
  if (first === undefined) return Object.freeze({ _tag: 'Missing', spelling: name })
  return matches.length === 1
    ? Object.freeze({ _tag: 'Resolved', spelling: name, declaration: first })
    : Object.freeze({ _tag: 'Ambiguous', spelling: name, declarations: Object.freeze(matches) })
}

export const lookupStruct = (structs: ReadonlyArray<StructFact>, name: string): StructLookup => {
  const matches = structs.filter(
    (struct) => struct.name._tag === 'Present' && struct.name.spelling === name,
  )
  const first = matches.at(0)
  if (first === undefined) return Object.freeze({ _tag: 'Missing', spelling: name })
  return matches.length === 1
    ? Object.freeze({ _tag: 'Resolved', spelling: name, declaration: first })
    : Object.freeze({ _tag: 'Ambiguous', spelling: name, declarations: Object.freeze(matches) })
}

export const lookupField = (fields: ReadonlyArray<FieldFact>, name: string): FieldLookup => {
  const matches = fields.filter(
    (field) => field.name._tag === 'Present' && field.name.spelling === name,
  )
  const first = matches.at(0)
  if (first === undefined) return Object.freeze({ _tag: 'Missing', spelling: name })
  return matches.length === 1
    ? Object.freeze({ _tag: 'Resolved', spelling: name, field: first })
    : Object.freeze({ _tag: 'Ambiguous', spelling: name, fields: Object.freeze(matches) })
}

export const lookup = (self: Index, module: string, name: string): DeclarationLookup =>
  lookupDeclaration(
    self.modules.find((candidate) => candidate.module === module)?.declarations ??
      Object.freeze([]),
    name,
  )

export const member = (self: Index, module: string, name: string): MemberLookup =>
  lookupMember(
    self.modules.find((candidate) => candidate.module === module)?.members ?? Object.freeze([]),
    name,
  )

export const struct = (self: Index, module: string, name: string): StructLookup =>
  lookupStruct(
    self.modules.find((candidate) => candidate.module === module)?.structs ?? Object.freeze([]),
    name,
  )

/** Looks up one completed declaration by canonical identity. */
export const byCanonical = (self: Index, id: CanonicalId): MemberFact | undefined => {
  const result = member(self, id.module, id.name)
  return result._tag === 'Resolved' && result.declaration.canonical._tag === 'Canonical'
    ? result.declaration
    : undefined
}

/** Tests whether a value of this type can retain lexical storage through its fields. */
export const containsLexicalBorrow = (
  self: Index,
  type: Type.Type,
  seen: ReadonlySet<string> = new Set(),
): boolean => {
  if (Type.isSlice(type)) return true
  if (Type.isFixedArray(type)) return containsLexicalBorrow(self, type.element, seen)
  if (Type.isUnion(type))
    return type.members.some((member) => containsLexicalBorrow(self, member, seen))
  if (Type.isEffect(type))
    return (
      containsLexicalBorrow(self, type.success, seen) ||
      type.failures.some((failure) => containsLexicalBorrow(self, failure, seen))
    )
  if (!Type.isNominal(type)) return false
  const key = Type.key(type)
  if (seen.has(key)) return false
  const declaration = byCanonical(self, {
    _tag: 'CanonicalDeclarationId',
    module: type.module,
    name: type.name,
  })
  if (declaration?._tag !== 'StructDeclaration')
    return type.arguments.some((argument) => containsLexicalBorrow(self, argument, seen))
  const substitution =
    Type.substitution(
      declaration.typeParameters.map((parameter) => parameter.type),
      type.arguments,
    ) ?? new Map()
  const next = new Set(seen).add(key)
  return declaration.fields.some(
    (field) =>
      field.declaredType._tag === 'Resolved' &&
      containsLexicalBorrow(self, Type.substitute(field.declaredType.type, substitution), next),
  )
}
