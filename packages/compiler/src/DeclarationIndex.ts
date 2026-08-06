import * as Option from 'effect/Option'
import * as Diagnostic from './Diagnostic.js'
import type * as ModuleClosure from './ModuleClosure.js'
import * as SourceFile from './SourceFile.js'
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
      readonly _tag: 'Unavailable'
      readonly syntax: SyntaxTree.Element
      readonly cause?: Diagnostic.Identity
    }

export type ReturnTypeFact = DeclaredTypeFact

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
  readonly parameterCount: number
  readonly parameters: ReadonlyArray<ParameterFact>
  readonly name: DeclaredName
  readonly returnType: ReturnTypeFact
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

const declaredTypeNode = (parent: SyntaxTree.Node): SyntaxTree.Node => {
  const child = parent.children.find(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) &&
      (element.kind === 'TypePath' || element.kind === 'FixedArrayType'),
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
): TypeResolution => {
  if (syntax.kind === 'FixedArrayType') {
    const arrayName = SyntaxTree.directToken(syntax, 'Identifier')
    const elementSyntax = syntax.children.find(
      (element): element is SyntaxTree.Node =>
        SyntaxTree.isNode(element) &&
        (element.kind === 'TypePath' || element.kind === 'FixedArrayType'),
    )
    const lengthToken = SyntaxTree.directToken(syntax, 'DecimalInteger')
    if (arrayName === undefined || elementSyntax === undefined) {
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Unavailable',
          syntax: SyntaxTree.unavailableChild(syntax, 'Identifier'),
        }),
        diagnostics: Object.freeze([]),
      })
    }
    const element = analyzeDeclaredType(source, elementSyntax)
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
          token: arrayName,
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
        token: arrayName,
        syntax,
      }),
      diagnostics: Object.freeze(diagnostics),
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
  if (segments.length === 1 && (first.spelling === 'I32' || first.spelling === 'Bool')) {
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
  const type = analyzeDeclaredType(source, declaredTypeNode(node))
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
) => {
  const first = new Map<string, { readonly id: FieldId; readonly token: Token.Token }>()
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const fields = SyntaxTree.directNodes(node, 'StructField').map(
    (fieldNode, ordinal): FieldFact => {
      const id: FieldId = Object.freeze({ _tag: 'FieldId', struct: structId, ordinal })
      const name = presentName(source, fieldNode)
      const type = analyzeDeclaredType(source, declaredTypeNode(fieldNode))
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
    if (node.kind === 'StructDeclaration') {
      const collected = collectFields(source, node, id)
      diagnostics.push(...collected.diagnostics)
      return Object.freeze({
        _tag: 'StructDeclaration',
        id,
        canonical,
        visibility,
        name,
        fields: collected.fields,
        dependency: Object.freeze({ _tag: 'Available', types: Object.freeze([]) }),
        syntax: node,
      })
    }
    const parameterList = childNode(node, 'ParameterList')
    const parameters = SyntaxTree.directNodes(parameterList, 'ParameterDeclaration').map(
      (parameter, parameterOrdinal) => analyzeParameter(source, parameter, id, parameterOrdinal),
    )
    const returnType = analyzeDeclaredType(source, declaredTypeNode(childNode(node, 'ReturnType')))
    const facts = Object.freeze(parameters.map((parameter) => parameter.fact))
    diagnostics.push(
      ...parameters.flatMap((parameter) => parameter.diagnostics),
      ...duplicateParameterDiagnostics(facts),
      ...returnType.diagnostics,
    )
    return Object.freeze({
      _tag: 'FunctionDeclaration',
      id,
      canonical,
      visibility,
      parameterCount: facts.length,
      parameters: facts,
      name,
      returnType: returnType.fact,
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
): TypeResolution =>
  fact._tag === 'Unresolved'
    ? resolver(module, fact.path)
    : fact._tag !== 'FixedArray'
      ? Object.freeze({ fact, diagnostics: Object.freeze([]) })
      : (() => {
          const element = resolveDeclaredType(module, fact.element, resolver)
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
          const resolved = resolveDeclaredType(module.module, parameter.declaredType, resolver)
          diagnostics.push(...resolved.diagnostics)
          return Object.freeze({ ...parameter, declaredType: resolved.fact })
        })
        const result = resolveDeclaredType(module.module, member.returnType, resolver)
        diagnostics.push(...result.diagnostics)
        return Object.freeze({
          ...member,
          parameters: Object.freeze(parameters),
          returnType: result.fact,
        })
      }
      const fields = member.fields.map((field) => {
        const resolved = resolveDeclaredType(module.module, field.declaredType, resolver)
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
