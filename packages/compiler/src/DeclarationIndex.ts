import * as Option from 'effect/Option'
import * as Diagnostic from './Diagnostic.js'
import type * as ModuleClosure from './ModuleClosure.js'
import * as SourceFile from './SourceFile.js'
import type * as SyntaxFile from './SyntaxFile.js'
import * as SyntaxTree from './SyntaxTree.js'
import type * as Token from './Token.js'

/** The only semantic type recognized by the first analysis slice. */
export type SemanticType = 'I32' | 'Bool'

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

/** A declaration name supplied by syntax or explicitly unavailable after recovery. */
export type DeclaredName =
  | {
      readonly _tag: 'Present'
      readonly spelling: string
      readonly token: Token.Token
    }
  | {
      readonly _tag: 'Unavailable'
      readonly syntax: SyntaxTree.Element
    }

/** The resolved, unresolved, or syntax-unavailable declared return type. */
export type DeclaredTypeFact =
  | {
      readonly _tag: 'Resolved'
      readonly type: SemanticType
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Element
    }
  | {
      readonly _tag: 'Unresolved'
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Element
      readonly cause?: Diagnostic.Identity
    }
  | {
      readonly _tag: 'Unavailable'
      readonly syntax: SyntaxTree.Element
    }

/** The declared type fact attached to a function return. */
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

/** The closed result of looking up a parameter spelling within one function. */
export type ParameterLookup =
  | {
      readonly _tag: 'Resolved'
      readonly spelling: string
      readonly parameter: ParameterFact
    }
  | {
      readonly _tag: 'Missing'
      readonly spelling: string
    }
  | {
      readonly _tag: 'Ambiguous'
      readonly spelling: string
      readonly parameters: ReadonlyArray<ParameterFact>
    }

/** The closed result of looking up one declaration spelling. */
export type DeclarationLookup =
  | {
      readonly _tag: 'Resolved'
      readonly spelling: string
      readonly declaration: DeclarationFact
    }
  | {
      readonly _tag: 'Missing'
      readonly spelling: string
    }
  | {
      readonly _tag: 'Ambiguous'
      readonly spelling: string
      readonly declarations: ReadonlyArray<DeclarationFact>
    }

/** One module's collected headers with their header-level diagnostics. */
export interface ModuleHeaders {
  readonly _tag: 'ModuleHeaders'
  readonly module: string
  readonly declarations: ReadonlyArray<DeclarationFact>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

/** The immutable declaration index of one loaded closure. */
export interface Index {
  readonly _tag: 'DeclarationIndex'
  readonly modules: ReadonlyArray<ModuleHeaders>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

const spelling = (source: SourceFile.SourceFile, token: Token.Token): string =>
  Option.getOrThrowWith(
    SourceFile.spelling(source, token.span),
    () => new RangeError(`Header token span does not belong to source ${source.id}`),
  )

const childNode = (parent: SyntaxTree.Node, kind: SyntaxTree.NodeKind): SyntaxTree.Node => {
  const child = SyntaxTree.directNode(parent, kind)
  if (child === undefined) {
    throw new RangeError(`Header collection expected ${kind} below ${parent.kind}`)
  }
  return child
}

const presentName = (source: SourceFile.SourceFile, node: SyntaxTree.Node): DeclaredName => {
  const token = SyntaxTree.directToken(node, 'Identifier')
  return token === undefined
    ? Object.freeze({
        _tag: 'Unavailable',
        syntax: SyntaxTree.unavailableChild(node, 'Identifier'),
      })
    : Object.freeze({
        _tag: 'Present',
        spelling: spelling(source, token),
        token,
      })
}

interface DeclaredTypeResult {
  readonly fact: DeclaredTypeFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

/** Resolves one declared type token against the built-in bootstrap types. */
export const analyzeDeclaredType = (
  source: SourceFile.SourceFile,
  token: Token.Token | undefined,
  syntax: SyntaxTree.Element,
): DeclaredTypeResult => {
  if (token === undefined) {
    return Object.freeze({
      fact: Object.freeze({ _tag: 'Unavailable', syntax }),
      diagnostics: Object.freeze([]),
    })
  }

  const tokenSpelling = spelling(source, token)
  if (tokenSpelling === 'I32' || tokenSpelling === 'Bool') {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Resolved',
        type: tokenSpelling,
        spelling: tokenSpelling,
        token,
        syntax,
      }),
      diagnostics: Object.freeze([]),
    })
  }

  const diagnostic = Diagnostic.unknownType(tokenSpelling, token.span)
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Unresolved',
      spelling: tokenSpelling,
      token,
      syntax,
      cause: Diagnostic.identity(diagnostic),
    }),
    diagnostics: Object.freeze([diagnostic]),
  })
}

const analyzeReturnType = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
): DeclaredTypeResult =>
  analyzeDeclaredType(
    source,
    SyntaxTree.directToken(node, 'Identifier'),
    SyntaxTree.directToken(node, 'Identifier') === undefined
      ? SyntaxTree.unavailableChild(node, 'Identifier')
      : node,
  )

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

interface ParameterResult {
  readonly fact: ParameterFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

const analyzeParameter = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  functionId: DeclarationId,
  ordinal: number,
): ParameterResult => {
  const colonIndex = node.children.findIndex((element) => isSeparator(element, 'Colon'))
  const nameElements = colonIndex < 0 ? node.children : node.children.slice(0, colonIndex)
  const typeElements = colonIndex < 0 ? Object.freeze([]) : node.children.slice(colonIndex + 1)
  const nameToken = identifierToken(nameElements)
  const typeToken = identifierToken(typeElements)
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
  const declaredType = analyzeDeclaredType(
    source,
    typeToken,
    typeToken ?? SyntaxTree.unavailableElement(typeElements, node),
  )

  return Object.freeze({
    fact: Object.freeze({
      _tag: 'ParameterDeclaration',
      id: Object.freeze({
        _tag: 'ParameterId',
        function: functionId,
        ordinal,
      }),
      name,
      declaredType: declaredType.fact,
      syntax: node,
    }),
    diagnostics: declaredType.diagnostics,
  })
}

interface PresentParameterNameEntry {
  readonly spelling: string
  readonly token: Token.Token
  readonly parameter: ParameterFact
}

const presentParameterNameEntries = (
  parameters: ReadonlyArray<ParameterFact>,
): ReadonlyArray<PresentParameterNameEntry> =>
  Object.freeze(
    parameters.flatMap((parameter): ReadonlyArray<PresentParameterNameEntry> => {
      const name = parameter.name
      return name._tag === 'Present'
        ? [Object.freeze({ spelling: name.spelling, token: name.token, parameter })]
        : []
    }),
  )

const duplicateParameterDiagnostics = (
  parameters: ReadonlyArray<ParameterFact>,
): ReadonlyArray<Diagnostic.Diagnostic> => {
  const firstBySpelling = new Map<string, PresentParameterNameEntry>()
  let diagnostics: ReadonlyArray<Diagnostic.Diagnostic> = Object.freeze([])

  for (const entry of presentParameterNameEntries(parameters)) {
    const original = firstBySpelling.get(entry.spelling)
    if (original === undefined) {
      firstBySpelling.set(entry.spelling, entry)
    } else {
      diagnostics = Object.freeze([
        ...diagnostics,
        Diagnostic.duplicateParameterName(entry.spelling, original.token.span, entry.token.span),
      ])
    }
  }

  return diagnostics
}

/** Looks up every present parameter with the exact requested spelling in one collection. */
export const lookupParameter = (
  parameters: ReadonlyArray<ParameterFact>,
  spelling: string,
): ParameterLookup => {
  const matches = presentParameterNameEntries(parameters)
    .filter((entry) => entry.spelling === spelling)
    .map((entry) => entry.parameter)
  const first = matches.at(0)

  if (first === undefined) return Object.freeze({ _tag: 'Missing', spelling })
  if (matches.length === 1) {
    return Object.freeze({ _tag: 'Resolved', spelling, parameter: first })
  }
  return Object.freeze({
    _tag: 'Ambiguous',
    spelling,
    parameters: Object.freeze(matches),
  })
}

interface PresentNameEntry {
  readonly spelling: string
  readonly token: Token.Token
  readonly declaration: DeclarationFact
}

const presentNameEntries = (
  declarations: ReadonlyArray<DeclarationFact>,
): ReadonlyArray<PresentNameEntry> =>
  Object.freeze(
    declarations.flatMap((declaration): ReadonlyArray<PresentNameEntry> => {
      const name = declaration.name
      return name._tag === 'Present'
        ? [Object.freeze({ spelling: name.spelling, token: name.token, declaration })]
        : []
    }),
  )

/** Looks up every present declaration with the exact requested spelling in one collection. */
export const lookupDeclaration = (
  declarations: ReadonlyArray<DeclarationFact>,
  spelling: string,
): DeclarationLookup => {
  const matches = presentNameEntries(declarations)
    .filter((entry) => entry.spelling === spelling)
    .map((entry) => entry.declaration)
  const first = matches.at(0)

  if (first === undefined) return Object.freeze({ _tag: 'Missing', spelling })
  if (matches.length === 1) {
    return Object.freeze({ _tag: 'Resolved', spelling, declaration: first })
  }
  return Object.freeze({
    _tag: 'Ambiguous',
    spelling,
    declarations: Object.freeze(matches),
  })
}

const compareDiagnostics = (left: Diagnostic.Diagnostic, right: Diagnostic.Diagnostic): number =>
  left.span.start - right.span.start ||
  left.span.end - right.span.end ||
  (left.code < right.code ? -1 : left.code > right.code ? 1 : 0)

interface PendingHeader {
  readonly node: SyntaxTree.Node
  readonly id: DeclarationId
  readonly name: DeclaredName
  readonly parameters: ReadonlyArray<ParameterFact>
  readonly returnType: ReturnTypeFact
  readonly visibility: 'Public' | 'Private'
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

/** Collects one module's ordered declaration headers with canonical identity states. */
const collectModule = (syntax: SyntaxFile.SyntaxFile): ModuleHeaders => {
  const source = syntax.source
  const pending = SyntaxTree.directNodes(syntax.root, 'FunctionDeclaration').map(
    (node, ordinal): PendingHeader => {
      const returnTypeNode = childNode(node, 'ReturnType')
      const parameterListNode = childNode(node, 'ParameterList')
      const returnType = analyzeReturnType(source, returnTypeNode)
      const id: DeclarationId = Object.freeze({
        _tag: 'DeclarationId',
        sourceId: source.id,
        ordinal,
      })
      const analyzedParameters = SyntaxTree.directNodes(
        parameterListNode,
        'ParameterDeclaration',
      ).map((parameterNode, parameterOrdinal) =>
        analyzeParameter(source, parameterNode, id, parameterOrdinal),
      )
      const parameters = Object.freeze(analyzedParameters.map((result) => result.fact))
      return Object.freeze({
        node,
        id,
        name: presentName(source, node),
        parameters,
        returnType: returnType.fact,
        visibility:
          SyntaxTree.directToken(node, 'PubKeyword') === undefined
            ? ('Private' as const)
            : ('Public' as const),
        diagnostics: Object.freeze([
          ...analyzedParameters.flatMap((result) => result.diagnostics),
          ...duplicateParameterDiagnostics(parameters),
          ...returnType.diagnostics,
        ]),
      })
    },
  )

  const firstBySpelling = new Map<string, CanonicalId>()
  const duplicateDiagnostics: Array<Diagnostic.Diagnostic> = []
  const declarations = pending.map((header): DeclarationFact => {
    let canonical: CanonicalState
    if (header.name._tag !== 'Present') {
      canonical = Object.freeze({ _tag: 'Unidentified' as const })
    } else {
      const original = firstBySpelling.get(header.name.spelling)
      if (original === undefined) {
        const id: CanonicalId = Object.freeze({
          _tag: 'CanonicalDeclarationId',
          module: source.id,
          name: header.name.spelling,
        })
        firstBySpelling.set(header.name.spelling, id)
        canonical = Object.freeze({ _tag: 'Canonical' as const, id })
      } else {
        const diagnostic = Diagnostic.duplicateDeclarationName(
          header.name.spelling,
          Option.getOrThrowWith(
            firstTokenSpan(pending, header.name.spelling),
            () => new RangeError('Duplicate declaration lost its original span'),
          ),
          header.name.token.span,
        )
        duplicateDiagnostics.push(diagnostic)
        canonical = Object.freeze({
          _tag: 'Duplicate' as const,
          original,
          cause: Diagnostic.identity(diagnostic),
        })
      }
    }
    return Object.freeze({
      _tag: 'FunctionDeclaration',
      id: header.id,
      canonical,
      visibility: header.visibility,
      parameterCount: header.parameters.length,
      parameters: header.parameters,
      name: header.name,
      returnType: header.returnType,
      syntax: header.node,
    })
  })

  return Object.freeze({
    _tag: 'ModuleHeaders',
    module: source.id,
    declarations: Object.freeze(declarations),
    diagnostics: Object.freeze(
      [...pending.flatMap((header) => header.diagnostics), ...duplicateDiagnostics].sort(
        compareDiagnostics,
      ),
    ),
  })
}

const firstTokenSpan = (
  pending: ReadonlyArray<PendingHeader>,
  spelling: string,
): Option.Option<Token.Token['span']> => {
  for (const header of pending) {
    if (header.name._tag === 'Present' && header.name.spelling === spelling) {
      return Option.some(header.name.token.span)
    }
  }
  return Option.none()
}

/** Collects the declaration index of one loaded closure in canonical module order. */
export const collect = (closure: ModuleClosure.Closure): Index => {
  const modules = Object.freeze(closure.modules.map((module) => collectModule(module.syntax)))
  return Object.freeze({
    _tag: 'DeclarationIndex',
    modules,
    diagnostics: Diagnostic.merge(...modules.map((module) => module.diagnostics)),
  })
}

/** Looks up one declaration name within one module of the index. */
export const lookup = (self: Index, module: string, spelling: string): DeclarationLookup => {
  const headers = self.modules.find((candidate) => candidate.module === module)
  return lookupDeclaration(headers?.declarations ?? Object.freeze([]), spelling)
}
