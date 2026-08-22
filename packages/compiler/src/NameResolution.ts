import * as Option from 'effect/Option'
import * as DeclarationCollection from './DeclarationCollection.js'
import * as DeclarationCompletion from './DeclarationCompletion.js'
import * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as ImportPath from './ImportPath.js'
import * as Intrinsic from './Intrinsic.js'
import type * as ModuleClosure from './ModuleClosure.js'
import * as ResolutionSeams from './ResolutionSeams.js'
import * as SourceFile from './SourceFile.js'
import * as SyntaxTree from './SyntaxTree.js'
import type * as Token from './Token.js'
import * as Type from './Type.js'

export type IntrinsicActor = Intrinsic.Actor['spelling']

export type Binding =
  | {
      readonly _tag: 'LocalDeclaration'
      readonly spelling: string
      readonly declaration: DeclarationFacts.CanonicalId
    }
  | { readonly _tag: 'IntrinsicActor'; readonly spelling: IntrinsicActor }
  | {
      readonly _tag: 'ModuleNamespace'
      readonly spelling: string
      readonly module: string
      readonly syntax: SyntaxTree.Node
      readonly token: Token.Token
    }
  | {
      readonly _tag: 'ImportedMember'
      readonly spelling: string
      readonly sourceSpelling: string
      readonly module: string
      readonly declaration: DeclarationFacts.CanonicalId
      readonly syntax: SyntaxTree.Node
      readonly sourceToken: Token.Token
      readonly localToken: Token.Token
    }
  | {
      readonly _tag: 'Unavailable'
      readonly spelling: string
      readonly syntax: SyntaxTree.Element
      readonly tokens: ReadonlyArray<Token.Token>
      readonly cause?: Diagnostic.Identity
      readonly declaration?: DeclarationFacts.CanonicalId
    }

export type ImportOutcome =
  | {
      readonly _tag: 'Available'
      readonly import: ModuleClosure.ImportFact
      readonly bindings: ReadonlyArray<Binding>
    }
  | {
      readonly _tag: 'Unavailable'
      readonly import: ModuleClosure.ImportFact
      readonly cause?: Diagnostic.Identity
    }

export interface Conflict {
  readonly _tag: 'BindingConflict'
  readonly spelling: string
  readonly bindings: ReadonlyArray<Binding>
  readonly cause: Diagnostic.Identity
}
export interface ModuleScope {
  readonly _tag: 'ModuleScope'
  readonly module: string
  readonly bindings: ReadonlyArray<Binding>
  readonly imports: ReadonlyArray<ImportOutcome>
  readonly conflicts: ReadonlyArray<Conflict>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}
export interface Resolution {
  readonly _tag: 'NameResolution'
  readonly modules: ReadonlyArray<ModuleScope>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}
export type Lookup =
  | {
      readonly _tag: 'Resolved'
      readonly spelling: string
      readonly declaration: DeclarationFacts.MemberFact
    }
  | { readonly _tag: 'Intrinsic'; readonly spelling: string; readonly actor: IntrinsicActor }
  | { readonly _tag: 'Namespace'; readonly spelling: string; readonly module: string }
  | { readonly _tag: 'Missing'; readonly spelling: string }
  | {
      readonly _tag: 'Inaccessible'
      readonly spelling: string
      readonly declaration: DeclarationFacts.MemberFact
      readonly cause: Diagnostic.Identity
    }
  | { readonly _tag: 'Conflict'; readonly spelling: string; readonly conflict: Conflict }
  | {
      readonly _tag: 'Unavailable'
      readonly spelling: string
      readonly cause?: Diagnostic.Identity
      readonly declaration?: DeclarationFacts.MemberFact
    }

const text = (source: SourceFile.SourceFile, token: Token.Token): string =>
  Option.getOrThrowWith(
    SourceFile.spelling(source, token.span),
    () => new RangeError('Import token belongs to another source'),
  )
const identifiers = (node: SyntaxTree.Node): ReadonlyArray<Token.Token> =>
  node.children.filter(
    (element): element is Token.Token =>
      SyntaxTree.isToken(element) && element.kind === 'Identifier',
  )
const aliasName = (
  source: SourceFile.SourceFile,
  parent: SyntaxTree.Node,
):
  | {
      readonly spelling: string
      readonly token: Token.Token
    }
  | undefined => {
  const alias = SyntaxTree.directNode(parent, 'ImportAlias')
  const token = alias === undefined ? undefined : SyntaxTree.directToken(alias, 'Identifier')
  return alias === undefined || token === undefined
    ? undefined
    : Object.freeze({
        spelling: text(source, token),
        token,
      })
}
type CanonicalMember = DeclarationFacts.MemberFact & {
  readonly canonical: Extract<DeclarationFacts.CanonicalState, { readonly _tag: 'Canonical' }>
}
const isCanonicalMember = (
  declaration: DeclarationFacts.MemberFact,
): declaration is CanonicalMember => declaration.canonical._tag === 'Canonical'
const canonicalDeclaration = (
  index: DeclarationIndex.Index,
  module: string,
  spelling: string,
): CanonicalMember | undefined => {
  const result = DeclarationFacts.member(index, module, spelling)
  return result._tag === 'Resolved' && isCanonicalMember(result.declaration)
    ? result.declaration
    : undefined
}

const bindingTarget = (binding: Exclude<Binding, { readonly _tag: 'Unavailable' }>): string => {
  switch (binding._tag) {
    case 'IntrinsicActor':
      return `intrinsic:${binding.spelling}`
    case 'ModuleNamespace':
      return `module:${binding.module}`
    case 'LocalDeclaration':
    case 'ImportedMember':
      return `declaration:${binding.declaration.module}.${binding.declaration.name}`
  }
}

export const resolve = (
  closure: ModuleClosure.Facts,
  index: DeclarationIndex.Index,
): Resolution => {
  const scopes: Array<ModuleScope> = []
  for (const module of closure.modules) {
    const diagnostics: Array<Diagnostic.Diagnostic> = []
    const candidates: Array<Binding> = Intrinsic.all().map((intrinsic) =>
      Object.freeze({ _tag: 'IntrinsicActor', spelling: intrinsic.spelling }),
    )
    const headers = index.modules.find((value) => value.module === module.name)
    for (const declaration of headers?.members ?? [])
      if (declaration.canonical._tag === 'Canonical')
        candidates.push(
          Object.freeze({
            _tag: 'LocalDeclaration',
            spelling: declaration.canonical.id.name,
            declaration: declaration.canonical.id,
          }),
        )
    const imports: Array<ImportOutcome> = []
    const source = module.syntax.source
    for (const imported of module.imports) {
      if (imported.target._tag !== 'Resolved') {
        imports.push(
          Object.freeze({
            _tag: 'Unavailable',
            import: imported,
            ...('cause' in imported.target ? { cause: imported.target.cause } : {}),
          }),
        )
        continue
      }
      const target = imported.target.module
      const created: Array<Binding> = []
      const pathNames = ImportPath.segments(imported.path)
      const defaultName = pathNames.at(-1)
      const aliasSyntax = SyntaxTree.directNode(imported.syntax, 'ImportAlias')
      const explicitAlias = aliasName(source, imported.syntax)
      const list = SyntaxTree.directNode(imported.syntax, 'ImportMemberList')
      if (aliasSyntax !== undefined && explicitAlias === undefined) {
        imports.push(Object.freeze({ _tag: 'Unavailable', import: imported }))
        continue
      }
      if (list === undefined || explicitAlias !== undefined) {
        const implicitName =
          defaultName === undefined || ImportPath.isReservedSegment(defaultName)
            ? undefined
            : defaultName
        const local =
          explicitAlias?.spelling ??
          (implicitName === undefined ? undefined : text(source, implicitName))
        const localToken = explicitAlias?.token ?? implicitName
        if (local !== undefined && localToken !== undefined)
          created.push(
            Object.freeze({
              _tag: 'ModuleNamespace',
              spelling: local,
              module: target,
              syntax: imported.syntax,
              token: localToken,
            }),
          )
      }
      for (const member of list === undefined ? [] : SyntaxTree.directNodes(list, 'ImportMember')) {
        const sourceToken = identifiers(member).at(0)
        if (sourceToken === undefined || !SyntaxTree.isAvailableSyntax(member)) continue
        const sourceName = text(source, sourceToken)
        const alias = aliasName(source, member)
        const declaration = canonicalDeclaration(index, target, sourceName)
        if (declaration === undefined) {
          const diagnostic = Diagnostic.unknownImportedMember(target, sourceName, sourceToken.span)
          diagnostics.push(diagnostic)
          created.push(
            Object.freeze({
              _tag: 'Unavailable',
              spelling: alias?.spelling ?? sourceName,
              syntax: member,
              tokens: Object.freeze([sourceToken, ...(alias === undefined ? [] : [alias.token])]),
              cause: Diagnostic.identity(diagnostic),
            }),
          )
          continue
        }
        if (declaration.visibility === 'Private') {
          const diagnostic = Diagnostic.inaccessibleImportedMember(
            target,
            sourceName,
            sourceToken.span,
          )
          diagnostics.push(diagnostic)
          created.push(
            Object.freeze({
              _tag: 'Unavailable',
              spelling: alias?.spelling ?? sourceName,
              syntax: member,
              tokens: Object.freeze([sourceToken, ...(alias === undefined ? [] : [alias.token])]),
              cause: Diagnostic.identity(diagnostic),
              declaration: declaration.canonical.id,
            }),
          )
          continue
        }
        created.push(
          Object.freeze({
            _tag: 'ImportedMember',
            spelling: alias?.spelling ?? sourceName,
            sourceSpelling: sourceName,
            module: target,
            declaration: declaration.canonical.id,
            syntax: member,
            sourceToken,
            localToken: alias?.token ?? sourceToken,
          }),
        )
      }
      candidates.push(...created)
      imports.push(
        Object.freeze({ _tag: 'Available', import: imported, bindings: Object.freeze(created) }),
      )
    }
    const grouped = new Map<string, Array<Binding>>()
    for (const binding of candidates) {
      if (binding._tag === 'Unavailable') continue
      const group = grouped.get(binding.spelling)
      if (group === undefined) grouped.set(binding.spelling, [binding])
      else if (
        !group.some(
          (candidate) =>
            candidate._tag !== 'Unavailable' && bindingTarget(candidate) === bindingTarget(binding),
        )
      )
        group.push(binding)
    }
    const conflicts: Array<Conflict> = []
    for (const [spelling, bindings] of grouped)
      if (bindings.length > 1) {
        const last = bindings.at(-1)
        let span = module.syntax.root.span
        if (last?._tag === 'LocalDeclaration') {
          const declaration = DeclarationFacts.byCanonical(index, last.declaration)
          if (declaration?.name._tag === 'Present') span = declaration.name.token.span
        } else if (
          last?._tag === 'ModuleNamespace' ||
          last?._tag === 'ImportedMember' ||
          last?._tag === 'Unavailable'
        ) {
          span = last.syntax.span
        }
        const diagnostic = Diagnostic.bindingConflict(spelling, span)
        diagnostics.push(diagnostic)
        conflicts.push(
          Object.freeze({
            _tag: 'BindingConflict',
            spelling,
            bindings: Object.freeze(bindings),
            cause: Diagnostic.identity(diagnostic),
          }),
        )
      }
    scopes.push(
      Object.freeze({
        _tag: 'ModuleScope',
        module: module.name,
        bindings: Object.freeze(candidates),
        imports: Object.freeze(imports),
        conflicts: Object.freeze(conflicts),
        diagnostics: Diagnostic.merge(diagnostics),
      }),
    )
  }
  return Object.freeze({
    _tag: 'NameResolution',
    modules: Object.freeze(scopes),
    diagnostics: Diagnostic.merge(...scopes.map((scope) => scope.diagnostics)),
  })
}

export const scopeOf = (self: Resolution, module: string): ModuleScope | undefined =>
  self.modules.find((scope) => scope.module === module)
export const lookup = (
  scope: ModuleScope,
  index: DeclarationIndex.Index,
  spelling: string,
): Lookup => {
  const conflict = scope.conflicts.find((value) => value.spelling === spelling)
  if (conflict !== undefined) return Object.freeze({ _tag: 'Conflict', spelling, conflict })
  const binding = scope.bindings.find((value) => value.spelling === spelling)
  if (binding === undefined) return Object.freeze({ _tag: 'Missing', spelling })
  if (binding._tag === 'IntrinsicActor')
    return Object.freeze({ _tag: 'Intrinsic', spelling, actor: binding.spelling })
  if (binding._tag === 'Unavailable') {
    const declaration =
      binding.declaration === undefined
        ? undefined
        : DeclarationFacts.byCanonical(index, binding.declaration)
    return Object.freeze({
      _tag: 'Unavailable',
      spelling,
      ...(binding.cause === undefined ? {} : { cause: binding.cause }),
      ...(declaration === undefined ? {} : { declaration }),
    })
  }
  if (binding._tag === 'ModuleNamespace')
    return Object.freeze({ _tag: 'Namespace', spelling, module: binding.module })
  const declaration = DeclarationFacts.byCanonical(index, binding.declaration)
  return declaration === undefined
    ? Object.freeze({ _tag: 'Unavailable', spelling })
    : Object.freeze({ _tag: 'Resolved', spelling, declaration })
}
export const lookupQualified = (
  scope: ModuleScope,
  index: DeclarationIndex.Index,
  namespace: string,
  member: string,
  token: Token.Token,
): Lookup => {
  const qualifier = lookup(scope, index, namespace)
  if (
    qualifier._tag === 'Intrinsic' ||
    qualifier._tag === 'Conflict' ||
    qualifier._tag === 'Missing'
  )
    return qualifier
  if (qualifier._tag !== 'Namespace')
    return Object.freeze({ _tag: 'Missing', spelling: `${namespace}.${member}` })
  const declaration = canonicalDeclaration(index, qualifier.module, member)
  if (declaration === undefined) {
    const diagnostic = Diagnostic.unknownImportedMember(qualifier.module, member, token.span)
    return Object.freeze({
      _tag: 'Unavailable',
      spelling: member,
      cause: Diagnostic.identity(diagnostic),
    })
  }
  if (declaration.visibility === 'Private') {
    const diagnostic = Diagnostic.inaccessibleImportedMember(qualifier.module, member, token.span)
    return Object.freeze({
      _tag: 'Inaccessible',
      spelling: member,
      declaration,
      cause: Diagnostic.identity(diagnostic),
    })
  }
  return Object.freeze({ _tag: 'Resolved', spelling: member, declaration })
}

const unresolved = (
  path: DeclarationFacts.TypePathFact,
  diagnostic: Diagnostic.Diagnostic,
  candidate?: Type.Nominal,
): DeclarationFacts.TypeResolution => {
  const token = path.segments.at(0)?.token
  if (token === undefined) {
    return Object.freeze({
      fact: Object.freeze({ _tag: 'Unavailable', syntax: path.syntax }),
      diagnostics: Object.freeze([diagnostic]),
    })
  }
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Unresolved',
      spelling: path.spelling,
      token,
      syntax: path.syntax,
      path,
      cause: Diagnostic.identity(diagnostic),
      ...(candidate === undefined ? {} : { candidate }),
    }),
    diagnostics: Object.freeze([diagnostic]),
  })
}

const unavailable = (
  path: DeclarationFacts.TypePathFact,
  cause?: Diagnostic.Identity,
  candidate?: Type.Nominal,
): DeclarationFacts.TypeResolution => {
  const token = path.segments.at(0)?.token
  if (token === undefined) {
    return Object.freeze({
      fact: Object.freeze({ _tag: 'Unavailable', syntax: path.syntax }),
      diagnostics: Object.freeze([]),
    })
  }
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Unresolved',
      spelling: path.spelling,
      token,
      syntax: path.syntax,
      path,
      ...(cause === undefined ? {} : { cause }),
      ...(candidate === undefined ? {} : { candidate }),
    }),
    diagnostics: Object.freeze([]),
  })
}

const resolvedType = (
  path: DeclarationFacts.TypePathFact,
  type: DeclarationFacts.SemanticType,
): DeclarationFacts.TypeResolution => {
  const token = path.segments.at(0)?.token
  return token === undefined
    ? Object.freeze({
        fact: Object.freeze({ _tag: 'Unavailable', syntax: path.syntax }),
        diagnostics: Object.freeze([]),
      })
    : Object.freeze({
        fact: Object.freeze({
          _tag: 'Resolved',
          type,
          spelling: path.spelling,
          token,
          syntax: path.syntax,
          path,
        }),
        diagnostics: Object.freeze([]),
      })
}

const nominalOf = (declaration: DeclarationFacts.MemberFact): Type.Nominal | undefined =>
  (declaration._tag === 'StructDeclaration' ||
    declaration._tag === 'ServiceDeclaration' ||
    declaration._tag === 'InterfaceDeclaration') &&
  declaration.canonical._tag === 'Canonical'
    ? Type.nominal(declaration.canonical.id.module, declaration.canonical.id.name)
    : undefined

const typeUseSpan = (path: DeclarationFacts.TypePathFact): Token.Token['span'] =>
  path.segments.at(-1)?.token.span ?? path.syntax.span

/** Resolves one retained declaration type path through an immutable module scope. */
export const resolveType = (
  resolution: Resolution,
  index: DeclarationIndex.Index,
  module: string,
  path: DeclarationFacts.TypePathFact,
): DeclarationFacts.TypeResolution => {
  const scope = scopeOf(resolution, module)
  const first = path.segments.at(0)
  const second = path.segments.at(1)
  if (scope === undefined || first === undefined) {
    return Object.freeze({
      fact: Object.freeze({ _tag: 'Unavailable', syntax: path.syntax }),
      diagnostics: Object.freeze([]),
    })
  }
  const result =
    second === undefined
      ? lookup(scope, index, first.spelling)
      : lookupQualified(scope, index, first.spelling, second.spelling, second.token)
  if (result._tag === 'Intrinsic') {
    if (
      result.actor === 'Intrinsic' &&
      second?.spelling === 'SharedCore' &&
      path.segments.length === 2
    ) {
      const sharedCore = Type.intrinsicNominals.get('Intrinsic.SharedCore')
      if (sharedCore !== undefined) return resolvedType(path, sharedCore)
    }
    if (Type.isBuiltin(result.actor)) return resolvedType(path, result.actor)
    return unresolved(path, Diagnostic.expectedType(path.spelling, typeUseSpan(path)))
  }
  if (result._tag === 'Resolved') {
    const nominal = nominalOf(result.declaration)
    if (nominal !== undefined) return resolvedType(path, nominal)
    return unresolved(path, Diagnostic.expectedType(path.spelling, typeUseSpan(path)))
  }
  if (result._tag === 'Inaccessible') {
    const nominal = nominalOf(result.declaration)
    const diagnostic = Diagnostic.inaccessibleImportedMember(
      result.declaration.canonical._tag === 'Canonical'
        ? result.declaration.canonical.id.module
        : module,
      result.spelling,
      typeUseSpan(path),
    )
    return unresolved(path, diagnostic, nominal)
  }
  if (result._tag === 'Conflict') return unavailable(path, result.conflict.cause)
  if (result._tag === 'Unavailable')
    return unavailable(
      path,
      result.cause,
      result.declaration === undefined ? undefined : nominalOf(result.declaration),
    )
  if (result._tag === 'Namespace') {
    return unresolved(path, Diagnostic.expectedType(path.spelling, typeUseSpan(path)))
  }
  return unresolved(path, Diagnostic.unknownType(path.spelling, typeUseSpan(path)))
}

/** Resolves one retained item path through the same import scope and visibility gate as values. */
export const resolveItem = (
  resolution: Resolution,
  index: DeclarationIndex.Index,
  module: string,
  path: DeclarationFacts.TypePathFact,
): DeclarationFacts.ItemResolution => {
  const scope = scopeOf(resolution, module)
  const first = path.segments.at(0)
  const second = path.segments.at(1)
  if (scope === undefined || first === undefined || path.segments.length > 2)
    return Object.freeze({ _tag: 'Missing' })
  if (second === undefined) {
    const local = DeclarationFacts.lookupDeclaration(
      index.modules.find((candidate) => candidate.module === module)?.declarations ?? [],
      first.spelling,
    )
    if (local._tag === 'Ambiguous')
      return Object.freeze({ _tag: 'Ambiguous', count: local.declarations.length })
  }
  const result =
    second === undefined
      ? lookup(scope, index, first.spelling)
      : lookupQualified(scope, index, first.spelling, second.spelling, second.token)
  if (result._tag === 'Resolved')
    return Object.freeze({ _tag: 'Resolved', declaration: result.declaration })
  if (result._tag === 'Inaccessible')
    return Object.freeze({
      _tag: 'Inaccessible',
      declaration: result.declaration,
      cause: result.cause,
    })
  if (result._tag === 'Conflict')
    return Object.freeze({
      _tag: 'Ambiguous',
      count: result.conflict.bindings.length,
      cause: result.conflict.cause,
    })
  if (result._tag === 'Unavailable')
    return Object.freeze({
      _tag: 'Unavailable',
      ...(result.declaration === undefined ? {} : { declaration: result.declaration }),
      ...(result.cause === undefined ? {} : { cause: result.cause }),
    })
  return Object.freeze({ _tag: 'Missing' })
}

/** Runs identity collection, scope construction, and declared-type completion in phase order. */
export const analyze = (
  closure: ModuleClosure.Facts,
): { readonly index: DeclarationIndex.Index; readonly resolution: Resolution } => {
  const collected = DeclarationCollection.collect(closure)
  const preliminary = resolve(closure, collected)
  const resolvers = ResolutionSeams.make(
    (module: string, path: DeclarationFacts.TypePathFact) =>
      resolveType(preliminary, collected, module, path),
    (module: string, path: DeclarationFacts.TypePathFact) =>
      resolveItem(preliminary, collected, module, path),
  )
  const index = DeclarationCompletion.complete(collected, resolvers)
  return Object.freeze({ index, resolution: resolve(closure, index) })
}
