import * as Option from 'effect/Option'
import * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import type * as ModuleClosure from './ModuleClosure.js'
import * as SourceFile from './SourceFile.js'
import * as SyntaxTree from './SyntaxTree.js'
import type * as Token from './Token.js'

export type Binding =
  | {
      readonly _tag: 'LocalDeclaration'
      readonly spelling: string
      readonly declaration: DeclarationIndex.DeclarationFact
    }
  | { readonly _tag: 'IntrinsicActor'; readonly spelling: 'I32' | 'Bool' }
  | {
      readonly _tag: 'ModuleNamespace'
      readonly spelling: string
      readonly module: string
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'ImportedMember'
      readonly spelling: string
      readonly sourceSpelling: string
      readonly module: string
      readonly declaration: DeclarationIndex.DeclarationFact
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'Unavailable'
      readonly spelling: string
      readonly syntax: SyntaxTree.Element
      readonly cause?: Diagnostic.Identity
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
      readonly declaration: DeclarationIndex.DeclarationFact
    }
  | { readonly _tag: 'Intrinsic'; readonly spelling: string; readonly actor: 'I32' | 'Bool' }
  | { readonly _tag: 'Namespace'; readonly spelling: string; readonly module: string }
  | { readonly _tag: 'Missing'; readonly spelling: string }
  | {
      readonly _tag: 'Inaccessible'
      readonly spelling: string
      readonly declaration: DeclarationIndex.DeclarationFact
      readonly cause: Diagnostic.Identity
    }
  | { readonly _tag: 'Conflict'; readonly spelling: string; readonly conflict: Conflict }
  | {
      readonly _tag: 'Unavailable'
      readonly spelling: string
      readonly cause?: Diagnostic.Identity
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
): { readonly spelling: string; readonly token: Token.Token } | undefined => {
  const alias = SyntaxTree.directNode(parent, 'ImportAlias')
  const token = alias === undefined ? undefined : SyntaxTree.directToken(alias, 'Identifier')
  return token === undefined ? undefined : Object.freeze({ spelling: text(source, token), token })
}
const canonicalDeclaration = (
  index: DeclarationIndex.Index,
  module: string,
  spelling: string,
): DeclarationIndex.DeclarationFact | undefined => {
  const result = DeclarationIndex.lookup(index, module, spelling)
  return result._tag === 'Resolved' && result.declaration.canonical._tag === 'Canonical'
    ? result.declaration
    : undefined
}

export const resolve = (
  closure: ModuleClosure.Closure,
  index: DeclarationIndex.Index,
): Resolution => {
  const scopes: Array<ModuleScope> = []
  for (const module of closure.modules) {
    const diagnostics: Array<Diagnostic.Diagnostic> = []
    const candidates: Array<Binding> = [
      Object.freeze({ _tag: 'IntrinsicActor', spelling: 'I32' }),
      Object.freeze({ _tag: 'IntrinsicActor', spelling: 'Bool' }),
    ]
    const headers = index.modules.find((value) => value.module === module.name)
    for (const declaration of headers?.declarations ?? [])
      if (declaration.canonical._tag === 'Canonical')
        candidates.push(
          Object.freeze({
            _tag: 'LocalDeclaration',
            spelling: declaration.canonical.id.name,
            declaration,
          }),
        )
    const seenTargets = new Set<string>()
    const imports: Array<ImportOutcome> = []
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
      if (seenTargets.has(target)) {
        const diagnostic = Diagnostic.duplicateImport(target, imported.path.span)
        diagnostics.push(diagnostic)
        imports.push(
          Object.freeze({
            _tag: 'Unavailable',
            import: imported,
            cause: Diagnostic.identity(diagnostic),
          }),
        )
        continue
      }
      seenTargets.add(target)
      const created: Array<Binding> = []
      const source = module.syntax.source
      const pathNames = identifiers(imported.path)
      const defaultName = pathNames.at(-1)
      const aliasSyntax = SyntaxTree.directNode(imported.syntax, 'ImportAlias')
      const explicitAlias = aliasName(source, imported.syntax)
      const list = SyntaxTree.directNode(imported.syntax, 'ImportMemberList')
      if (aliasSyntax !== undefined && explicitAlias === undefined) {
        imports.push(Object.freeze({ _tag: 'Unavailable', import: imported }))
        continue
      }
      if (list === undefined || explicitAlias !== undefined) {
        const local =
          explicitAlias?.spelling ??
          (defaultName === undefined ? undefined : text(source, defaultName))
        if (
          explicitAlias !== undefined &&
          defaultName !== undefined &&
          explicitAlias.spelling === text(source, defaultName)
        ) {
          const diagnostic = Diagnostic.redundantAlias(
            explicitAlias.spelling,
            explicitAlias.token.span,
          )
          diagnostics.push(diagnostic)
          created.push(
            Object.freeze({
              _tag: 'Unavailable',
              spelling: explicitAlias.spelling,
              syntax: aliasSyntax ?? imported.syntax,
              cause: Diagnostic.identity(diagnostic),
            }),
          )
        } else if (local !== undefined)
          created.push(
            Object.freeze({
              _tag: 'ModuleNamespace',
              spelling: local,
              module: target,
              syntax: imported.syntax,
            }),
          )
      }
      for (const member of list === undefined ? [] : SyntaxTree.directNodes(list, 'ImportMember')) {
        const sourceToken = identifiers(member).at(0)
        if (sourceToken === undefined || !SyntaxTree.isAvailableSyntax(member)) continue
        const sourceName = text(source, sourceToken)
        const alias = aliasName(source, member)
        if (alias !== undefined && alias.spelling === sourceName) {
          const diagnostic = Diagnostic.redundantAlias(alias.spelling, alias.token.span)
          diagnostics.push(diagnostic)
          created.push(
            Object.freeze({
              _tag: 'Unavailable',
              spelling: alias.spelling,
              syntax: member,
              cause: Diagnostic.identity(diagnostic),
            }),
          )
          continue
        }
        const declaration = canonicalDeclaration(index, target, sourceName)
        if (declaration === undefined) {
          const diagnostic = Diagnostic.unknownImportedMember(target, sourceName, sourceToken.span)
          diagnostics.push(diagnostic)
          created.push(
            Object.freeze({
              _tag: 'Unavailable',
              spelling: alias?.spelling ?? sourceName,
              syntax: member,
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
              cause: Diagnostic.identity(diagnostic),
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
            declaration,
            syntax: member,
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
      else group.push(binding)
    }
    const conflicts: Array<Conflict> = []
    for (const [spelling, bindings] of grouped)
      if (bindings.length > 1) {
        const last = bindings.at(-1)
        const span =
          last?._tag === 'LocalDeclaration' && last.declaration.name._tag === 'Present'
            ? last.declaration.name.token.span
            : last?._tag === 'ModuleNamespace' ||
                last?._tag === 'ImportedMember' ||
                last?._tag === 'Unavailable'
              ? last.syntax.span
              : module.syntax.root.span
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
export const lookup = (scope: ModuleScope, spelling: string): Lookup => {
  const conflict = scope.conflicts.find((value) => value.spelling === spelling)
  if (conflict !== undefined) return Object.freeze({ _tag: 'Conflict', spelling, conflict })
  const binding = scope.bindings.find((value) => value.spelling === spelling)
  if (binding === undefined) return Object.freeze({ _tag: 'Missing', spelling })
  if (binding._tag === 'IntrinsicActor')
    return Object.freeze({ _tag: 'Intrinsic', spelling, actor: binding.spelling })
  if (binding._tag === 'Unavailable')
    return Object.freeze({
      _tag: 'Unavailable',
      spelling,
      ...(binding.cause === undefined ? {} : { cause: binding.cause }),
    })
  if (binding._tag === 'ModuleNamespace')
    return Object.freeze({ _tag: 'Namespace', spelling, module: binding.module })
  return Object.freeze({ _tag: 'Resolved', spelling, declaration: binding.declaration })
}
export const lookupQualified = (
  scope: ModuleScope,
  index: DeclarationIndex.Index,
  namespace: string,
  member: string,
  token: Token.Token,
): Lookup => {
  const qualifier = lookup(scope, namespace)
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
