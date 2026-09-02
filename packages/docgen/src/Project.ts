import * as Analysis from '@silklang/compiler/Analysis'
import type * as DeclarationFacts from '@silklang/compiler/DeclarationFacts'
import * as Presentation from '@silklang/compiler/Presentation'
import * as ProjectAnalysis from '@silklang/compiler/ProjectAnalysis'
import type * as SourceFile from '@silklang/compiler/SourceFile'
import type * as SyntaxTree from '@silklang/compiler/SyntaxTree'
import * as Document from './Document.js'

export type ItemKind =
  | 'Function'
  | 'Struct'
  | 'Enum'
  | 'EnumMember'
  | 'Union'
  | 'UnionVariant'
  | 'Service'
  | 'Interface'
  | 'Constant'
  | 'Role'
  | 'Alias'
  | 'Parameter'
  | 'TypeParameter'
  | 'Field'
  | 'Implementation'
  | 'Operation'
  | 'Method'
  | 'AssociatedFunction'

export interface Signature {
  readonly text: string
}

export interface Item {
  readonly id: string
  readonly kind: ItemKind
  readonly name: string
  readonly visibility: 'Public' | 'Private' | 'Inherited'
  readonly signature: Signature
  readonly source: Document.SourceRange
  readonly documentation?: Document.Document
  readonly children: ReadonlyArray<Item>
}

export interface Module {
  readonly name: string
  readonly sourceId: string
  readonly documentation?: Document.Document
  readonly items: ReadonlyArray<Item>
}

/** The experimental, formatter-neutral project documentation model. */
export interface Project {
  readonly schema: 'silk-documentation'
  readonly experimental: true
  readonly modules: ReadonlyArray<Module>
}

export interface Options {
  readonly includePrivate?: boolean
}

const nameOf = (name: DeclarationFacts.DeclaredName, fallback: string): string =>
  name._tag === 'Present' ? name.spelling : fallback

const declarationId = (module: string, member: DeclarationFacts.MemberFact): string =>
  member.canonical._tag === 'Canonical'
    ? `${member.canonical.id.module}::${member.canonical.id.name}`
    : `${module}::#${member.id.ordinal}`

const linkTargetKind = (member: DeclarationFacts.MemberFact): Document.LinkTarget['kind'] => {
  switch (member._tag) {
    case 'FunctionDeclaration':
      return 'Function'
    case 'StructDeclaration':
      return 'Struct'
    case 'EnumDeclaration':
      return 'Enum'
    case 'UnionDeclaration':
      return 'Union'
    case 'ServiceDeclaration':
      return 'Service'
    case 'InterfaceDeclaration':
      return 'Interface'
    case 'RoleDeclaration':
      return 'Role'
    case 'AliasDeclaration':
      return 'Alias'
    default:
      return 'Constant'
  }
}

const isAssociated = (
  member: DeclarationFacts.MemberFact,
): member is DeclarationFacts.DeclarationFact & {
  readonly associatedMember: DeclarationFacts.AssociatedMemberFact
} => member._tag === 'FunctionDeclaration' && member.associatedMember !== undefined

const sameId = (left: DeclarationFacts.CanonicalId, right: DeclarationFacts.CanonicalId): boolean =>
  left.module === right.module && left.name === right.name

/** The owner whose associated members a doc block attached to this declaration links first. */
const ownerOf = (member: DeclarationFacts.MemberFact): DeclarationFacts.CanonicalId | undefined => {
  if (isAssociated(member)) return member.associatedMember.owner
  return member.canonical._tag === 'Canonical' ? member.canonical.id : undefined
}

const itemKind = (member: DeclarationFacts.MemberFact): ItemKind => {
  switch (member._tag) {
    case 'FunctionDeclaration':
      if (member.associatedMember === undefined) return 'Function'
      return member.associatedMember.receiver ? 'Method' : 'AssociatedFunction'
    case 'StructDeclaration':
      return 'Struct'
    case 'EnumDeclaration':
      return 'Enum'
    case 'UnionDeclaration':
      return 'Union'
    case 'ServiceDeclaration':
      return 'Service'
    case 'InterfaceDeclaration':
      return 'Interface'
    case 'RoleDeclaration':
      return 'Role'
    case 'AliasDeclaration':
      return 'Alias'
    default:
      return 'Constant'
  }
}

const rangeOf = (node: SyntaxTree.Node): Document.SourceRange =>
  Object.freeze({
    sourceId: node.span.sourceId,
    start: node.span.start,
    end: node.span.end,
  })

const parsedDocumentation = (
  snapshot: Analysis.FrontendSnapshot,
  module: string,
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
): Document.Document | undefined => {
  const raw = Analysis.documentationOfSyntax(snapshot, module, node)
  return raw === undefined ? undefined : Document.parse(source, raw)
}

/**
 * A `[`member`]` link inside the owner's module reaches an inherent member, which the unqualified
 * root lookup cannot see. The doc block's own owner wins; otherwise the member must be unique
 * across the module's owners.
 */
const associatedTarget = (
  snapshot: Analysis.FrontendSnapshot,
  module: string,
  spelling: string,
  owner: DeclarationFacts.CanonicalId | undefined,
): Document.LinkTarget | undefined => {
  const headers = Analysis.declarationIndex(snapshot).modules.find(
    (candidate) => candidate.module === module,
  )
  const candidates = (headers?.members ?? []).filter(
    (member) =>
      isAssociated(member) &&
      member.associatedMember.name === spelling &&
      member.canonical._tag === 'Canonical',
  )
  const preferred =
    owner === undefined
      ? undefined
      : candidates.find(
          (member) =>
            isAssociated(member) &&
            member.associatedMember.owner !== undefined &&
            sameId(member.associatedMember.owner, owner),
        )
  const member = preferred ?? (candidates.length === 1 ? candidates.at(0) : undefined)
  if (member === undefined || member.canonical._tag !== 'Canonical') return undefined
  return Object.freeze({
    id: declarationId(module, member),
    module: member.canonical.id.module,
    name: member.canonical.id.name,
    kind: 'Function',
  })
}

const targetOf = (
  snapshot: Analysis.FrontendSnapshot,
  module: string,
  spelling: string,
  owner: DeclarationFacts.CanonicalId | undefined,
): Document.LinkTarget | undefined => {
  const associated = associatedTarget(snapshot, module, spelling, owner)
  if (associated !== undefined) return associated
  const lookup = Analysis.lookupName(snapshot, module, spelling)
  if (lookup._tag === 'Resolved') {
    const targetModule =
      lookup.declaration.canonical._tag === 'Canonical'
        ? lookup.declaration.canonical.id.module
        : lookup.declaration.id.sourceId
    const targetName = nameOf(lookup.declaration.name, spelling)
    return Object.freeze({
      id: declarationId(targetModule, lookup.declaration),
      module: targetModule,
      name: targetName,
      kind: linkTargetKind(lookup.declaration),
    })
  }

  const index = Analysis.declarationIndex(snapshot)
  const parentIsVisible = (parent: DeclarationFacts.MemberFact): boolean => {
    if (parent.name._tag !== 'Present') return false
    const visible = Analysis.lookupName(snapshot, module, parent.name.spelling)
    return visible._tag === 'Resolved' && visible.declaration === parent
  }
  const enumMembers = index.modules.flatMap((headers) =>
    headers.enums.flatMap((enum_) =>
      parentIsVisible(enum_)
        ? enum_.members.flatMap((member) =>
            member.name._tag === 'Present' && member.name.spelling === spelling
              ? [Object.freeze({ enum_, member })]
              : [],
          )
        : [],
    ),
  )
  const unionVariants = index.modules.flatMap((headers) =>
    headers.unions.flatMap((union) =>
      parentIsVisible(union)
        ? union.variants.flatMap((variant) =>
            variant.name._tag === 'Present' && variant.name.spelling === spelling
              ? [Object.freeze({ union, variant })]
              : [],
          )
        : [],
    ),
  )
  if (enumMembers.length + unionVariants.length !== 1) return undefined
  const enumMember = enumMembers.at(0)
  if (enumMember !== undefined) {
    const targetModule =
      enumMember.enum_.canonical._tag === 'Canonical'
        ? enumMember.enum_.canonical.id.module
        : enumMember.enum_.id.sourceId
    return Object.freeze({
      id: `${declarationId(targetModule, enumMember.enum_)}::member:${enumMember.member.id.ordinal}`,
      module: targetModule,
      name: spelling,
      kind: 'EnumMember',
    })
  }
  const unionVariant = unionVariants.at(0)
  if (unionVariant === undefined) return undefined
  const targetModule =
    unionVariant.union.canonical._tag === 'Canonical'
      ? unionVariant.union.canonical.id.module
      : unionVariant.union.id.sourceId
  return Object.freeze({
    id: `${declarationId(targetModule, unionVariant.union)}::variant:${unionVariant.variant.id.ordinal}`,
    module: targetModule,
    name: spelling,
    kind: 'UnionVariant',
  })
}

const resolveDocumentation = (
  snapshot: Analysis.FrontendSnapshot,
  module: string,
  documentation: Document.Document | undefined,
  owner: DeclarationFacts.CanonicalId | undefined,
): Document.Document | undefined =>
  documentation === undefined
    ? undefined
    : Document.resolve(documentation, (spelling) => targetOf(snapshot, module, spelling, owner))

const typeParameterItem = (
  snapshot: Analysis.FrontendSnapshot,
  module: string,
  source: SourceFile.SourceFile,
  ownerId: string,
  parameter: DeclarationFacts.TypeParameterFact,
  ordinal: number,
  owner: DeclarationFacts.CanonicalId | undefined,
): Item => {
  const presentation = Presentation.typeParameter(parameter)
  const documentation = resolveDocumentation(
    snapshot,
    module,
    parsedDocumentation(snapshot, module, source, parameter.syntax),
    owner,
  )
  return Object.freeze({
    id: `${ownerId}::type-parameter:${ordinal}`,
    kind: 'TypeParameter',
    name: nameOf(parameter.name, parameter.type.name),
    visibility: 'Inherited',
    signature: Object.freeze({ text: presentation.text }),
    source: rangeOf(parameter.syntax),
    ...(documentation === undefined ? {} : { documentation }),
    children: Object.freeze([]),
  })
}

const parameterItem = (
  snapshot: Analysis.FrontendSnapshot,
  module: string,
  source: SourceFile.SourceFile,
  ownerId: string,
  parameter: DeclarationFacts.ParameterFact,
  owner: DeclarationFacts.CanonicalId | undefined,
): Item => {
  const presentation = Presentation.parameter(parameter)
  const documentation = resolveDocumentation(
    snapshot,
    module,
    parsedDocumentation(snapshot, module, source, parameter.syntax),
    owner,
  )
  return Object.freeze({
    id: `${ownerId}::parameter:${parameter.id.ordinal}`,
    kind: 'Parameter',
    name: nameOf(parameter.name, '_'),
    visibility: 'Inherited',
    signature: Object.freeze({ text: presentation.text }),
    source: rangeOf(parameter.syntax),
    ...(documentation === undefined ? {} : { documentation }),
    children: Object.freeze([]),
  })
}

const fieldItem = (
  snapshot: Analysis.FrontendSnapshot,
  module: string,
  source: SourceFile.SourceFile,
  ownerId: string,
  field: DeclarationFacts.FieldFact,
  owner: DeclarationFacts.CanonicalId | undefined,
): Item => {
  const presentation = Presentation.field(field)
  const documentation = resolveDocumentation(
    snapshot,
    module,
    parsedDocumentation(snapshot, module, source, field.syntax),
    owner,
  )
  return Object.freeze({
    id: `${ownerId}::field:${field.id.ordinal}`,
    kind: 'Field',
    name: nameOf(field.name, '_'),
    visibility: field.visibility,
    signature: Object.freeze({ text: presentation.text }),
    source: rangeOf(field.syntax),
    ...(documentation === undefined ? {} : { documentation }),
    children: Object.freeze([]),
  })
}

const serviceOperationItem = (
  snapshot: Analysis.FrontendSnapshot,
  module: string,
  source: SourceFile.SourceFile,
  ownerId: string,
  operation: DeclarationFacts.ServiceOperationFact,
  owner: DeclarationFacts.CanonicalId | undefined,
): Item => {
  const presentation = Presentation.serviceOperation(operation)
  const documentation = resolveDocumentation(
    snapshot,
    module,
    parsedDocumentation(snapshot, module, source, operation.syntax),
    owner,
  )
  const id = `${ownerId}::operation:${nameOf(operation.name, '_')}`
  const typeParameters = operation.typeParameters.map((parameter, ordinal) =>
    typeParameterItem(snapshot, module, source, id, parameter, ordinal, owner),
  )
  const parameters = operation.parameters.map((parameter) =>
    parameterItem(snapshot, module, source, id, parameter, owner),
  )
  return Object.freeze({
    id,
    kind: 'Operation',
    name: nameOf(operation.name, '_'),
    visibility: 'Inherited',
    signature: Object.freeze({ text: presentation.text }),
    source: rangeOf(operation.syntax),
    ...(documentation === undefined ? {} : { documentation }),
    children: Object.freeze([...typeParameters, ...parameters]),
  })
}

const enumMemberItem = (
  snapshot: Analysis.FrontendSnapshot,
  module: string,
  source: SourceFile.SourceFile,
  parent: string,
  member: DeclarationFacts.EnumMemberFact,
  owner: DeclarationFacts.CanonicalId | undefined,
): Item => {
  const name = nameOf(member.name, '_')
  const discriminant =
    member.discriminant._tag === 'Available' ? ` = ${member.discriminant.value}` : ''
  const documentation = resolveDocumentation(
    snapshot,
    module,
    parsedDocumentation(snapshot, module, source, member.syntax),
    owner,
  )
  return Object.freeze({
    id: `${parent}::member:${member.id.ordinal}`,
    kind: 'EnumMember',
    name,
    visibility: 'Inherited',
    signature: Object.freeze({ text: `${name}${discriminant}` }),
    source: rangeOf(member.syntax),
    ...(documentation === undefined ? {} : { documentation }),
    children: Object.freeze([]),
  })
}

const unionVariantItem = (
  snapshot: Analysis.FrontendSnapshot,
  module: string,
  source: SourceFile.SourceFile,
  parent: string,
  union: DeclarationFacts.UnionFact,
  variant: DeclarationFacts.UnionVariantFact,
  options: Options,
  owner: DeclarationFacts.CanonicalId | undefined,
): Item => {
  const name = nameOf(variant.name, '_')
  const documentation = resolveDocumentation(
    snapshot,
    module,
    parsedDocumentation(snapshot, module, source, variant.syntax),
    owner,
  )
  const id = `${parent}::variant:${variant.id.ordinal}`
  return Object.freeze({
    id,
    kind: 'UnionVariant',
    name,
    visibility: 'Inherited',
    signature: Object.freeze({ text: Presentation.unionVariant(union, variant).text }),
    source: rangeOf(variant.syntax),
    ...(documentation === undefined ? {} : { documentation }),
    children: Object.freeze(
      variant.fields
        .filter((field) => options.includePrivate === true || field.visibility === 'Public')
        .map((field) => fieldItem(snapshot, module, source, id, field, owner)),
    ),
  })
}

const memberPresentation = (member: DeclarationFacts.MemberFact) => {
  switch (member._tag) {
    case 'FunctionDeclaration':
      return Presentation.functionDeclaration(member)
    case 'StructDeclaration':
      return Presentation.structDeclaration(member)
    case 'EnumDeclaration':
      return Presentation.enumDeclaration(member)
    case 'UnionDeclaration':
      return Presentation.unionDeclaration(member)
    case 'ServiceDeclaration':
    case 'InterfaceDeclaration':
      return Presentation.serviceDeclaration(member)
    case 'RoleDeclaration':
      return Presentation.roleDeclaration(member)
    case 'ConstantDeclaration':
      return Presentation.constantDeclaration(member)
    case 'AliasDeclaration':
      return Presentation.aliasDeclaration(member)
  }
}

const ownedChildren = (
  snapshot: Analysis.FrontendSnapshot,
  module: string,
  source: SourceFile.SourceFile,
  id: string,
  member: DeclarationFacts.MemberFact,
  options: Options,
  owner: DeclarationFacts.CanonicalId | undefined,
): ReadonlyArray<Item> => {
  switch (member._tag) {
    case 'FunctionDeclaration':
      return member.parameters.map((parameter) =>
        parameterItem(snapshot, module, source, id, parameter, owner),
      )
    case 'StructDeclaration':
      return member.fields
        .filter((field) => options.includePrivate === true || field.visibility === 'Public')
        .map((field) => fieldItem(snapshot, module, source, id, field, owner))
    case 'EnumDeclaration':
      return member.members.map((enumMember) =>
        enumMemberItem(snapshot, module, source, id, enumMember, owner),
      )
    case 'UnionDeclaration':
      return member.variants.map((variant) =>
        unionVariantItem(snapshot, module, source, id, member, variant, options, owner),
      )
    case 'ServiceDeclaration':
    case 'InterfaceDeclaration':
      return member.operations.map((operation) =>
        serviceOperationItem(snapshot, module, source, id, operation, owner),
      )
    default:
      return []
  }
}

const memberItem = (
  snapshot: Analysis.FrontendSnapshot,
  module: string,
  source: SourceFile.SourceFile,
  member: DeclarationFacts.MemberFact,
  options: Options,
  associated: ReadonlyArray<DeclarationFacts.MemberFact>,
): Item => {
  const id = declarationId(module, member)
  const owner = ownerOf(member)
  const presentation = memberPresentation(member)
  const documentation = resolveDocumentation(
    snapshot,
    module,
    parsedDocumentation(snapshot, module, source, member.syntax),
    owner,
  )
  const typeParameters = member.typeParameters.map((parameter, ordinal) =>
    typeParameterItem(snapshot, module, source, id, parameter, ordinal, owner),
  )
  const children = ownedChildren(snapshot, module, source, id, member, options, owner)
  const members = associated.map((candidate) =>
    memberItem(snapshot, module, source, candidate, options, []),
  )
  return Object.freeze({
    id,
    kind: itemKind(member),
    name: isAssociated(member)
      ? `${member.associatedMember.ownerSpelling}.${member.associatedMember.name}`
      : nameOf(member.name, '_'),
    visibility: member.visibility,
    signature: Object.freeze({ text: presentation.text }),
    source: rangeOf(member.syntax),
    ...(documentation === undefined ? {} : { documentation }),
    children: Object.freeze([...typeParameters, ...children, ...members]),
  })
}

const declaredType = (fact: DeclarationFacts.DeclaredTypeFact): string =>
  fact._tag === 'Unavailable' ? '_' : fact.spelling

const conformanceItem = (
  snapshot: Analysis.FrontendSnapshot,
  module: string,
  source: SourceFile.SourceFile,
  conformance: DeclarationFacts.ConformanceFact,
): Item => {
  const id = `${module}::implementation:${conformance.ordinal}`
  const documentation = resolveDocumentation(
    snapshot,
    module,
    parsedDocumentation(snapshot, module, source, conformance.syntax),
    undefined,
  )
  const children = conformance.operations.map((operation, ordinal): Item => {
    const operationName = nameOf(operation.name, `operation-${ordinal}`)
    const operationDocumentation = resolveDocumentation(
      snapshot,
      module,
      parsedDocumentation(snapshot, module, source, operation.syntax),
      undefined,
    )
    return Object.freeze({
      id: `${id}::operation:${ordinal}`,
      kind: 'Operation',
      name: operationName,
      visibility: 'Inherited',
      signature: Object.freeze({
        text: `${operationName} = ${operation.target._tag === 'TypePath' ? operation.target.spelling : '_'}`,
      }),
      source: rangeOf(operation.syntax),
      ...(operationDocumentation === undefined ? {} : { documentation: operationDocumentation }),
      children: Object.freeze([]),
    })
  })
  return Object.freeze({
    id,
    kind: 'Implementation',
    name: `${declaredType(conformance.capability)} for ${declaredType(conformance.provider)}`,
    visibility: 'Inherited',
    signature: Object.freeze({
      text: `impl ${declaredType(conformance.capability)} for ${declaredType(conformance.provider)}`,
    }),
    source: rangeOf(conformance.syntax),
    ...(documentation === undefined ? {} : { documentation }),
    children: Object.freeze(children),
  })
}

const moduleModel = (
  snapshot: Analysis.FrontendSnapshot,
  headers: DeclarationFacts.ModuleHeaders,
  options: Options,
): Module | undefined => {
  const syntax = Analysis.moduleAnalysis(snapshot, headers.module)?.syntax
  if (syntax === undefined) return undefined
  const raw = Analysis.moduleDocumentation(snapshot, headers.module)
  const documentation = resolveDocumentation(
    snapshot,
    headers.module,
    raw === undefined ? undefined : Document.parse(syntax.source, raw),
    undefined,
  )
  const visible = (member: DeclarationFacts.MemberFact): boolean =>
    options.includePrivate === true || member.visibility === 'Public'
  const associatedOf = (
    owner: DeclarationFacts.MemberFact,
  ): ReadonlyArray<DeclarationFacts.MemberFact> => {
    if (owner.canonical._tag !== 'Canonical') return []
    const id = owner.canonical.id
    return headers.members.filter(
      (member) =>
        isAssociated(member) &&
        member.associatedMember.owner !== undefined &&
        sameId(member.associatedMember.owner, id) &&
        visible(member),
    )
  }
  const members = headers.members
    .filter((member) => !isAssociated(member) && visible(member))
    .map((member) =>
      memberItem(snapshot, headers.module, syntax.source, member, options, associatedOf(member)),
    )
  const conformances = headers.conformances.map((conformance) =>
    conformanceItem(snapshot, headers.module, syntax.source, conformance),
  )
  return Object.freeze({
    name: headers.module,
    sourceId: syntax.source.id,
    ...(documentation === undefined ? {} : { documentation }),
    items: Object.freeze([...members, ...conformances]),
  })
}

/** Builds documentation lazily from one already-created compiler analysis snapshot. */
export const make = (snapshot: Analysis.FrontendSnapshot, options: Options = {}): Project =>
  Object.freeze({
    schema: 'silk-documentation',
    experimental: true,
    modules: Object.freeze(
      Analysis.declarationIndex(snapshot).modules.flatMap((headers) => {
        const module = moduleModel(snapshot, headers, options)
        return module === undefined ? [] : [module]
      }),
    ),
  })

/** Builds documentation from one multi-root compiler project without repeating shared analysis. */
export const fromProjectAnalysis = (
  self: ProjectAnalysis.ProjectAnalysis,
  options: Options = {},
): Project => {
  const root = self.roots.at(0)
  if (root === undefined) throw new RangeError('Documentation requires at least one project root')
  const snapshot = ProjectAnalysis.view(self, root)
  if (snapshot === undefined)
    throw new RangeError(`Documentation could not find project root ${root}`)
  return make(snapshot, options)
}
