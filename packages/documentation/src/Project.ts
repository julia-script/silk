import * as Analysis from '@silk-effect/compiler/Analysis'
import type * as DeclarationFacts from '@silk-effect/compiler/DeclarationFacts'
import * as Presentation from '@silk-effect/compiler/Presentation'
import * as ProjectAnalysis from '@silk-effect/compiler/ProjectAnalysis'
import type * as SourceFile from '@silk-effect/compiler/SourceFile'
import type * as SyntaxTree from '@silk-effect/compiler/SyntaxTree'
import * as Document from './Document.js'

export type ItemKind =
  | 'Function'
  | 'Struct'
  | 'Enum'
  | 'EnumMember'
  | 'Service'
  | 'Interface'
  | 'Constant'
  | 'Role'
  | 'Parameter'
  | 'TypeParameter'
  | 'Field'
  | 'Implementation'
  | 'Operation'

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
    case 'ServiceDeclaration':
      return 'Service'
    case 'InterfaceDeclaration':
      return 'Interface'
    case 'RoleDeclaration':
      return 'Role'
    default:
      return 'Constant'
  }
}

const itemKind = (member: DeclarationFacts.MemberFact): ItemKind => {
  switch (member._tag) {
    case 'FunctionDeclaration':
      return 'Function'
    case 'StructDeclaration':
      return 'Struct'
    case 'EnumDeclaration':
      return 'Enum'
    case 'ServiceDeclaration':
      return 'Service'
    case 'InterfaceDeclaration':
      return 'Interface'
    case 'RoleDeclaration':
      return 'Role'
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

const targetOf = (
  snapshot: Analysis.FrontendSnapshot,
  module: string,
  spelling: string,
): Document.LinkTarget | undefined => {
  const lookup = Analysis.lookupName(snapshot, module, spelling)
  if (lookup._tag !== 'Resolved') return undefined
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

const resolveDocumentation = (
  snapshot: Analysis.FrontendSnapshot,
  module: string,
  documentation: Document.Document | undefined,
): Document.Document | undefined =>
  documentation === undefined
    ? undefined
    : Document.resolve(documentation, (spelling) => targetOf(snapshot, module, spelling))

const typeParameterItem = (
  snapshot: Analysis.FrontendSnapshot,
  module: string,
  source: SourceFile.SourceFile,
  ownerId: string,
  parameter: DeclarationFacts.TypeParameterFact,
  ordinal: number,
): Item => {
  const presentation = Presentation.typeParameter(parameter)
  const documentation = resolveDocumentation(
    snapshot,
    module,
    parsedDocumentation(snapshot, module, source, parameter.syntax),
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
): Item => {
  const presentation = Presentation.parameter(parameter)
  const documentation = resolveDocumentation(
    snapshot,
    module,
    parsedDocumentation(snapshot, module, source, parameter.syntax),
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
): Item => {
  const presentation = Presentation.field(field)
  const documentation = resolveDocumentation(
    snapshot,
    module,
    parsedDocumentation(snapshot, module, source, field.syntax),
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
): Item => {
  const presentation = Presentation.serviceOperation(operation)
  const documentation = resolveDocumentation(
    snapshot,
    module,
    parsedDocumentation(snapshot, module, source, operation.syntax),
  )
  const id = `${ownerId}::operation:${nameOf(operation.name, '_')}`
  const typeParameters = operation.typeParameters.map((parameter, ordinal) =>
    typeParameterItem(snapshot, module, source, id, parameter, ordinal),
  )
  const parameters = operation.parameters.map((parameter) =>
    parameterItem(snapshot, module, source, id, parameter),
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

const enumMemberItem = (parent: string, member: DeclarationFacts.EnumMemberFact): Item => {
  const name = nameOf(member.name, '_')
  const discriminant =
    member.discriminant._tag === 'Available' ? ` = ${member.discriminant.value}` : ''
  return Object.freeze({
    id: `${parent}::member:${member.id.ordinal}`,
    kind: 'EnumMember',
    name,
    visibility: 'Inherited',
    signature: Object.freeze({ text: `${name}${discriminant}` }),
    source: rangeOf(member.syntax),
    children: Object.freeze([]),
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
    case 'ServiceDeclaration':
    case 'InterfaceDeclaration':
      return Presentation.serviceDeclaration(member)
    case 'RoleDeclaration':
      return Presentation.roleDeclaration(member)
    case 'ConstantDeclaration':
      return Presentation.constantDeclaration(member)
  }
}

const ownedChildren = (
  snapshot: Analysis.FrontendSnapshot,
  module: string,
  source: SourceFile.SourceFile,
  id: string,
  member: DeclarationFacts.MemberFact,
  options: Options,
): ReadonlyArray<Item> => {
  switch (member._tag) {
    case 'FunctionDeclaration':
      return member.parameters.map((parameter) =>
        parameterItem(snapshot, module, source, id, parameter),
      )
    case 'StructDeclaration':
      return member.fields
        .filter((field) => options.includePrivate === true || field.visibility === 'Public')
        .map((field) => fieldItem(snapshot, module, source, id, field))
    case 'EnumDeclaration':
      return member.members.map((enumMember) => enumMemberItem(id, enumMember))
    case 'ServiceDeclaration':
    case 'InterfaceDeclaration':
      return member.operations.map((operation) =>
        serviceOperationItem(snapshot, module, source, id, operation),
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
): Item => {
  const id = declarationId(module, member)
  const presentation = memberPresentation(member)
  const documentation = resolveDocumentation(
    snapshot,
    module,
    parsedDocumentation(snapshot, module, source, member.syntax),
  )
  const typeParameters = member.typeParameters.map((parameter, ordinal) =>
    typeParameterItem(snapshot, module, source, id, parameter, ordinal),
  )
  const children = ownedChildren(snapshot, module, source, id, member, options)
  return Object.freeze({
    id,
    kind: itemKind(member),
    name: nameOf(member.name, '_'),
    visibility: member.visibility,
    signature: Object.freeze({ text: presentation.text }),
    source: rangeOf(member.syntax),
    ...(documentation === undefined ? {} : { documentation }),
    children: Object.freeze([...typeParameters, ...children]),
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
  )
  const children = conformance.operations.map((operation, ordinal): Item => {
    const operationName = nameOf(operation.name, `operation-${ordinal}`)
    const operationDocumentation = resolveDocumentation(
      snapshot,
      module,
      parsedDocumentation(snapshot, module, source, operation.syntax),
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
  )
  const members = headers.members
    .filter((member) => options.includePrivate === true || member.visibility === 'Public')
    .map((member) => memberItem(snapshot, headers.module, syntax.source, member, options))
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
