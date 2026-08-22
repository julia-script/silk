import * as Analysis from '@silk-effect/compiler/Analysis'
import type * as DeclarationFacts from '@silk-effect/compiler/DeclarationFacts'
import * as Presentation from '@silk-effect/compiler/Presentation'
import type * as SourceFile from '@silk-effect/compiler/SourceFile'
import type * as SyntaxTree from '@silk-effect/compiler/SyntaxTree'
import * as Document from './Document.js'

export type ItemKind =
  | 'Function'
  | 'Struct'
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
    kind:
      lookup.declaration._tag === 'FunctionDeclaration'
        ? 'Function'
        : lookup.declaration._tag === 'StructDeclaration'
          ? 'Struct'
          : lookup.declaration._tag === 'ServiceDeclaration'
            ? 'Service'
            : lookup.declaration._tag === 'InterfaceDeclaration'
              ? 'Interface'
              : lookup.declaration._tag === 'RoleDeclaration'
                ? 'Role'
                : 'Constant',
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

const memberItem = (
  snapshot: Analysis.FrontendSnapshot,
  module: string,
  source: SourceFile.SourceFile,
  member: DeclarationFacts.MemberFact,
  options: Options,
): Item => {
  const id = declarationId(module, member)
  const presentation =
    member._tag === 'FunctionDeclaration'
      ? Presentation.functionDeclaration(member)
      : member._tag === 'StructDeclaration'
        ? Presentation.structDeclaration(member)
        : member._tag === 'ServiceDeclaration' || member._tag === 'InterfaceDeclaration'
          ? Presentation.serviceDeclaration(member)
          : member._tag === 'RoleDeclaration'
            ? Presentation.roleDeclaration(member)
            : Presentation.constantDeclaration(member)
  const documentation = resolveDocumentation(
    snapshot,
    module,
    parsedDocumentation(snapshot, module, source, member.syntax),
  )
  const typeParameters = member.typeParameters.map((parameter, ordinal) =>
    typeParameterItem(snapshot, module, source, id, parameter, ordinal),
  )
  const ownedChildren =
    member._tag === 'FunctionDeclaration'
      ? member.parameters.map((parameter) => parameterItem(snapshot, module, source, id, parameter))
      : member._tag === 'StructDeclaration'
        ? member.fields
            .filter((field) => options.includePrivate === true || field.visibility === 'Public')
            .map((field) => fieldItem(snapshot, module, source, id, field))
        : member._tag === 'ServiceDeclaration' || member._tag === 'InterfaceDeclaration'
          ? member.operations.map((operation) =>
              serviceOperationItem(snapshot, module, source, id, operation),
            )
          : []
  return Object.freeze({
    id,
    kind:
      member._tag === 'FunctionDeclaration'
        ? 'Function'
        : member._tag === 'StructDeclaration'
          ? 'Struct'
          : member._tag === 'ServiceDeclaration'
            ? 'Service'
            : member._tag === 'InterfaceDeclaration'
              ? 'Interface'
              : member._tag === 'RoleDeclaration'
                ? 'Role'
                : 'Constant',
    name: nameOf(member.name, '_'),
    visibility: member.visibility,
    signature: Object.freeze({ text: presentation.text }),
    source: rangeOf(member.syntax),
    ...(documentation === undefined ? {} : { documentation }),
    children: Object.freeze([...typeParameters, ...ownedChildren]),
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
