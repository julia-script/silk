import * as CAbi from './CAbi.js'
import * as CLayout from './CLayout.js'
import * as ConformanceHead from './ConformanceHead.js'
import { copyAssumptions, copyProof } from './ConformanceProof.js'
import type {
  ConformanceFact,
  ConstantFact,
  DeclarationFact,
  DeclaredName,
  EnumFact,
  InherentImplFact,
  InterfaceFact,
  MemberFact,
  ModuleHeaders,
  ParameterFact,
  ReturnTypeFact,
  ServiceFact,
  StructFact,
  TypeParameterFact,
  UnionFact,
} from './DeclarationFacts.js'
import {
  closeConformanceSelf,
  interfaceApplication,
  interfaceOperationContracts,
  returnedBorrow,
} from './DeclarationFacts.js'
import * as DeclarationIndex from './DeclarationIndex.js'
import * as NameResolution from './NameResolution.js'
import {
  attachExposure,
  canonicalKey,
  closeOpaqueReturnType,
  declaredRequirements,
  inferInterfaceWitnessTarget,
  inlineNeighbors,
  inlineParametersOf,
  interfaceWitnessCompatibility,
  memberByNominal,
  refreshInterfaceApplications,
  resolveBounds,
  resolveConstraintFacts,
  resolveDeclaredType,
  resolveFailureRow,
  resolveOpaqueResult,
  resolveRequirementRow,
  sealedWitnessCompatibility,
  semanticConstraints,
  stronglyConnected,
  unpromisedWitnessBound,
  witnessBinding,
} from './DeclarationResolution.js'
import * as Diagnostic from './Diagnostic.js'
import * as InterfaceWitnessCompatibility from './InterfaceWitnessCompatibility.js'
import * as Intrinsic from './Intrinsic.js'
import * as TypeInference from './internal/TypeInference.js'
import * as ResolutionSeams from './ResolutionSeams.js'
import * as SourceSpan from './SourceSpan.js'
import * as SyntaxTree from './SyntaxTree.js'
import * as Type from './Type.js'

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

/** Makes resolved executable-representation bounds visible while closing their declaration. */
const withResolvedRepresentationParameters = (
  resolvers: ResolutionSeams.ResolutionSeams,
  collected: ReadonlyArray<TypeParameterFact>,
  resolved: ReadonlyArray<TypeParameterFact>,
): ResolutionSeams.ResolutionSeams =>
  collected.reduce((current, parameter, ordinal) => {
    const closed = resolved.at(ordinal)
    return closed === undefined || closed.type.representationBound === undefined
      ? current
      : ResolutionSeams.withRepresentationBinding(current, parameter.type, closed.type)
  }, resolvers)

/** The declaration an inherent impl owner names: any nominal declaration kind, including enums. */
const nominalOwnerDeclaration = (
  modules: ReadonlyArray<ModuleHeaders>,
  nominal: Type.Nominal,
): MemberFact | undefined =>
  modules
    .find((module) => module.module === nominal.module)
    ?.members.find(
      (member) =>
        member.canonical._tag === 'Canonical' &&
        member.canonical.id.name === nominal.name &&
        (member._tag === 'StructDeclaration' ||
          member._tag === 'UnionDeclaration' ||
          member._tag === 'EnumDeclaration' ||
          member._tag === 'ServiceDeclaration' ||
          member._tag === 'InterfaceDeclaration'),
    )

/** The one source module allowed to own an interface or service implementation. */
const sourceConformanceOwner = (
  capability: Type.Nominal,
  provider: Type.Type,
): string | undefined => {
  if (Type.isNominal(provider)) return provider.module
  if (Type.isBuiltin(provider) || Type.isString(provider)) return capability.module
  return undefined
}

/** Structural providers whose only coherent source owner is the declaring contract module. */
const isContractOwnedInlineProvider = (provider: Type.Type): boolean =>
  Type.isBuiltin(provider) || Type.isString(provider)

const foreignAdmission = (
  parameters: ReadonlyArray<ParameterFact>,
  result: ReturnTypeFact,
): ReadonlyArray<Diagnostic.Diagnostic> =>
  [...parameters.map((parameter) => parameter.declaredType), result].flatMap(
    (declared, ordinal) => {
      if (declared._tag !== 'Resolved') return []
      const admission = CAbi.admit(
        declared.type,
        ordinal < parameters.length ? 'Parameter' : 'Result',
      )
      return admission._tag === 'Admitted'
        ? []
        : [Diagnostic.foreignTypeNotAdmitted(declared.spelling, 'C', declared.syntax.span)]
    },
  )

export const complete = (
  self: DeclarationIndex.Index,
  resolvers: ResolutionSeams.ResolutionSeams,
): DeclarationIndex.Index => {
  const diagnostics: Array<Diagnostic.Diagnostic> = [...self.diagnostics]
  let modules = self.modules.map((module): ModuleHeaders => {
    const members = module.members.map((member): MemberFact => {
      if (member._tag === 'AliasDeclaration') {
        // The resolver memoizes each alias, so a use that already forced it reported its
        // diagnostics; this forcing only guarantees every alias resolves at least once.
        const resolved = resolvers.alias?.(member)
        if (resolved === undefined) return member
        diagnostics.push(...resolved.diagnostics)
        return Object.freeze({ ...member, target: resolved.fact })
      }
      if (member._tag === 'ConstantDeclaration') {
        const resolved = resolveDeclaredType(
          module.module,
          member.declaredType,
          resolvers,
          self.modules,
        )
        diagnostics.push(...resolved.diagnostics)
        return Object.freeze({ ...member, declaredType: resolved.fact })
      }
      if (member._tag === 'FunctionDeclaration') {
        const resolvedTypeParameters = resolveBounds(
          module.module,
          member.typeParameters,
          resolvers,
          self.modules,
          diagnostics,
        )
        const opaqueResult = resolveOpaqueResult(
          module.module,
          member.opaqueResult,
          resolvers,
          self.modules,
          diagnostics,
        )
        const typeParameterResolvers = withResolvedRepresentationParameters(
          resolvers,
          member.typeParameters,
          resolvedTypeParameters,
        )
        const memberResolvers: ResolutionSeams.ResolutionSeams =
          member.opaqueResult === undefined || opaqueResult === undefined
            ? typeParameterResolvers
            : ResolutionSeams.withRepresentationBinding(
                typeParameterResolvers,
                member.opaqueResult.binder.type,
                opaqueResult.binder.type,
              )
        const parameters = member.parameters.map((parameter) => {
          const resolved = resolveDeclaredType(
            module.module,
            parameter.declaredType,
            memberResolvers,
            self.modules,
          )
          diagnostics.push(...resolved.diagnostics)
          return Object.freeze({ ...parameter, declaredType: resolved.fact })
        })
        const resolvedResult = resolveDeclaredType(
          module.module,
          member.returnType,
          memberResolvers,
          self.modules,
        )
        diagnostics.push(...resolvedResult.diagnostics)
        const result = closeOpaqueReturnType(
          resolvedResult.fact,
          opaqueResult,
          resolvedTypeParameters,
        )
        const failureRow = resolveFailureRow(
          module.module,
          member.failureRow,
          memberResolvers,
          self.modules,
        )
        diagnostics.push(...failureRow.diagnostics)
        const requirementRow = resolveRequirementRow(
          module.module,
          member.requirementRow,
          memberResolvers,
          self.modules,
        )
        diagnostics.push(...requirementRow.diagnostics)
        const constraints = resolveConstraintFacts(
          module.module,
          member.constraints,
          memberResolvers,
          self.modules,
        )
        diagnostics.push(...constraints.diagnostics)
        const admission =
          member.foreign === undefined && member.foreignExport === undefined
            ? []
            : foreignAdmission(parameters, result.fact)
        diagnostics.push(...admission)
        const { foreignExport, ...retained } = member
        return Object.freeze({
          ...retained,
          // An export outside the C subset publishes no symbol, so discovery never roots it.
          ...(admission.length === 0 && foreignExport !== undefined ? { foreignExport } : {}),
          typeParameters: resolvedTypeParameters,
          parameters: Object.freeze(parameters),
          // A foreign or exported header outside the C subset withholds its result so no callable
          // is published.
          returnType:
            admission.length === 0
              ? result.fact
              : Object.freeze({ _tag: 'Unavailable' as const, syntax: result.fact.syntax }),
          ...(result.opaqueResult === undefined ? {} : { opaqueResult: result.opaqueResult }),
          failureRow: failureRow.fact,
          requirementRow: requirementRow.fact,
          constraints: constraints.facts,
          constraintContracts: semanticConstraints(constraints.facts),
        })
      }
      if (member._tag === 'ServiceDeclaration' || member._tag === 'InterfaceDeclaration') {
        const resolvedMemberTypeParameters = resolveBounds(
          module.module,
          member.typeParameters,
          resolvers,
          self.modules,
          diagnostics,
        )
        const memberResolvers = withResolvedRepresentationParameters(
          resolvers,
          member.typeParameters,
          resolvedMemberTypeParameters,
        )
        const operations = member.operations.map((operation) => {
          if (operation.opaqueResult !== undefined) {
            const owner = member.name._tag === 'Present' ? member.name.spelling : '<anonymous>'
            const name = operation.name._tag === 'Present' ? operation.name.spelling : '<anonymous>'
            diagnostics.push(
              Diagnostic.bodylessOpaqueResult(
                `${owner}.${name}`,
                member._tag === 'ServiceDeclaration' ? 'ServiceOperation' : 'InterfaceOperation',
                operation.opaqueResult.syntax.span,
              ),
            )
          }
          const resolvedOperationTypeParameters = resolveBounds(
            module.module,
            operation.typeParameters,
            memberResolvers,
            self.modules,
            diagnostics,
          )
          const opaqueResult = resolveOpaqueResult(
            module.module,
            operation.opaqueResult,
            memberResolvers,
            self.modules,
            diagnostics,
          )
          const operationTypeParameterResolvers = withResolvedRepresentationParameters(
            memberResolvers,
            operation.typeParameters,
            resolvedOperationTypeParameters,
          )
          const operationResolvers: ResolutionSeams.ResolutionSeams =
            operation.opaqueResult === undefined || opaqueResult === undefined
              ? operationTypeParameterResolvers
              : ResolutionSeams.withRepresentationBinding(
                  operationTypeParameterResolvers,
                  operation.opaqueResult.binder.type,
                  opaqueResult.binder.type,
                )
          const parameters = operation.parameters.map((parameter) => {
            const resolved = resolveDeclaredType(
              module.module,
              parameter.declaredType,
              operationResolvers,
              self.modules,
            )
            diagnostics.push(...resolved.diagnostics)
            return Object.freeze({ ...parameter, declaredType: resolved.fact })
          })
          const resolvedResult = resolveDeclaredType(
            module.module,
            operation.returnType,
            operationResolvers,
            self.modules,
          )
          const result = closeOpaqueReturnType(resolvedResult.fact, opaqueResult, [
            ...resolvedMemberTypeParameters,
            ...resolvedOperationTypeParameters,
          ])
          const failureRow = resolveFailureRow(
            module.module,
            operation.failureRow,
            operationResolvers,
            self.modules,
          )
          const requirementRow = resolveRequirementRow(
            module.module,
            operation.requirementRow,
            operationResolvers,
            self.modules,
          )
          const constraints = resolveConstraintFacts(
            module.module,
            operation.constraints,
            operationResolvers,
            self.modules,
          )
          diagnostics.push(
            ...resolvedResult.diagnostics,
            ...failureRow.diagnostics,
            ...requirementRow.diagnostics,
            ...constraints.diagnostics,
          )
          return Object.freeze({
            ...operation,
            typeParameters: resolvedOperationTypeParameters,
            parameters: Object.freeze(parameters),
            returnType: result.fact,
            ...(result.opaqueResult === undefined ? {} : { opaqueResult: result.opaqueResult }),
            failureRow: failureRow.fact,
            requirementRow: requirementRow.fact,
            constraints: constraints.facts,
            constraintContracts: semanticConstraints(constraints.facts),
          })
        })
        const completed = Object.freeze({
          ...member,
          typeParameters: resolvedMemberTypeParameters,
          operations: Object.freeze(operations),
        })
        return Object.freeze({
          ...completed,
          operationContracts: interfaceOperationContracts(completed, operations),
        })
      }
      if (member._tag === 'RoleDeclaration' || member._tag === 'EnumDeclaration') return member
      const resolveFields = (fields: StructFact['fields']): StructFact['fields'] =>
        Object.freeze(
          fields.map((field) => {
            const resolved = resolveDeclaredType(
              module.module,
              field.declaredType,
              resolvers,
              self.modules,
            )
            diagnostics.push(...resolved.diagnostics)
            return Object.freeze({ ...field, declaredType: resolved.fact })
          }),
        )
      if (member._tag === 'UnionDeclaration') {
        const variants = Object.freeze(
          member.variants.map((variant) =>
            Object.freeze({ ...variant, fields: resolveFields(variant.fields) }),
          ),
        )
        const unavailableCauses = variants.flatMap((variant) =>
          variant.fields.flatMap((field) =>
            field.declaredType._tag === 'Unresolved' && field.declaredType.cause !== undefined
              ? [field.declaredType.cause]
              : [],
          ),
        )
        return Object.freeze({
          ...member,
          typeParameters: resolveBounds(
            module.module,
            member.typeParameters,
            resolvers,
            self.modules,
            diagnostics,
          ),
          variants,
          validity:
            member.validity._tag === 'Valid' && unavailableCauses.length === 0
              ? member.validity
              : Object.freeze({
                  _tag: 'Invalid' as const,
                  causes: Object.freeze([
                    ...(member.validity._tag === 'Invalid' ? member.validity.causes : []),
                    ...unavailableCauses,
                  ]),
                }),
        })
      }
      return Object.freeze({
        ...member,
        typeParameters: resolveBounds(
          module.module,
          member.typeParameters,
          resolvers,
          self.modules,
          diagnostics,
        ),
        fields: resolveFields(member.fields),
      })
    })
    const conformances = module.conformances.map((conformance) => {
      const capability = resolveDeclaredType(
        module.module,
        conformance.capability,
        resolvers,
        self.modules,
      )
      const provider = resolveDeclaredType(
        module.module,
        conformance.provider,
        resolvers,
        self.modules,
      )
      diagnostics.push(...capability.diagnostics, ...provider.diagnostics)
      const hook =
        conformance.hook === undefined
          ? undefined
          : (() => {
              const parameterType = resolveDeclaredType(
                module.module,
                conformance.hook.parameterType,
                resolvers,
                self.modules,
              )
              const returnType = resolveDeclaredType(
                module.module,
                conformance.hook.returnType,
                resolvers,
                self.modules,
              )
              const failureRow = resolveFailureRow(
                module.module,
                conformance.hook.failureRow,
                resolvers,
                self.modules,
              )
              const requirementRow = resolveRequirementRow(
                module.module,
                conformance.hook.requirementRow,
                resolvers,
                self.modules,
              )
              diagnostics.push(
                ...parameterType.diagnostics,
                ...returnType.diagnostics,
                ...failureRow.diagnostics,
                ...requirementRow.diagnostics,
              )
              return Object.freeze({
                ...conformance.hook,
                parameterType: parameterType.fact,
                returnType: returnType.fact,
                failureRow: failureRow.fact,
                requirementRow: requirementRow.fact,
              })
            })()
      const requirements = conformance.requirements.map((requirement) => {
        const resolved = resolveDeclaredType(
          module.module,
          requirement.capability,
          resolvers,
          self.modules,
        )
        diagnostics.push(...resolved.diagnostics)
        return Object.freeze({ ...requirement, capability: resolved.fact })
      })
      return Object.freeze({
        ...conformance,
        requirements: Object.freeze(requirements),
        capability: capability.fact,
        provider: provider.fact,
        ...(hook === undefined ? {} : { hook }),
      })
    })
    const conformanceProviders = new Map(
      conformances.flatMap((conformance) =>
        conformance.provider._tag === 'Resolved'
          ? [[conformance.ordinal, conformance.provider.type] as const]
          : [],
      ),
    )
    // An inherent head resolves its owner exactly as a conformance resolves its provider. The
    // checks that need scope run here: the owner must be a nominal of this module named directly
    // (not through an alias), or the head publishes no members.
    const inherentImpls = module.inherentImpls.map((head): InherentImplFact => {
      const owner = resolveDeclaredType(module.module, head.owner, resolvers, self.modules)
      // A head already rejected at collection keeps only that diagnostic; its owner is resolved
      // for closing `Self` in its members, not for a second report.
      if (head.validity._tag === 'Invalid') return Object.freeze({ ...head, owner: owner.fact })
      // The owner is a declaration of this module named by its own spelling. A local declaration
      // wins over the type resolver's answer because a zero-data owner struct may share its
      // spelling with a builtin storage type (`Slot`, `RawBuffer`), and the impl names the
      // declaration. Only when no local declaration matches does the resolver's answer tell a
      // foreign owner from an alias or a non-nominal.
      const localOwner = module.members.find(
        (member) =>
          member.canonical._tag === 'Canonical' &&
          member.canonical.id.name === head.ownerSpelling &&
          NameResolution.isNominalOwner(member),
      )
      if (
        localOwner !== undefined &&
        localOwner.canonical._tag === 'Canonical' &&
        owner.fact._tag === 'Resolved'
      ) {
        // The resolver's fact keeps the head's argument facts for tooling; only its type is replaced
        // by the local declaration's nominal, and its diagnostics stand unless they came from the
        // builtin that shadows the local spelling.
        if (Type.isNominal(owner.fact.type)) diagnostics.push(...owner.diagnostics)
        const binders = head.typeParameters.filter(
          (parameter) => parameter.duplicateOf === undefined,
        )
        const ownerType = Type.nominal(
          module.module,
          localOwner.canonical.id.name,
          binders.map((parameter) => parameter.type),
        )
        return Object.freeze({ ...head, owner: Object.freeze({ ...owner.fact, type: ownerType }) })
      }
      const ownerType = owner.fact._tag === 'Resolved' ? owner.fact.type : undefined
      const nominal = ownerType !== undefined && Type.isNominal(ownerType) ? ownerType : undefined
      const ownerDeclaration =
        nominal === undefined ? undefined : nominalOwnerDeclaration(self.modules, nominal)
      const localAlias = module.members.some(
        (member) =>
          member._tag === 'AliasDeclaration' &&
          member.name._tag === 'Present' &&
          member.name.spelling === head.ownerSpelling,
      )
      let problem: 'ForeignOwner' | 'AliasOwner' | 'NotNominal' | undefined
      if (nominal === undefined || ownerDeclaration === undefined) {
        problem = owner.fact._tag === 'Resolved' ? 'NotNominal' : undefined
      } else if (localAlias || nominal.name !== head.ownerSpelling) {
        problem = 'AliasOwner'
      } else if (nominal.module !== module.module) {
        problem = 'ForeignOwner'
      } else {
        problem = undefined
      }
      if (problem === undefined) return Object.freeze({ ...head, owner: owner.fact })
      const diagnostic = Diagnostic.invalidInherentHead(
        head.ownerSpelling,
        problem,
        head.owner._tag === 'Unavailable' ? head.syntax.span : head.owner.syntax.span,
      )
      diagnostics.push(diagnostic)
      return Object.freeze({
        ...head,
        owner: owner.fact,
        validity: Object.freeze({
          _tag: 'Invalid' as const,
          cause: Diagnostic.identity(diagnostic),
        }),
      })
    })
    // Every head whose owner resolved closes `Self` on its members, even a rejected one, so a
    // rejected member's body reports only the head's diagnostic and not a cascade about `Self`.
    const resolvedOwners = new Map(
      inherentImpls.flatMap((head) =>
        head.owner._tag === 'Resolved' && Type.isNominal(head.owner.type)
          ? [[head.ordinal, head.owner.type] as const]
          : [],
      ),
    )
    const inherentOwners = new Map(
      inherentImpls.flatMap((head) =>
        head.validity._tag === 'Valid' &&
        head.owner._tag === 'Resolved' &&
        Type.isNominal(head.owner.type)
          ? [[head.ordinal, head.owner.type] as const]
          : [],
      ),
    )
    interface OwnerItem {
      readonly kind: string
      readonly span: SourceSpan.SourceSpan
    }
    const ownerItemNames = (owner: Type.Nominal): ReadonlyMap<string, OwnerItem> => {
      const declaration = nominalOwnerDeclaration(self.modules, owner)
      const names = new Map<string, OwnerItem>()
      const add = (name: DeclaredName, kind: string): void => {
        if (name._tag === 'Present') names.set(name.spelling, { kind, span: name.token.span })
      }
      if (declaration?._tag === 'StructDeclaration')
        for (const field of declaration.fields) add(field.name, 'field')
      if (declaration?._tag === 'UnionDeclaration')
        for (const variant of declaration.variants) add(variant.name, 'variant')
      if (declaration?._tag === 'EnumDeclaration') {
        for (const member of declaration.members) add(member.name, 'member')
        names.set('value', { kind: 'generated operation', span: declaration.syntax.span })
      }
      if (
        declaration?._tag === 'ServiceDeclaration' ||
        declaration?._tag === 'InterfaceDeclaration'
      )
        for (const operation of declaration.operations) add(operation.name, 'operation')
      return names
    }
    const closedMembers = members.map((member): MemberFact => {
      if (member._tag !== 'FunctionDeclaration') return member
      if (member.conformanceImplementation !== undefined) {
        const provider = conformanceProviders.get(member.conformanceImplementation.ordinal)
        return provider === undefined
          ? member
          : closeConformanceSelf(member, member.conformanceImplementation.self, provider)
      }
      const association = member.associatedMember
      if (association === undefined) return member
      const owner = inherentOwners.get(association.ordinal)
      const unpublished = (): MemberFact => {
        const resolvedOwner = resolvedOwners.get(association.ordinal)
        const closed =
          resolvedOwner === undefined
            ? member
            : closeConformanceSelf(member, association.self, resolvedOwner)
        return member.canonical._tag === 'Canonical'
          ? Object.freeze({
              ...closed,
              canonical: Object.freeze({ _tag: 'Unidentified' as const }),
            })
          : closed
      }
      if (owner === undefined) return unpublished()
      const collision = ownerItemNames(owner).get(association.name)
      if (collision !== undefined && member.canonical._tag === 'Canonical') {
        diagnostics.push(
          Diagnostic.invalidInherentMember(
            association.ownerSpelling,
            association.name,
            'Collision',
            member.name._tag === 'Present' ? member.name.token.span : member.syntax.span,
            collision.kind,
            collision.span,
          ),
        )
        return unpublished()
      }
      const closed = closeConformanceSelf(member, association.self, owner)
      // A member bound may name `Self` too (`U: Like<Self>`); close it to the owner and rebuild
      // the application so proof search sees the concrete capability.
      const selfSubstitution: Type.Substitution = new Map<string, Type.GenericArgument>([
        [Type.key(association.self), owner],
      ])
      const typeParameters = closed.typeParameters.map((parameter) =>
        parameter.bounds.length === 0
          ? parameter
          : Object.freeze({
              ...parameter,
              bounds: Object.freeze(
                parameter.bounds.map((bound) => {
                  if (bound._tag !== 'ResolvedBound') return bound
                  const capability = Type.substitute(bound.application.capability, selfSubstitution)
                  if (
                    !Type.isNominal(capability) ||
                    Type.equals(capability, bound.application.capability)
                  )
                    return bound
                  const declaration = memberByNominal(self.modules, capability)
                  const application =
                    declaration?._tag === 'InterfaceDeclaration' ||
                    declaration?._tag === 'ServiceDeclaration'
                      ? interfaceApplication(declaration, capability, parameter.type)
                      : undefined
                  return application === undefined
                    ? bound
                    : Object.freeze({ ...bound, application })
                }),
              ),
            }),
      )
      return Object.freeze({
        ...closed,
        typeParameters: Object.freeze(typeParameters),
        associatedMember: Object.freeze({
          ...association,
          owner: Object.freeze({
            _tag: 'CanonicalDeclarationId' as const,
            module: owner.module,
            name: owner.name,
          }),
        }),
      })
    })
    return Object.freeze({
      ...module,
      inherentImpls: Object.freeze(inherentImpls),
      members: Object.freeze(closedMembers),
      declarations: Object.freeze(
        closedMembers.filter(
          (member): member is DeclarationFact => member._tag === 'FunctionDeclaration',
        ),
      ),
      structs: Object.freeze(
        closedMembers.filter((member): member is StructFact => member._tag === 'StructDeclaration'),
      ),
      enums: Object.freeze(
        closedMembers.filter((member): member is EnumFact => member._tag === 'EnumDeclaration'),
      ),
      unions: Object.freeze(
        closedMembers.filter((member): member is UnionFact => member._tag === 'UnionDeclaration'),
      ),
      services: Object.freeze(
        closedMembers.filter(
          (member): member is ServiceFact => member._tag === 'ServiceDeclaration',
        ),
      ),
      interfaces: Object.freeze(
        closedMembers.filter(
          (member): member is InterfaceFact => member._tag === 'InterfaceDeclaration',
        ),
      ),
      constants: Object.freeze(
        closedMembers.filter(
          (member): member is ConstantFact => member._tag === 'ConstantDeclaration',
        ),
      ),
      conformances: Object.freeze(conformances),
    })
  })

  // C-layout validation needs the whole resolved declaration graph: a field may embed a record
  // declared later or imported from another module. Invalid contracts keep the nominal available
  // to ordinary Silk tooling, but they do not retain the foreign-layout promise.
  const resolveCLayoutStruct = CLayout.resolveFrom(modules)
  modules = modules.map((module): ModuleHeaders => {
    const members = module.members.map((member): MemberFact => {
      if (member._tag !== 'StructDeclaration' || member.layout._tag !== 'Foreign') return member
      const record = member.name._tag === 'Present' ? member.name.spelling : '<anonymous>'
      let invalid = false
      if (member.typeParameters.length !== 0) {
        invalid = true
        diagnostics.push(
          Diagnostic.genericCLayoutRecord(
            record,
            SyntaxTree.directNode(member.syntax, 'TypeParameterList')?.span ?? member.syntax.span,
          ),
        )
      }
      const admissions =
        member.typeParameters.length === 0
          ? CLayout.admitFields(member, resolveCLayoutStruct)
          : Object.freeze([])
      for (const [ordinal, field] of member.fields.entries()) {
        if (field.declaredType._tag !== 'Resolved') {
          invalid = true
          continue
        }
        const admission = admissions.at(ordinal)
        if (admission?._tag !== 'NotAdmitted') continue
        invalid = true
        diagnostics.push(
          Diagnostic.unsupportedCLayoutField(
            record,
            field.name._tag === 'Present' ? field.name.spelling : `${field.id.ordinal}`,
            field.declaredType.spelling,
            tightSpan(field.declaredType.syntax),
          ),
        )
      }
      return invalid
        ? Object.freeze({
            ...member,
            layout: Object.freeze({
              _tag: 'InvalidForeign' as const,
              abi: member.layout.abi,
              abiSpan: member.layout.abiSpan,
            }),
          })
        : member
    })
    return Object.freeze({
      ...module,
      members: Object.freeze(members),
      structs: Object.freeze(
        members.filter((member): member is StructFact => member._tag === 'StructDeclaration'),
      ),
    })
  })

  // Bounds may precede the interface they name, and the first completion pass intentionally reads
  // the old immutable module graph. Refresh only their applications now that every interface owns
  // resolved operation contracts; no witness selection or executable behavior is decided here.
  modules = modules.map((module): ModuleHeaders => {
    const members = module.members.map((member): MemberFact => {
      if (member._tag === 'EnumDeclaration') return member
      const typeParameters = refreshInterfaceApplications(member.typeParameters, modules)
      if (member._tag !== 'ServiceDeclaration' && member._tag !== 'InterfaceDeclaration')
        return Object.freeze({ ...member, typeParameters })
      const operations = Object.freeze(
        member.operations.map((operation) =>
          Object.freeze({
            ...operation,
            typeParameters: refreshInterfaceApplications(operation.typeParameters, modules),
          }),
        ),
      )
      return Object.freeze({
        ...member,
        typeParameters,
        operations,
        operationContracts: interfaceOperationContracts(member, operations),
      })
    })
    const conformances = Object.freeze(
      module.conformances.map((conformance) => {
        const capability =
          conformance.capability._tag === 'Resolved' && Type.isNominal(conformance.capability.type)
            ? conformance.capability.type
            : undefined
        const provider =
          conformance.provider._tag === 'Resolved' ? conformance.provider.type : undefined
        const declaration =
          capability === undefined ? undefined : memberByNominal(modules, capability)
        const application =
          capability !== undefined &&
          provider !== undefined &&
          (declaration?._tag === 'InterfaceDeclaration' ||
            declaration?._tag === 'ServiceDeclaration')
            ? interfaceApplication(declaration, capability, provider)
            : undefined
        return Object.freeze({
          ...conformance,
          typeParameters: refreshInterfaceApplications(conformance.typeParameters, modules),
          operations: Object.freeze(
            conformance.operations.map((operation) => {
              const contract = application?.operations.find(
                (candidate) =>
                  operation.name._tag === 'Present' &&
                  candidate.declaration.name._tag === 'Present' &&
                  candidate.declaration.name.spelling === operation.name.spelling,
              )
              return Object.freeze({
                ...operation,
                ...(contract === undefined ? {} : { contract }),
              })
            }),
          ),
        })
      }),
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
        members.filter((member): member is ConstantFact => member._tag === 'ConstantDeclaration'),
      ),
      conformances,
    })
  })

  // Coherence and termination are program-wide questions, so both are answered once every module's
  // headers have resolved and before any conformance body is validated. Overlap is decided on head
  // shapes alone: no bound is consulted, because whether a bound is satisfiable depends on the whole
  // program and would let one specialization silently change which witness it selects.
  const acceptedHeads: Array<{
    readonly module: string
    readonly ordinal: number
    readonly head: ConformanceHead.ConformanceHead
    readonly span: SourceSpan.SourceSpan
  }> = []
  modules = modules.map((module) =>
    Object.freeze({
      ...module,
      conformances: Object.freeze(
        module.conformances.map((conformance): ConformanceFact => {
          if (
            conformance.capability._tag !== 'Resolved' ||
            !Type.isNominal(conformance.capability.type) ||
            conformance.provider._tag !== 'Resolved'
          )
            return conformance
          const requirements = declaredRequirements(modules, conformance)
          const head = ConformanceHead.make(
            conformance.capability.type,
            conformance.provider.type,
            requirements,
          )
          // A damaged requirement is retained on the conformance fact for diagnostics, but cannot
          // be shortened into a zero-obligation head. Header validation below reports the source
          // error; leaving termination unavailable keeps the fact out of coherence and proof search.
          if (requirements.length !== conformance.requirements.length)
            return Object.freeze({ ...conformance, head })
          const contract = memberByNominal(modules, conformance.capability.type)
          if (
            (contract?._tag === 'InterfaceDeclaration' ||
              contract?._tag === 'ServiceDeclaration') &&
            Type.isNominal(conformance.provider.type) &&
            conformance.provider.type.module !== module.module
          )
            return Object.freeze({ ...conformance, head })
          const failures = ConformanceHead.terminationFailures(head)
          if (failures.length > 0)
            diagnostics.push(
              Diagnostic.nonTerminatingConformance(
                ConformanceHead.encode(head),
                failures.map(ConformanceHead.describeTermination),
                conformance.syntax.span,
              ),
            )
          // This is the one authority on whether two conformances may cover one provider. Two
          // unbounded headers with one identical shape are the case a reader recognizes as a
          // duplicate and are named that way; two headers a bound is the only difference between
          // are reported as the overlap they are, because calling them duplicates would suggest the
          // bounds were compared. Comparing the alpha-normalized heads is what makes the two tests
          // agree — keying the duplicate check on unnormalized capabilities, as an earlier version
          // did, let exactly the bound-distinguished pair match neither and survive.
          const headKey = ConformanceHead.key(head)
          const overlapping = acceptedHeads.find((candidate) =>
            ConformanceHead.mayOverlap(candidate.head, head),
          )
          if (overlapping === undefined)
            acceptedHeads.push(
              Object.freeze({
                module: module.module,
                ordinal: conformance.ordinal,
                head,
                span: conformance.syntax.span,
              }),
            )
          else if (
            ConformanceHead.key(overlapping.head) === headKey &&
            head.requirements.length === 0 &&
            overlapping.head.requirements.length === 0
          )
            diagnostics.push(
              Object.freeze({
                ...Diagnostic.invalidConformance(
                  `duplicate ${conformance.capability.type.name} implementation for ${Type.encode(conformance.provider.type)}`,
                  conformance.syntax.span,
                ),
                relatedSpans: Object.freeze([
                  Object.freeze({ label: 'first implementation', span: overlapping.span }),
                ]),
              }),
            )
          else
            diagnostics.push(
              Diagnostic.overlappingConformance(
                ConformanceHead.encode(head),
                ConformanceHead.encode(overlapping.head),
                conformance.syntax.span,
                overlapping.span,
              ),
            )
          return Object.freeze({
            ...conformance,
            head,
            coherence:
              overlapping === undefined
                ? Object.freeze({ _tag: 'Coherent' as const })
                : Object.freeze({
                    _tag: 'Overlapping' as const,
                    module: overlapping.module,
                    ordinal: overlapping.ordinal,
                  }),
            termination:
              failures.length === 0
                ? Object.freeze({ _tag: 'Terminating' as const })
                : Object.freeze({ _tag: 'NonTerminating' as const, failures }),
          })
        }),
      ),
    }),
  )

  const containsPositionRestrictedBorrow = Type.containsPositionRestrictedBorrow

  const invalidConformances = new Set<ConformanceFact>()
  const inferredWitnessArguments = new Map<
    ConformanceFact['operations'][number],
    ReadonlyArray<Type.GenericArgument>
  >()
  for (const module of modules) {
    for (const conformance of module.conformances) {
      const markInvalid = (): void => {
        invalidConformances.add(conformance)
      }
      const rejectConformance = <A extends Diagnostic.Diagnostic>(diagnostic: A): A => {
        markInvalid()
        return diagnostic
      }
      const invalidDiagnostic = (
        ...args: Parameters<typeof Diagnostic.invalidConformance>
      ): ReturnType<typeof Diagnostic.invalidConformance> => {
        return rejectConformance(Diagnostic.invalidConformance(...args))
      }
      if (
        conformance.capability._tag !== 'Resolved' ||
        !Type.isNominal(conformance.capability.type) ||
        conformance.provider._tag !== 'Resolved'
      ) {
        diagnostics.push(
          invalidDiagnostic(
            'the capability must resolve to a nominal type and the provider must resolve to a type',
            conformance.syntax.span,
          ),
        )
        continue
      }
      const capability = conformance.capability.type
      const provider = conformance.provider.type
      const sourceMember = memberByNominal(modules, capability)
      const sourceContract =
        sourceMember?._tag === 'InterfaceDeclaration' || sourceMember?._tag === 'ServiceDeclaration'
          ? sourceMember
          : undefined
      const conformanceOwner = sourceConformanceOwner(capability, provider)
      if (sourceContract !== undefined && conformanceOwner === undefined) {
        diagnostics.push(
          invalidDiagnostic(
            'interface and service providers must be nominal types, scalar types, or string',
            conformance.syntax.span,
          ),
        )
        continue
      }
      if (sourceContract !== undefined && conformanceOwner !== conformance.module) {
        const ownership = Type.isNominal(provider)
          ? "the provider's module"
          : "the contract's module for scalar and string providers"
        diagnostics.push(
          invalidDiagnostic(
            `implementation for ${Type.encode(provider)} must be declared in ${conformanceOwner}, ${ownership}`,
            conformance.syntax.span,
          ),
        )
        continue
      }
      if (sourceContract !== undefined && !Type.isTypeArgument(provider)) {
        diagnostics.push(
          invalidDiagnostic(
            'interface and service providers must be concrete value types',
            conformance.syntax.span,
          ),
        )
        continue
      }
      const declaredParameters = conformance.typeParameters
        .filter((parameter) => parameter.duplicateOf === undefined)
        .map((parameter) => parameter.type)
      // Source interface and service arguments may mention the header's binders, because one
      // parametric provider may implement one equally parametric source capability. Compiler-sealed
      // capabilities remain concrete.
      if (
        !Type.isConcrete(capability) &&
        (sourceContract === undefined || declaredParameters.length === 0)
      ) {
        diagnostics.push(
          invalidDiagnostic(
            'the capability must be concrete; impl type parameters may only bind the provider',
            conformance.syntax.span,
          ),
        )
        continue
      }
      // This predicate is the exact complement of what `declaredRequirements` admits. They have to
      // agree: a requirement the reader accepts but the reader of obligations drops would be a
      // bound nothing ever proves.
      const unstatedRequirement = conformance.requirements.find((requirement) => {
        if (requirement.capability._tag !== 'Resolved') return true
        const applied = requirement.capability.type
        if (!Type.isNominal(applied)) return true
        const declaration = memberByNominal(modules, applied)
        return (
          !Type.equals(applied, Type.copyCapability) &&
          declaration?._tag !== 'InterfaceDeclaration' &&
          declaration?._tag !== 'ServiceDeclaration'
        )
      })
      if (unstatedRequirement !== undefined) {
        diagnostics.push(
          invalidDiagnostic(
            `requirement ${unstatedRequirement.spelling} must be an interface or service contract`,
            unstatedRequirement.syntax.span,
          ),
        )
        continue
      }
      if (conformance.termination._tag === 'NonTerminating') continue
      if (conformance.coherence._tag === 'Overlapping') continue
      const usedParameterKeys = new Set(
        Type.parameters(provider).map((parameter) => Type.key(parameter)),
      )
      const unused = declaredParameters.filter(
        (parameter) => !usedParameterKeys.has(Type.key(parameter)),
      )
      if (unused.length > 0) {
        diagnostics.push(
          invalidDiagnostic(
            `impl type parameter ${unused.map((parameter) => parameter.name).join(', ')} is not used by the provider type`,
            conformance.syntax.span,
          ),
        )
        continue
      }
      if (sourceContract !== undefined) {
        if (conformance.hook !== undefined) {
          diagnostics.push(
            invalidDiagnostic(
              `${capability.name} implementations use operation mappings, not a hook body`,
              conformance.hook.syntax.span,
            ),
          )
          continue
        }
        const mapped = new Map<string, ConformanceFact['operations'][number]>()
        let invalid = false
        for (const mapping of conformance.operations) {
          if (mapping.name._tag !== 'Present') {
            markInvalid()
            invalid = true
            continue
          }
          if (mapped.has(mapping.name.spelling)) {
            diagnostics.push(
              invalidDiagnostic(
                `duplicate ${capability.name}.${mapping.name.spelling} operation mapping`,
                mapping.syntax.span,
              ),
            )
            invalid = true
          } else mapped.set(mapping.name.spelling, mapping)
        }
        const operationNames = new Set(
          sourceContract.operations.flatMap((operation) =>
            operation.name._tag === 'Present' ? [operation.name.spelling] : [],
          ),
        )
        const missing = [...operationNames].filter((name) => !mapped.has(name))
        const extra = [...mapped.keys()].filter((name) => !operationNames.has(name))
        if (missing.length > 0 || extra.length > 0) {
          diagnostics.push(
            invalidDiagnostic(
              [
                ...(missing.length === 0 ? [] : [`missing ${missing.join(', ')}`]),
                ...(extra.length === 0 ? [] : [`unknown ${extra.join(', ')}`]),
              ].join('; '),
              conformance.syntax.span,
            ),
          )
          invalid = true
        }
        const substitution = TypeInference.substitution(
          sourceContract.typeParameters.map((parameter) => parameter.type),
          capability.arguments,
        )
        if (substitution === undefined) {
          diagnostics.push(
            invalidDiagnostic(
              `${capability.name} implementation has the wrong interface type-argument arity`,
              conformance.syntax.span,
            ),
          )
          continue
        }
        if (invalid) continue
        const interfaceProviderModule = Type.isNominal(provider)
          ? modules.find((candidate) => candidate.module === provider.module)
          : undefined
        const conformanceModule = modules.find(
          (candidate) => candidate.module === conformance.module,
        )
        for (const contract of sourceContract.operations) {
          if (contract.name._tag !== 'Present') continue
          const mapping = mapped.get(contract.name.spelling)
          if (mapping === undefined) continue
          const target = mapping.target
          const contractName = contract.name.spelling
          const rejectIncompatibleMapping = (detail?: string): void => {
            diagnostics.push(
              invalidDiagnostic(
                `${target._tag === 'TypePath' ? target.spelling : '_'} is incompatible with ${capability.name}.${contractName}${detail === undefined ? '' : `: ${detail}`}`,
                mapping.syntax.span,
              ),
            )
          }
          // Every witness uses the applied contract's literal operands. Source functions and
          // sealed intrinsics are checked by the same ownership rules.
          const mappedProviderTarget =
            mapping.form === 'Mapped' &&
            Type.isNominal(provider) &&
            target._tag === 'TypePath' &&
            target.segments.length === 2 &&
            target.segments.at(0)?.spelling === provider.name &&
            target.segments.at(0)?.spelling !== 'Intrinsic'
          const inlineTarget = mapping.form === 'Inline' && target._tag === 'TypePath'
          if (inlineTarget || mappedProviderTarget) {
            const targetName = target.segments.at(1)?.spelling
            const implementation =
              mapping.form === 'Inline'
                ? conformanceModule?.declarations.find(
                    (declaration) =>
                      declaration.conformanceImplementation?.ordinal === conformance.ordinal &&
                      declaration.conformanceImplementation.operation === contractName,
                  )
                : interfaceProviderModule?.declarations.find(
                    (declaration) =>
                      targetName !== undefined &&
                      declaration.name._tag === 'Present' &&
                      declaration.name.spelling === targetName,
                  )
            if (implementation === undefined) {
              diagnostics.push(
                invalidDiagnostic(
                  mapping.form === 'Inline'
                    ? `inline operation ${capability.name}.${contractName} does not exist`
                    : `mapped operation ${Type.isNominal(provider) ? provider.name : Type.encode(provider)}.${targetName ?? '_'} does not exist`,
                  mapping.syntax.span,
                ),
              )
              continue
            }
            const inference = inferInterfaceWitnessTarget(implementation, mapping.contract)
            if (inference === undefined || inference._tag === 'Failed') {
              if (inference?._tag === 'Failed') {
                const problem = inference.problem
                if (problem._tag === 'IncompatibleConstraint') {
                  rejectIncompatibleMapping()
                  continue
                }
                let detail: string
                if (problem._tag === 'UnresolvedBinder') {
                  detail = `cannot infer witness target binder ${problem.binder.name}`
                } else if (problem._tag === 'ConflictingBinder') {
                  detail = `witness target binder ${problem.binder.name} is ${Type.encodeGenericArgument(problem.previous)} from ${problem.previousConstraint} but ${Type.encodeGenericArgument(problem.conflicting)} from ${problem.conflictingConstraint}`
                } else {
                  detail = `witness target binder ${problem.binder.name} cannot accept ${Type.encodeGenericArgument(problem.argument)}`
                }
                diagnostics.push(
                  invalidDiagnostic(`${target.spelling}: ${detail}`, mapping.syntax.span),
                )
              } else rejectIncompatibleMapping()
              continue
            }
            const binding = witnessBinding(implementation, declaredParameters)
            // A witness may only ask for what the header already promises. Its own bounds are the
            // obligations its body will discharge, so a bound the header never requires would be
            // proved nowhere and would surface as a call with no lowering rather than a diagnostic.
            const unpromisedBound = unpromisedWitnessBound(
              binding,
              inference.arguments,
              conformance,
            )
            if (unpromisedBound !== undefined) {
              diagnostics.push(
                invalidDiagnostic(
                  `${target.spelling} requires ${unpromisedBound.bound.spelling} for ${unpromisedBound.binder.type.name}, which ${capability.name} for ${Type.encode(provider)} does not require`,
                  mapping.syntax.span,
                ),
              )
              continue
            }
            const compatibility = interfaceWitnessCompatibility(
              mapping.contract,
              implementation,
              inference.substitution,
            )
            if (compatibility === undefined || compatibility._tag === 'Incompatible')
              rejectIncompatibleMapping(
                compatibility === undefined
                  ? undefined
                  : InterfaceWitnessCompatibility.describe(compatibility),
              )
            else inferredWitnessArguments.set(mapping, inference.arguments)
            continue
          }
          if (mapping.form === 'Inline') {
            rejectIncompatibleMapping('inline witness target is unavailable')
            continue
          }
          if (
            isContractOwnedInlineProvider(provider) &&
            mapping.form === 'Mapped' &&
            (target._tag !== 'TypePath' || target.segments.at(0)?.spelling !== 'Intrinsic')
          ) {
            rejectIncompatibleMapping('scalar and string source witnesses must be declared inline')
            continue
          }
          const operation =
            target._tag === 'TypePath' &&
            target.segments.length === 2 &&
            target.segments.at(0)?.spelling === 'Intrinsic'
              ? Intrinsic.findOperation('Intrinsic', target.segments.at(1)?.spelling ?? '')
              : undefined
          const builtin =
            operation !== undefined && Intrinsic.isBuiltinOperation(operation)
              ? operation
              : undefined
          const parameters = builtin?.callParameters
          const result = builtin?.rule.result
          const compatibility =
            parameters === undefined || result === undefined
              ? undefined
              : sealedWitnessCompatibility(mapping.contract, parameters, result)
          if (
            operation === undefined ||
            operation.unsafe ||
            compatibility === undefined ||
            compatibility._tag === 'Incompatible'
          )
            rejectIncompatibleMapping(
              compatibility === undefined
                ? undefined
                : InterfaceWitnessCompatibility.describe(compatibility),
            )
        }
        continue
      }

      if (Type.equals(capability, Type.copyCapability) && !Type.isNominal(provider)) {
        diagnostics.push(
          invalidDiagnostic(
            `Copy cannot be implemented for structural provider ${Type.encode(provider)}; shared references are compiler-proven Copy and every other structural type follows its sealed rule`,
            conformance.syntax.span,
          ),
        )
        continue
      }

      if (!Type.isNominal(provider)) continue

      const providerEnum = modules
        .flatMap((candidate) => candidate.enums)
        .find(
          (candidate) =>
            candidate.canonical._tag === 'Canonical' &&
            candidate.canonical.id.module === provider.module &&
            candidate.canonical.id.name === provider.name,
        )
      if (
        providerEnum !== undefined &&
        (Type.equals(capability, Type.copyCapability) ||
          Type.equals(capability, Type.dropCapability))
      ) {
        diagnostics.push(
          invalidDiagnostic(
            `scalar enum ${Type.encode(provider)} has sealed compiler-proved Copy semantics and cannot implement ${capability.name}`,
            conformance.syntax.span,
          ),
        )
        continue
      }

      if (Type.equals(capability, Type.copyCapability)) {
        if (
          Type.isIntrinsicNominal(provider) ||
          provider.module !== conformance.module ||
          conformance.operations.length !== 0 ||
          conformance.hook !== undefined
        ) {
          diagnostics.push(
            invalidDiagnostic(
              'Copy requires one empty impl on a struct declared in the same module',
              conformance.syntax.span,
            ),
          )
        }
        continue
      }

      if (Type.equals(capability, Type.dropCapability)) {
        const hook = conformance.hook
        if (conformance.operations.length !== 0 || hook === undefined) {
          diagnostics.push(
            rejectConformance(
              Diagnostic.invalidDropHook(
                'Drop requires one inline fn drop hook and no operation mappings',
                conformance.syntax.span,
              ),
            ),
          )
          continue
        }
        const parameter = hook.parameterType
        const validSelf =
          parameter._tag === 'Resolved' &&
          Type.isReference(parameter.type) &&
          parameter.type.access === 'Exclusive' &&
          Type.equals(parameter.type.target, provider)
        if (
          hook.name._tag !== 'Present' ||
          hook.name.spelling !== 'drop' ||
          hook.functionKind !== 'Ordinary' ||
          hook.typeParameterCount !== 0 ||
          hook.parameterCount !== 1 ||
          hook.parameterName._tag !== 'Present' ||
          hook.parameterName.spelling !== 'self' ||
          !validSelf ||
          hook.returnType._tag !== 'Resolved' ||
          !Type.equals(hook.returnType.type, Type.unit) ||
          hook.failureRow.failures.length !== 0 ||
          hook.requirementRow.requirements.length !== 0
        ) {
          diagnostics.push(
            rejectConformance(
              Diagnostic.invalidDropHook(
                'the hook must be fn drop(self: &mut Provider) -> () with no generics, failures, or requirements',
                hook.syntax.span,
              ),
            ),
          )
        }
        continue
      }

      diagnostics.push(
        invalidDiagnostic(
          `unsupported compiler-sealed capability ${Type.encode(capability)}`,
          conformance.syntax.span,
        ),
      )
    }
  }

  modules = modules.map((module) =>
    Object.freeze({
      ...module,
      conformances: Object.freeze(
        module.conformances.map((conformance) =>
          Object.freeze({
            ...conformance,
            operations: Object.freeze(
              conformance.operations.map((operation) => {
                const targetArguments = inferredWitnessArguments.get(operation)
                return targetArguments === undefined
                  ? operation
                  : Object.freeze({ ...operation, targetArguments })
              }),
            ),
            validity:
              invalidConformances.has(conformance) ||
              conformance.coherence._tag !== 'Coherent' ||
              conformance.termination._tag !== 'Terminating'
                ? Object.freeze({ _tag: 'InvalidConformance' as const })
                : Object.freeze({ _tag: 'ValidConformance' as const }),
          }),
        ),
      ),
    }),
  )

  // Copy syntax is validated above; now validate the property over the complete provisional field
  // graph before any downstream phase can observe the conformances as evidence.
  const provisionalCopyIndex = DeclarationIndex.make('Complete', modules, [])
  const invalidCopyKeys = new Set<string>()
  for (const module of modules) {
    for (const conformance of module.conformances) {
      if (
        conformance.validity._tag !== 'ValidConformance' ||
        conformance.capability._tag !== 'Resolved' ||
        !Type.equals(conformance.capability.type, Type.copyCapability) ||
        conformance.provider._tag !== 'Resolved'
      )
        continue
      const proof = copyProof(
        provisionalCopyIndex,
        conformance.provider.type,
        copyAssumptions(conformance),
      )
      if (
        proof._tag === 'Copy' ||
        (proof._tag === 'UnavailableCopy' &&
          proof.reason.includes('executable Copy depends on its concrete realized captures'))
      )
        continue
      invalidCopyKeys.add(`${module.module}\u0000${conformance.ordinal}`)
      diagnostics.push(
        Diagnostic.invalidConformance(
          `Copy cannot be implemented for ${Type.encode(conformance.provider.type)}: ${proof.reason}`,
          conformance.syntax.span,
        ),
      )
    }
  }
  if (invalidCopyKeys.size > 0)
    modules = modules.map((module) =>
      Object.freeze({
        ...module,
        conformances: Object.freeze(
          module.conformances.map((conformance) =>
            invalidCopyKeys.has(`${module.module}\u0000${conformance.ordinal}`)
              ? Object.freeze({
                  ...conformance,
                  validity: Object.freeze({ _tag: 'InvalidConformance' as const }),
                })
              : conformance,
          ),
        ),
      }),
    )

  for (const module of modules) {
    for (const member of module.members) {
      if (member._tag === 'ConstantDeclaration') continue
      if (member._tag === 'FunctionDeclaration') {
        for (const parameter of member.parameters) {
          const mut = SyntaxTree.directToken(parameter.syntax, 'MutKeyword')
          if (
            mut !== undefined &&
            parameter.declaredType._tag === 'Resolved' &&
            (Type.isReference(parameter.declaredType.type) ||
              Type.isSlice(parameter.declaredType.type))
          ) {
            diagnostics.push(Diagnostic.invalidMutableParameter('BorrowedView', mut.span))
          }
          if (
            parameter.declaredType._tag === 'Resolved' &&
            !Type.isParameterBorrowType(parameter.declaredType.type)
          ) {
            diagnostics.push(
              Diagnostic.borrowedViewTypePosition('parameter', parameter.declaredType.syntax.span),
            )
          }
        }
        if (
          member.returnType._tag === 'Resolved' &&
          containsPositionRestrictedBorrow(member.returnType.type) &&
          (!Type.isSlot(member.returnType.type) ||
            containsPositionRestrictedBorrow(
              Type.typeArgumentAt(member.returnType.type, 0) ?? 'never',
            )) &&
          (!Type.isViewBorrow(member.returnType.type) || returnedBorrow(member) === undefined)
        ) {
          diagnostics.push(
            Type.isViewBorrow(member.returnType.type)
              ? Diagnostic.invalidReturnedBorrowSignature(member.returnType.syntax.span)
              : Diagnostic.borrowedViewTypePosition('return', member.returnType.syntax.span),
          )
        }
        continue
      }
      if (member._tag === 'ServiceDeclaration' || member._tag === 'InterfaceDeclaration') {
        for (const operation of member.operations) {
          for (const parameter of operation.parameters) {
            const mut = SyntaxTree.directToken(parameter.syntax, 'MutKeyword')
            if (mut !== undefined) {
              diagnostics.push(Diagnostic.invalidMutableParameter('Contract', mut.span))
            }
            if (
              parameter.declaredType._tag === 'Resolved' &&
              !Type.isParameterBorrowType(parameter.declaredType.type)
            )
              diagnostics.push(
                Diagnostic.borrowedViewTypePosition(
                  'parameter',
                  parameter.declaredType.syntax.span,
                ),
              )
          }
          if (
            operation.returnType._tag === 'Resolved' &&
            containsPositionRestrictedBorrow(operation.returnType.type) &&
            (!Type.isSlot(operation.returnType.type) ||
              containsPositionRestrictedBorrow(
                Type.typeArgumentAt(operation.returnType.type, 0) ?? 'never',
              ))
          )
            diagnostics.push(
              Type.isViewBorrow(operation.returnType.type)
                ? Diagnostic.invalidReturnedBorrowSignature(operation.returnType.syntax.span)
                : Diagnostic.borrowedViewTypePosition('return', operation.returnType.syntax.span),
            )
        }
        continue
      }
      if (
        member._tag === 'RoleDeclaration' ||
        member._tag === 'EnumDeclaration' ||
        member._tag === 'AliasDeclaration'
      )
        continue
      const fields =
        member._tag === 'UnionDeclaration'
          ? member.variants.flatMap((variant) => variant.fields)
          : member.fields
      for (const field of fields) {
        if (
          field.declaredType._tag === 'Resolved' &&
          containsPositionRestrictedBorrow(field.declaredType.type)
        ) {
          diagnostics.push(
            Diagnostic.borrowedViewTypePosition('field', field.declaredType.syntax.span),
          )
        }
      }
    }
  }

  modules = modules.map((module): ModuleHeaders => {
    const members = module.members.map((member): MemberFact => {
      if (member.visibility !== 'Public') return member
      // The alias resolver attached exposure when it erased the target.
      if (member._tag === 'AliasDeclaration') return member
      if (member._tag === 'ConstantDeclaration') {
        return Object.freeze({
          ...member,
          declaredType: attachExposure(member.declaredType, modules, diagnostics),
        })
      }
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
      if (member._tag === 'ServiceDeclaration' || member._tag === 'InterfaceDeclaration') {
        const operations = member.operations.map((operation) =>
          Object.freeze({
            ...operation,
            parameters: Object.freeze(
              operation.parameters.map((parameter) =>
                Object.freeze({
                  ...parameter,
                  declaredType: attachExposure(parameter.declaredType, modules, diagnostics),
                }),
              ),
            ),
            returnType: attachExposure(operation.returnType, modules, diagnostics),
            failureRow: Object.freeze({
              ...operation.failureRow,
              members: Object.freeze(
                operation.failureRow.members.map((failure) =>
                  attachExposure(failure, modules, diagnostics),
                ),
              ),
            }),
          }),
        )
        const exposed = Object.freeze({ ...member, operations: Object.freeze(operations) })
        return Object.freeze({
          ...exposed,
          operationContracts: interfaceOperationContracts(exposed, operations),
        })
      }
      if (member._tag === 'RoleDeclaration' || member._tag === 'EnumDeclaration') return member
      const exposeFields = (fields: StructFact['fields']): StructFact['fields'] =>
        Object.freeze(
          fields.map((field) =>
            field.visibility === 'Public'
              ? Object.freeze({
                  ...field,
                  declaredType: attachExposure(field.declaredType, modules, diagnostics),
                })
              : field,
          ),
        )
      return member._tag === 'UnionDeclaration'
        ? Object.freeze({
            ...member,
            variants: Object.freeze(
              member.variants.map((variant) =>
                Object.freeze({ ...variant, fields: exposeFields(variant.fields) }),
              ),
            ),
          })
        : Object.freeze({ ...member, fields: exposeFields(member.fields) })
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
        members.filter((member): member is ConstantFact => member._tag === 'ConstantDeclaration'),
      ),
    })
  })

  const aggregates = modules.flatMap((module) => [...module.structs, ...module.unions])
  // One graph, two readers: the component walk and the self-edge test below must agree about what
  // "inline" means, or a struct that reaches itself through an indirection is a component of one
  // in the first and a cycle in the second.
  const inlineParameters = inlineParametersOf(aggregates)
  const cycleCause = new Map<string, Diagnostic.Identity>()
  for (const component of stronglyConnected(aggregates, inlineParameters)) {
    const first = component.at(0)
    if (first === undefined) continue
    const keys = component.flatMap((struct) =>
      struct.canonical._tag === 'Canonical' ? [canonicalKey(struct.canonical.id)] : [],
    )
    const selfEdge =
      keys.length === 1 &&
      (first._tag === 'StructDeclaration'
        ? first.fields
        : first.variants.flatMap((variant) => variant.fields)
      ).some((field) =>
        inlineNeighbors(field, inlineParameters).some((neighbor) => neighbor === keys[0]),
      )
    if (keys.length < 2 && !selfEdge) continue
    const diagnostic = Diagnostic.inlineRecursiveAggregate(
      Object.freeze(keys),
      first.name._tag === 'Present' ? first.name.token.span : first.syntax.span,
    )
    diagnostics.push(diagnostic)
    const cause = Diagnostic.identity(diagnostic)
    for (const key of keys) cycleCause.set(key, cause)
  }

  modules = modules.map((module): ModuleHeaders => {
    const members = module.members.map((member): MemberFact => {
      if (member._tag !== 'StructDeclaration' && member._tag !== 'UnionDeclaration') return member
      const fields =
        member._tag === 'StructDeclaration'
          ? member.fields
          : member.variants.flatMap((variant) => variant.fields)
      const dependencyMap = new Map<string, Type.Nominal>()
      for (const field of fields) {
        if (field.declaredType._tag === 'Resolved') {
          for (const type of Type.nominals(field.declaredType.type)) {
            dependencyMap.set(Type.key(type), type)
          }
        }
      }
      const dependencies = [...dependencyMap.values()].sort(Type.compare)
      const fieldCause = fields.find(
        (field) =>
          (field.declaredType._tag === 'Unresolved' && field.declaredType.cause !== undefined) ||
          (field.declaredType._tag === 'Resolved' &&
            field.declaredType.exposureCause !== undefined),
      )
      const key =
        member.canonical._tag === 'Canonical' ? canonicalKey(member.canonical.id) : undefined
      let fieldDependencyCause: ReturnType<typeof Diagnostic.identity> | undefined
      if (fieldCause?.declaredType._tag === 'Unresolved') {
        fieldDependencyCause = fieldCause.declaredType.cause
      } else if (fieldCause?.declaredType._tag === 'Resolved') {
        fieldDependencyCause = fieldCause.declaredType.exposureCause
      }
      const cause = (key === undefined ? undefined : cycleCause.get(key)) ?? fieldDependencyCause
      const dependency = Object.freeze(
        cause === undefined
          ? { _tag: 'Available' as const, types: Object.freeze(dependencies) }
          : { _tag: 'Unavailable' as const, types: Object.freeze(dependencies), cause },
      )
      if (member._tag === 'UnionDeclaration' && cause !== undefined)
        return Object.freeze({
          ...member,
          dependency,
          validity: Object.freeze({
            _tag: 'Invalid' as const,
            causes: Object.freeze([
              ...(member.validity._tag === 'Invalid' ? member.validity.causes : []),
              cause,
            ]),
          }),
        })
      return Object.freeze({
        ...member,
        dependency,
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
        members.filter((member): member is ConstantFact => member._tag === 'ConstantDeclaration'),
      ),
      diagnostics: Diagnostic.merge(moduleDiagnostics),
    })
  })

  return DeclarationIndex.make('Complete', modules, Diagnostic.merge(diagnostics))
}
