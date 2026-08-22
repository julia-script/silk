import * as ConformanceHead from './ConformanceHead.js'
import { copyAssumptions, copyProof } from './ConformanceProof.js'
import type {
  ConformanceFact,
  ConstantFact,
  DeclarationFact,
  Index,
  InterfaceFact,
  MemberFact,
  ModuleHeaders,
  ServiceFact,
  StructFact,
} from './DeclarationIndex.js'
import {
  closeConformanceSelf,
  interfaceApplication,
  interfaceOperationContracts,
  memberByNominal,
  returnedBorrow,
} from './DeclarationIndex.js'
import {
  attachExposure,
  canonicalKey,
  closeOpaqueReturnType,
  declaredRequirements,
  inferInterfaceWitnessTarget,
  inlineNeighbors,
  inlineParametersOf,
  interfaceWitnessCompatibility,
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
import * as ResolutionSeams from './ResolutionSeams.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Type from './Type.js'

export const complete = (self: Index, resolvers: ResolutionSeams.ResolutionSeams): Index => {
  const diagnostics: Array<Diagnostic.Diagnostic> = [...self.diagnostics]
  let modules = self.modules.map((module): ModuleHeaders => {
    const members = module.members.map((member): MemberFact => {
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
        const memberResolvers: ResolutionSeams.ResolutionSeams =
          member.opaqueResult === undefined || opaqueResult === undefined
            ? resolvers
            : ResolutionSeams.withRepresentationBinding(
                resolvers,
                member.opaqueResult.binder.type,
                opaqueResult.binder.type,
              )
        const parameters = member.parameters.map((parameter) => {
          const resolved = resolveDeclaredType(
            module.module,
            parameter.declaredType,
            resolvers,
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
          resolvers,
          self.modules,
        )
        diagnostics.push(...failureRow.diagnostics)
        const requirementRow = resolveRequirementRow(
          module.module,
          member.requirementRow,
          resolvers,
          self.modules,
        )
        diagnostics.push(...requirementRow.diagnostics)
        const constraints = resolveConstraintFacts(
          module.module,
          member.constraints,
          resolvers,
          self.modules,
        )
        diagnostics.push(...constraints.diagnostics)
        return Object.freeze({
          ...member,
          typeParameters: resolvedTypeParameters,
          parameters: Object.freeze(parameters),
          returnType: result.fact,
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
            resolvers,
            self.modules,
            diagnostics,
          )
          const opaqueResult = resolveOpaqueResult(
            module.module,
            operation.opaqueResult,
            resolvers,
            self.modules,
            diagnostics,
          )
          const operationResolvers: ResolutionSeams.ResolutionSeams =
            operation.opaqueResult === undefined || opaqueResult === undefined
              ? resolvers
              : ResolutionSeams.withRepresentationBinding(
                  resolvers,
                  operation.opaqueResult.binder.type,
                  opaqueResult.binder.type,
                )
          const parameters = operation.parameters.map((parameter) => {
            const resolved = resolveDeclaredType(
              module.module,
              parameter.declaredType,
              resolvers,
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
            resolvers,
            self.modules,
          )
          const requirementRow = resolveRequirementRow(
            module.module,
            operation.requirementRow,
            resolvers,
            self.modules,
          )
          const constraints = resolveConstraintFacts(
            module.module,
            operation.constraints,
            resolvers,
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
      if (member._tag === 'RoleDeclaration') return member
      const fields = member.fields.map((field) => {
        const resolved = resolveDeclaredType(
          module.module,
          field.declaredType,
          resolvers,
          self.modules,
        )
        diagnostics.push(...resolved.diagnostics)
        return Object.freeze({ ...field, declaredType: resolved.fact })
      })
      return Object.freeze({
        ...member,
        typeParameters: resolveBounds(
          module.module,
          member.typeParameters,
          resolvers,
          self.modules,
          diagnostics,
        ),
        fields: Object.freeze(fields),
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
    const closedMembers = members.map((member): MemberFact => {
      if (member._tag !== 'FunctionDeclaration' || member.conformanceImplementation === undefined)
        return member
      const provider = conformanceProviders.get(member.conformanceImplementation.ordinal)
      return provider === undefined ? member : closeConformanceSelf(member, provider)
    })
    return Object.freeze({
      ...module,
      members: Object.freeze(closedMembers),
      declarations: Object.freeze(
        closedMembers.filter(
          (member): member is DeclarationFact => member._tag === 'FunctionDeclaration',
        ),
      ),
      structs: Object.freeze(
        closedMembers.filter((member): member is StructFact => member._tag === 'StructDeclaration'),
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

  // Bounds may precede the interface they name, and the first completion pass intentionally reads
  // the old immutable module graph. Refresh only their applications now that every interface owns
  // resolved operation contracts; no witness selection or executable behavior is decided here.
  modules = modules.map((module): ModuleHeaders => {
    const members = module.members.map((member): MemberFact => {
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
      if (
        sourceContract !== undefined &&
        Type.isNominal(provider) &&
        provider.module !== conformance.module
      ) {
        diagnostics.push(
          invalidDiagnostic(
            `implementation for ${Type.encode(provider)} must be declared in ${provider.module}, the provider's module`,
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
        const substitution = Type.substitution(
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
          if (
            Type.isNominal(provider) &&
            target._tag === 'TypePath' &&
            target.segments.length === 2 &&
            target.segments.at(0)?.spelling === provider.name &&
            target.segments.at(0)?.spelling !== 'Intrinsic'
          ) {
            const targetName = target.segments.at(1)?.spelling
            const implementation = interfaceProviderModule?.declarations.find(
              (declaration) =>
                targetName !== undefined &&
                declaration.name._tag === 'Present' &&
                declaration.name.spelling === targetName,
            )
            if (implementation === undefined) {
              diagnostics.push(
                invalidDiagnostic(
                  `mapped operation ${provider.name}.${targetName ?? '_'} does not exist`,
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
                const detail =
                  problem._tag === 'UnresolvedBinder'
                    ? `cannot infer witness target binder ${problem.binder.name}`
                    : problem._tag === 'ConflictingBinder'
                      ? `witness target binder ${problem.binder.name} is ${Type.encodeGenericArgument(problem.previous)} from ${problem.previousConstraint} but ${Type.encodeGenericArgument(problem.conflicting)} from ${problem.conflictingConstraint}`
                      : `witness target binder ${problem.binder.name} cannot accept ${Type.encodeGenericArgument(problem.argument)}`
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

      if (!Type.isNominal(provider)) continue

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
  const provisionalCopyIndex: Index = Object.freeze({
    _tag: 'DeclarationIndex',
    stage: 'Complete',
    modules: Object.freeze(modules),
    diagnostics: Object.freeze([]),
  })
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
          if (
            parameter.declaredType._tag === 'Resolved' &&
            containsPositionRestrictedBorrow(parameter.declaredType.type) &&
            (!(
              Type.isSlice(parameter.declaredType.type) ||
              Type.isReference(parameter.declaredType.type) ||
              Type.isSlot(parameter.declaredType.type)
            ) ||
              containsPositionRestrictedBorrow(
                Type.isSlice(parameter.declaredType.type)
                  ? parameter.declaredType.type.element
                  : Type.isReference(parameter.declaredType.type)
                    ? parameter.declaredType.type.target
                    : (Type.typeArgumentAt(parameter.declaredType.type, 0) ?? 'never'),
              ))
          ) {
            diagnostics.push(
              Diagnostic.sliceTypePosition('parameter', parameter.declaredType.syntax.span),
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
          (!Type.isSlice(member.returnType.type) || returnedBorrow(member) === undefined)
        ) {
          diagnostics.push(
            Type.isSlice(member.returnType.type)
              ? Diagnostic.invalidReturnedBorrowSignature(member.returnType.syntax.span)
              : Diagnostic.sliceTypePosition('return', member.returnType.syntax.span),
          )
        }
        continue
      }
      if (member._tag === 'ServiceDeclaration' || member._tag === 'InterfaceDeclaration') {
        for (const operation of member.operations) {
          for (const parameter of operation.parameters) {
            if (
              parameter.declaredType._tag === 'Resolved' &&
              containsPositionRestrictedBorrow(parameter.declaredType.type) &&
              (!(
                Type.isSlice(parameter.declaredType.type) ||
                Type.isReference(parameter.declaredType.type) ||
                Type.isSlot(parameter.declaredType.type)
              ) ||
                containsPositionRestrictedBorrow(
                  Type.isSlice(parameter.declaredType.type)
                    ? parameter.declaredType.type.element
                    : Type.isReference(parameter.declaredType.type)
                      ? parameter.declaredType.type.target
                      : (Type.typeArgumentAt(parameter.declaredType.type, 0) ?? 'never'),
                ))
            )
              diagnostics.push(
                Diagnostic.sliceTypePosition('parameter', parameter.declaredType.syntax.span),
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
              Diagnostic.sliceTypePosition('return', operation.returnType.syntax.span),
            )
        }
        continue
      }
      if (member._tag === 'RoleDeclaration') continue
      for (const field of member.fields) {
        if (
          field.declaredType._tag === 'Resolved' &&
          containsPositionRestrictedBorrow(field.declaredType.type)
        ) {
          diagnostics.push(Diagnostic.sliceTypePosition('field', field.declaredType.syntax.span))
        }
      }
    }
  }

  modules = modules.map((module): ModuleHeaders => {
    const members = module.members.map((member): MemberFact => {
      if (member.visibility !== 'Public') return member
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
      if (member._tag === 'RoleDeclaration') return member
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

  const structs = modules.flatMap((module) => module.structs)
  // One graph, two readers: the component walk and the self-edge test below must agree about what
  // "inline" means, or a struct that reaches itself through an indirection is a component of one
  // in the first and a cycle in the second.
  const inlineParameters = inlineParametersOf(structs)
  const cycleCause = new Map<string, Diagnostic.Identity>()
  for (const component of stronglyConnected(structs, inlineParameters)) {
    const first = component.at(0)
    if (first === undefined) continue
    const keys = component.flatMap((struct) =>
      struct.canonical._tag === 'Canonical' ? [canonicalKey(struct.canonical.id)] : [],
    )
    const selfEdge =
      keys.length === 1 &&
      first.fields.some((field) =>
        inlineNeighbors(field, inlineParameters).some((neighbor) => neighbor === keys[0]),
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

  return Object.freeze({
    _tag: 'DeclarationIndex',
    stage: 'Complete',
    modules: Object.freeze(modules),
    diagnostics: Diagnostic.merge(diagnostics),
  })
}
