import type * as ConformanceHead from './ConformanceHead.js'
import * as Constraint from './Constraint.js'
import type {
  BoundFact,
  CanonicalId,
  ConformanceFact,
  ConstraintFact,
  DeclarationFact,
  DeclaredTypeFact,
  FailureRowFact,
  FieldFact,
  InterfaceFact,
  InterfaceOperationApplicationFact,
  MemberFact,
  ModuleHeaders,
  OpaqueResultFact,
  ParameterFact,
  RequirementRoleFact,
  RequirementRowFact,
  ReturnTypeFact,
  RowExpressionFact,
  ServiceFact,
  StructFact,
  TypeParameterFact,
  TypePathFact,
  TypeResolution,
  TypeResolver,
} from './DeclarationFacts.js'
import {
  copyApplication,
  interfaceApplication,
  lookupDeclaration,
  requirementRoleIdentity,
} from './DeclarationFacts.js'
import type { Index } from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as InterfaceWitnessCompatibility from './InterfaceWitnessCompatibility.js'
import * as InterfaceWitnessInference from './InterfaceWitnessInference.js'
import * as Graph from './internal/Graph.js'
import * as TypeInference from './internal/TypeInference.js'
import * as RequirementRow from './RequirementRow.js'
import * as ResolutionSeams from './ResolutionSeams.js'
import * as RowAlgebra from './RowAlgebra.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Type from './Type.js'

const resolveExactRepresentation = (
  module: string,
  fact: Extract<DeclaredTypeFact, { readonly _tag: 'ExactRepresentation' }>,
  resolvers: ResolutionSeams.ResolutionSeams,
  modules: ReadonlyArray<ModuleHeaders>,
): TypeResolution => {
  const arguments_ = fact.arguments.map((argument) =>
    resolveDeclaredType(module, argument, resolvers, modules),
  )
  const argumentDiagnostics = arguments_.flatMap((argument) => argument.diagnostics)
  const reject = (diagnostic: Diagnostic.Diagnostic, candidate?: MemberFact): TypeResolution => {
    const canonical = candidate?.canonical._tag === 'Canonical' ? candidate.canonical.id : undefined
    return Object.freeze({
      fact: Object.freeze({
        ...fact,
        cause: Diagnostic.identity(diagnostic),
        ...(canonical === undefined ? {} : { itemCandidate: canonical }),
      }),
      diagnostics: Object.freeze([...argumentDiagnostics, diagnostic]),
    })
  }
  const unresolved = () =>
    Diagnostic.unresolvedExactRepresentationItem(fact.item.spelling, fact.token.span)
  const open = (expected: number, actual = arguments_.length) =>
    Diagnostic.openExactRepresentationItem(fact.item.spelling, expected, actual, fact.token.span)
  const lookup = resolvers.item(module, fact.item)
  if (lookup._tag === 'Ambiguous')
    return reject(
      Diagnostic.ambiguousExactRepresentationItem(
        fact.item.spelling,
        lookup.count,
        fact.token.span,
      ),
    )
  if (lookup._tag !== 'Resolved')
    return reject(
      unresolved(),
      lookup._tag === 'Inaccessible' || lookup._tag === 'Unavailable'
        ? lookup.declaration
        : undefined,
    )
  const declaration = lookup.declaration
  if (declaration._tag !== 'FunctionDeclaration' || declaration.functionKind !== 'Ordinary')
    return reject(
      Diagnostic.uncallableExactRepresentationItem(
        fact.item.spelling,
        declaration._tag === 'FunctionDeclaration' ? 'EffectDeclaration' : 'NonCallableDeclaration',
        fact.token.span,
      ),
      declaration,
    )
  if (declaration.typeParameters.length !== arguments_.length)
    return reject(open(declaration.typeParameters.length), declaration)
  const supplied = arguments_.map((argument, ordinal) =>
    argument.fact._tag === 'Resolved'
      ? genericArgumentForParameter(
          declaration.typeParameters.at(ordinal)?.type,
          argument.fact.type,
        )
      : undefined,
  )
  if (supplied.some((argument) => argument === undefined))
    return reject(open(declaration.typeParameters.length), declaration)
  const concrete = supplied.filter(
    (argument): argument is Type.GenericArgument => argument !== undefined,
  )
  const concreteCount = concrete.filter(Type.isRuntimeConcreteGenericArgument).length
  if (concreteCount !== concrete.length)
    return reject(open(declaration.typeParameters.length, concreteCount), declaration)
  const substitution = TypeInference.substitution(
    declaration.typeParameters.map((parameter) => parameter.type),
    concrete,
  )
  if (substitution === undefined)
    return reject(open(declaration.typeParameters.length), declaration)
  const canonical =
    declaration.canonical._tag === 'Canonical' ? declaration.canonical.id : undefined
  if (canonical === undefined) return reject(unresolved(), declaration)
  const declaredReturn = resolveDeclaredType(
    canonical.module,
    declaration.returnType,
    resolvers,
    modules,
  )
  if (declaredReturn.fact._tag !== 'Resolved') return reject(unresolved(), declaration)
  const declaredParameters = declaration.parameters.map(
    (parameter) =>
      resolveDeclaredType(canonical.module, parameter.declaredType, resolvers, modules).fact,
  )
  if (declaredParameters.some((parameter) => parameter._tag !== 'Resolved'))
    return reject(unresolved(), declaration)
  const structural = Type.callable(
    declaredParameters.flatMap((parameter) =>
      parameter._tag === 'Resolved' ? [Type.substitute(parameter.type, substitution)] : [],
    ),
    Type.substitute(declaredReturn.fact.type, substitution),
    'Shared',
    undefined,
    declaration.unsafe,
  )
  const identity = Type.callableIdentityArgument(
    `declaration:${canonical.module}:${canonical.name}`,
    Object.freeze({ _tag: 'Declaration', module: canonical.module, name: canonical.name }),
    concrete,
  )
  const type = Type.represented(
    structural,
    structural,
    Type.exactRepresentationArgument(identity, structural),
  )
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Resolved',
      type,
      spelling: fact.spelling,
      token: fact.token,
      syntax: fact.syntax,
      components: Object.freeze(arguments_.map((argument) => argument.fact)),
      exactItem: Object.freeze({ path: fact.item, declaration: canonical }),
    }),
    diagnostics: Object.freeze(argumentDiagnostics),
  })
}

export const resolveDeclaredType = (
  module: string,
  fact: DeclaredTypeFact,
  resolvers: ResolutionSeams.ResolutionSeams,
  modules: ReadonlyArray<ModuleHeaders>,
): TypeResolution => {
  if (fact._tag === 'RepresentationParameter') {
    const parameter =
      resolvers.representationBindings?.get(Type.key(fact.parameter)) ?? fact.parameter
    const bound = parameter.representationBound
    if (bound === undefined) return Object.freeze({ fact, diagnostics: Object.freeze([]) })
    const type = Type.represented(bound, bound, Type.representationParameterArgument(parameter))
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Resolved',
        type,
        spelling: fact.spelling,
        token: fact.token,
        syntax: fact.syntax,
        path: fact.path,
      }),
      diagnostics: Object.freeze([]),
    })
  }
  if (fact._tag === 'ExactRepresentation')
    return resolveExactRepresentation(module, fact, resolvers, modules)
  if (fact._tag === 'Unresolved') {
    const resolved = resolvers.type(module, fact.path)
    if (resolved.fact._tag !== 'Resolved' || !Type.isNominal(resolved.fact.type)) return resolved
    const declaration = memberByNominal(modules, resolved.fact.type)
    const expected =
      declaration?.typeParameters.length ?? Type.intrinsicNominalArity(resolved.fact.type)
    if (expected === 0) return resolved
    const diagnostic = Diagnostic.typeArgumentArity(fact.spelling, expected, 0, fact.token.span)
    return Object.freeze({
      fact: Object.freeze({
        ...fact,
        cause: Diagnostic.identity(diagnostic),
        candidate: resolved.fact.type,
      }),
      diagnostics: Object.freeze([diagnostic]),
    })
  }
  if (fact._tag === 'Callable') {
    const parameters = fact.parameters.map((parameter) =>
      resolveDeclaredType(module, parameter, resolvers, modules),
    )
    const result = resolveDeclaredType(module, fact.result, resolvers, modules)
    const diagnostics = Object.freeze([
      ...parameters.flatMap((parameter) => parameter.diagnostics),
      ...result.diagnostics,
    ])
    if (
      result.fact._tag === 'Resolved' &&
      parameters.every((parameter) => parameter.fact._tag === 'Resolved')
    ) {
      const type = Type.callable(
        parameters.flatMap((parameter) =>
          parameter.fact._tag === 'Resolved' ? [parameter.fact.type] : [],
        ),
        result.fact.type,
        fact.mode,
        undefined,
        fact.unsafe,
      )
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Resolved',
          type,
          spelling: Type.encode(type),
          token: fact.token,
          syntax: fact.syntax,
          components: Object.freeze([
            ...parameters.map((parameter) => parameter.fact),
            result.fact,
          ]),
        }),
        diagnostics,
      })
    }
    const resolvedFacts = [...parameters.map((parameter) => parameter.fact), result.fact]
    const cause = resolvedFacts
      .flatMap((resolved) =>
        'cause' in resolved && resolved.cause !== undefined ? [resolved.cause] : [],
      )
      .at(-1)
    return Object.freeze({
      fact: Object.freeze({
        ...fact,
        parameters: Object.freeze(parameters.map((parameter) => parameter.fact)),
        result: result.fact,
        ...(cause === undefined ? {} : { cause }),
      }),
      diagnostics,
    })
  }
  if (fact._tag === 'Effect') {
    const success = resolveDeclaredType(module, fact.success, resolvers, modules)
    const failures = fact.failures.map((failure) =>
      resolveDeclaredType(module, failure, resolvers, modules),
    )
    const requirements = fact.requirements.map((requirement) => {
      const role = resolveRequirementRole(module, requirement.role, resolvers)
      return Object.freeze({
        ...requirement,
        capability: resolveDeclaredType(module, requirement.capability, resolvers, modules),
        role,
      })
    })
    const diagnostics: Array<Diagnostic.Diagnostic> = [
      ...success.diagnostics,
      ...failures.flatMap((failure) => failure.diagnostics),
      ...requirements.flatMap((requirement) => requirement.capability.diagnostics),
      ...requirements.flatMap((requirement) => requirement.role.diagnostics),
    ]
    const failureTypes: Array<Type.Type> = []
    const symbolicFailureTypes: Array<{
      readonly type: Type.Parameter
      readonly span: SourceSpan.SourceSpan
    }> = []
    let failuresAvailable = true
    for (const failure of failures) {
      if (
        failure.fact._tag === 'Resolved' &&
        Type.isTypeArgument(failure.fact.type) &&
        !Type.isParameter(failure.fact.type)
      ) {
        failureTypes.push(failure.fact.type)
      } else if (
        failure.fact._tag === 'Resolved' &&
        Type.isParameter(failure.fact.type) &&
        failure.fact.type.kind === 'Value'
      ) {
        symbolicFailureTypes.push({ type: failure.fact.type, span: failure.fact.syntax.span })
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
        requirementRoleIdentity(requirement.role.fact) !== undefined &&
        (Type.isNominal(requirement.capability.fact.type) ||
          (Type.isParameter(requirement.capability.fact.type) &&
            requirement.capability.fact.type.kind === 'Value'))
      ) {
        requirementTypes.push(
          Object.freeze({
            capability: requirement.capability.fact.type,
            role: requirementRoleIdentity(requirement.role.fact) ?? RequirementRow.defaultRole,
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
      const base = Type.effect(
        success.fact.type,
        failureTypes,
        fact.access,
        requirementTypes,
        fact.requirementParameters,
      )
      const failureRow = symbolicFailureTypes.reduce<Type.FailureRow>(
        (row, failure) =>
          RowAlgebra.union(
            Type.failureRowPolicy(),
            row,
            RowAlgebra.singleton(
              Type.failureRowPolicy(),
              Type.failureMemberShape(failure.type),
              failure.span,
            ),
          ),
        base.failureRow,
      )
      const type = Type.effectWithRows(
        success.fact.type,
        failureRow,
        fact.access,
        base.requirementRow,
      )
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Resolved',
          type,
          spelling: Type.encode(type),
          token: fact.token,
          syntax: fact.syntax,
          components: Object.freeze([
            success.fact,
            ...failures.map((failure) => failure.fact),
            ...requirements.map((requirement) => requirement.capability.fact),
          ]),
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
            Object.freeze({
              ...requirement,
              capability: requirement.capability.fact,
              role: requirement.role.fact,
            }),
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
        ? resolvers.type(module, fact.target.path)
        : resolveDeclaredType(module, fact.target, resolvers, modules)
    const arguments_ = fact.arguments.map((argument) =>
      resolveDeclaredType(module, argument, resolvers, modules),
    )
    const requirements =
      fact.requirementRow?.requirements.map((requirement) => {
        const role = resolveRequirementRole(module, requirement.role, resolvers)
        return Object.freeze({
          ...requirement,
          capability: resolveDeclaredType(module, requirement.capability, resolvers, modules),
          role,
        })
      }) ?? []
    const diagnostics = [
      ...target.diagnostics,
      ...arguments_.flatMap((argument) => argument.diagnostics),
      ...requirements.flatMap((requirement) => requirement.capability.diagnostics),
      ...requirements.flatMap((requirement) => requirement.role.diagnostics),
    ]
    if (target.fact._tag === 'Resolved' && Type.isNominal(target.fact.type)) {
      const declaration = memberByNominal(modules, target.fact.type)
      const expected =
        declaration?.typeParameters.length ?? Type.intrinsicNominalArity(target.fact.type)
      const declaredParameters = declaration?.typeParameters.map((parameter) => parameter.type)
      const valueArguments = arguments_.map(
        (argument, ordinal): Type.GenericArgument | undefined => {
          if (argument.fact._tag !== 'Resolved') return undefined
          return genericArgumentForParameter(declaredParameters?.at(ordinal), argument.fact.type)
        },
      )
      const requirementTypes = requirements.flatMap(
        (requirement): ReadonlyArray<Type.Requirement> =>
          requirement.capability.fact._tag === 'Resolved' &&
          (Type.isNominal(requirement.capability.fact.type) ||
            (Type.isParameter(requirement.capability.fact.type) &&
              requirement.capability.fact.type.kind === 'Value'))
            ? [
                Object.freeze({
                  capability: requirement.capability.fact.type,
                  role:
                    requirementRoleIdentity(requirement.role.fact) ?? RequirementRow.defaultRole,
                  access: requirement.access,
                }),
              ]
            : [],
      )
      const requirementsAvailable =
        requirementTypes.length === requirements.length &&
        requirements.every(
          (requirement) => requirementRoleIdentity(requirement.role.fact) !== undefined,
        )
      const rowArguments: ReadonlyArray<Type.GenericArgument | undefined> = Object.freeze([
        ...(fact.requirementRow === undefined
          ? []
          : [
              requirementsAvailable
                ? Type.requirementRowArgument(requirementTypes, fact.requirementRow.parameters)
                : undefined,
            ]),
      ])
      const available = Object.freeze([...valueArguments, ...rowArguments])
      const suppliedCount = available.length
      if (expected === suppliedCount && available.every((argument) => argument !== undefined)) {
        const concrete = available.filter(
          (argument): argument is Type.GenericArgument => argument !== undefined,
        )
        const substitution =
          declaredParameters === undefined
            ? concrete.every(Type.isTypeArgument)
              ? new Map<string, Type.GenericArgument>()
              : undefined
            : TypeInference.substitution(declaredParameters, concrete)
        if (substitution === undefined) {
          const incompatibleBound = concrete.findIndex((argument, ordinal) => {
            const parameter = declaredParameters?.at(ordinal)
            if (
              parameter === undefined ||
              (parameter.kind !== 'CallableRepresentation' &&
                parameter.kind !== 'EffectRepresentation') ||
              !Type.isRepresentationArgument(argument) ||
              Type.representationArgumentKind(argument) !== parameter.kind ||
              parameter.representationBound === undefined
            )
              return false
            const prior = TypeInference.prefixSubstitution(
              declaredParameters?.slice(0, ordinal) ?? [],
              concrete.slice(0, ordinal),
            )
            if (prior === undefined) return false
            const required = Type.substitute(parameter.representationBound, prior)
            const actual =
              argument._tag === 'RepresentationParameterArgument'
                ? argument.parameter.representationBound
                : argument.contract
            return (
              actual !== undefined &&
              (Type.isCallable(required) || Type.isEffect(required)) &&
              Type.representationAdmissibility(actual, required)._tag === 'Unavailable'
            )
          })
          const incompatibleParameter =
            incompatibleBound < 0 ? undefined : declaredParameters?.at(incompatibleBound)
          const incompatibleArgument =
            incompatibleBound < 0 ? undefined : concrete.at(incompatibleBound)
          const incompatibleSupplied =
            incompatibleBound < 0 ? undefined : arguments_.at(incompatibleBound)
          if (
            incompatibleParameter !== undefined &&
            incompatibleParameter.representationBound !== undefined &&
            incompatibleArgument !== undefined &&
            Type.isRepresentationArgument(incompatibleArgument) &&
            incompatibleSupplied !== undefined
          ) {
            const prior = TypeInference.prefixSubstitution(
              declaredParameters?.slice(0, incompatibleBound) ?? [],
              concrete.slice(0, incompatibleBound),
            )
            const required =
              prior === undefined
                ? incompatibleParameter.representationBound
                : Type.substitute(incompatibleParameter.representationBound, prior)
            const actual =
              incompatibleArgument._tag === 'RepresentationParameterArgument'
                ? incompatibleArgument.parameter.representationBound
                : incompatibleArgument.contract
            const actualParameter =
              incompatibleArgument._tag === 'RepresentationParameterArgument'
                ? modules
                    .flatMap((candidateModule) => candidateModule.members)
                    .flatMap((member) => ('typeParameters' in member ? member.typeParameters : []))
                    .find(
                      (candidateParameter) =>
                        Type.key(candidateParameter.type) ===
                        Type.key(incompatibleArgument.parameter),
                    )
                : undefined
            const requiredParameter = declaration?.typeParameters.at(incompatibleBound)
            if ((Type.isCallable(required) || Type.isEffect(required)) && actual !== undefined)
              diagnostics.push(
                Diagnostic.incompatibleRepresentationBound(
                  incompatibleParameter.name,
                  Type.encode(required),
                  Type.encode(actual),
                  incompatibleSupplied.fact.syntax.span,
                  {
                    ...(requiredParameter === undefined
                      ? {}
                      : { requiredDeclarationSpan: requiredParameter.syntax.span }),
                    ...(actualParameter === undefined
                      ? {}
                      : { actualDeclarationSpan: actualParameter.syntax.span }),
                  },
                ),
              )
          }
          const mismatch = concrete.findIndex((argument, ordinal) => {
            const parameter = declaredParameters?.at(ordinal)
            if (parameter === undefined) return false
            if (parameter.kind === 'Value') return !Type.isTypeArgument(argument)
            if (parameter.kind === 'RequirementRow') return !Type.isRequirementRowArgument(argument)
            return (
              !Type.isRepresentationArgument(argument) ||
              Type.representationArgumentKind(argument) !== parameter.kind
            )
          })
          const parameter = declaredParameters?.at(mismatch)
          const supplied = arguments_.at(mismatch)
          if (incompatibleBound < 0 && parameter !== undefined && supplied !== undefined) {
            diagnostics.push(
              Diagnostic.genericParameterKindMismatch(
                parameter.name,
                parameter.kind,
                supplied.fact._tag === 'Resolved' && Type.isRepresented(supplied.fact.type)
                  ? supplied.fact.type.contract._tag === 'CallableType'
                    ? 'CallableRepresentation'
                    : 'EffectRepresentation'
                  : supplied.fact._tag === 'Resolved' &&
                      Type.isParameter(supplied.fact.type) &&
                      supplied.fact.type.kind === 'RequirementRow'
                    ? supplied.fact.type.kind
                    : 'Value',
                supplied.fact.syntax.span,
              ),
            )
          }
          const causeDiagnostic = diagnostics.at(-1)
          return Object.freeze({
            fact: Object.freeze({
              ...fact,
              ...(causeDiagnostic === undefined
                ? {}
                : { cause: Diagnostic.identity(causeDiagnostic) }),
            }),
            diagnostics: Object.freeze(diagnostics),
          })
        }
        const firstConcrete = concrete.at(0)
        const type =
          target.fact.type.module === 'silk/option' &&
          target.fact.type.name === 'Option' &&
          concrete.length === 1 &&
          firstConcrete !== undefined &&
          Type.isTypeArgument(firstConcrete)
            ? Type.option(firstConcrete)
            : Type.specializeNominal(target.fact.type, concrete)
        return Object.freeze({
          fact: Object.freeze({
            _tag: 'Resolved',
            type,
            spelling: Type.encode(type),
            token: fact.token,
            syntax: fact.syntax,
            components: Object.freeze([
              target.fact,
              ...arguments_.map((argument) => argument.fact),
              ...requirements.map((requirement) => requirement.capability.fact),
            ]),
          }),
          diagnostics: Object.freeze(diagnostics),
        })
      }
      if (expected === suppliedCount) {
        const unavailable = [
          ...arguments_,
          ...requirements.map((requirement) => requirement.capability),
        ].find((argument) => argument.fact._tag !== 'Resolved')
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
        suppliedCount,
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
      resolveDeclaredType(module, member, resolvers, modules),
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
      if (normalized._tag === 'InvalidMembers') {
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
    const element = resolveDeclaredType(module, fact.element, resolvers, modules)
    if (element.fact._tag === 'Resolved') {
      const type = Type.slice(fact.access, element.fact.type)
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Resolved',
          type,
          spelling: Type.encode(type),
          token: fact.token,
          syntax: fact.syntax,
          components: Object.freeze([element.fact]),
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
  if (fact._tag === 'Reference') {
    const target = resolveDeclaredType(module, fact.target, resolvers, modules)
    if (target.fact._tag === 'Resolved') {
      const type = Type.reference(fact.access, target.fact.type)
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Resolved',
          type,
          spelling: Type.encode(type),
          token: fact.token,
          syntax: fact.syntax,
          components: Object.freeze([target.fact]),
        }),
        diagnostics: target.diagnostics,
      })
    }
    return Object.freeze({
      fact: Object.freeze({
        ...fact,
        target: target.fact,
        ...('cause' in target.fact && target.fact.cause !== undefined
          ? { cause: target.fact.cause }
          : {}),
      }),
      diagnostics: target.diagnostics,
    })
  }
  if (fact._tag !== 'FixedArray') return Object.freeze({ fact, diagnostics: Object.freeze([]) })
  return (() => {
    const element = resolveDeclaredType(module, fact.element, resolvers, modules)
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
          components: Object.freeze([element.fact]),
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

export const canonicalKey = (id: CanonicalId): string => `${id.module}.${id.name}`

export const memberByNominal = (
  modules: ReadonlyArray<ModuleHeaders>,
  type: Type.Nominal,
): StructFact | ServiceFact | InterfaceFact | undefined => {
  const module = modules.find((candidate) => candidate.module === type.module)
  return [
    ...(module?.structs ?? []),
    ...(module?.services ?? []),
    ...(module?.interfaces ?? []),
  ].find(
    (member) => member.canonical._tag === 'Canonical' && member.canonical.id.name === type.name,
  )
}

const dependencyEligible = (
  modules: ReadonlyArray<ModuleHeaders>,
  capability: Type.Nominal,
): boolean => {
  const member = memberByNominal(modules, capability)
  return (
    (member?._tag === 'InterfaceDeclaration' || member?._tag === 'ServiceDeclaration') &&
    member.dependencyEligible
  )
}

/** Converts one resolved source type to the erased argument kind its declaration parameter owns. */
const genericArgumentForParameter = (
  parameter: Type.Parameter | undefined,
  type: Type.Type,
): Type.GenericArgument => {
  if (parameter?.kind === 'CallableRepresentation' || parameter?.kind === 'EffectRepresentation')
    return Type.isRepresented(type) ? type.representation.argument : type
  if (parameter?.kind === 'RequirementRow') {
    if (Type.isParameter(type) && type.kind === 'RequirementRow')
      return Type.requirementRowArgument([], [type])
    if (Type.isNominal(type) || (Type.isParameter(type) && type.kind === 'Value'))
      return Type.requirementRowArgument([
        Object.freeze({ capability: type, role: 'DefaultRole', access: 'Shared' }),
      ])
  }
  return type
}

/**
 * Resolves every type parameter's bound to the interface its spelling names in the bounded
 * declaration's own module scope, recording that interface's ordered operation contract.
 *
 * The resolver's own diagnostics are deliberately dropped: a bound that names nothing, or names a
 * declaration that is not an interface, stays `UnresolvedBound` and is reported once at the
 * specialization that would have had to satisfy it, where the type argument is known.
 */
export const resolveBounds = (
  module: string,
  typeParameters: ReadonlyArray<TypeParameterFact>,
  resolvers: ResolutionSeams.ResolutionSeams,
  modules: ReadonlyArray<ModuleHeaders>,
  diagnostics: Array<Diagnostic.Diagnostic>,
): ReadonlyArray<TypeParameterFact> => {
  if (
    typeParameters.every(
      (parameter) => parameter.bounds.length === 0 && parameter.representationBound === undefined,
    )
  )
    return typeParameters
  return Object.freeze(
    typeParameters.map((parameter): TypeParameterFact => {
      const representation = parameter.representationBound
      if (representation !== undefined) {
        const resolved = resolveDeclaredType(module, representation.contract, resolvers, modules)
        diagnostics.push(...resolved.diagnostics)
        const contract =
          resolved.fact._tag === 'Resolved' &&
          (Type.isCallable(resolved.fact.type) || Type.isEffect(resolved.fact.type))
            ? resolved.fact.type
            : undefined
        return Object.freeze({
          ...parameter,
          type:
            contract === undefined
              ? parameter.type
              : Type.parameter(
                  parameter.type.owner,
                  parameter.type.ordinal,
                  parameter.type.name,
                  parameter.type.kind,
                  contract,
                ),
          representationBound: Object.freeze({
            ...representation,
            contract: resolved.fact,
          }),
        })
      }
      if (parameter.bounds.length === 0) return parameter
      const parameterName = parameter.name._tag === 'Present' ? parameter.name : undefined
      const boundResolvers: ResolutionSeams.ResolutionSeams =
        parameterName === undefined
          ? resolvers
          : Object.freeze({
              ...resolvers,
              type: (candidateModule: string, path: TypePathFact): TypeResolution =>
                path.segments.length === 1 && path.spelling === parameterName.spelling
                  ? Object.freeze({
                      fact: Object.freeze({
                        _tag: 'Resolved' as const,
                        type: parameter.type,
                        spelling: parameterName.spelling,
                        token: parameterName.token,
                        syntax: path.syntax,
                        components: Object.freeze([]),
                      }),
                      diagnostics: Object.freeze([]),
                    })
                  : resolvers.type(candidateModule, path),
            })
      const bounds = Object.freeze(
        parameter.bounds.map((bound): BoundFact => {
          const unresolvedCapability =
            bound._tag === 'ResolvedBound'
              ? bound.application.capability
              : (() => {
                  const resolved = resolveDeclaredType(
                    module,
                    bound.application,
                    boundResolvers,
                    modules,
                  ).fact
                  if (resolved._tag === 'Resolved' && Type.isNominal(resolved.type))
                    return resolved.type
                  return resolved._tag === 'Unresolved' &&
                    resolved.candidate !== undefined &&
                    Type.isNominal(resolved.candidate)
                    ? resolved.candidate
                    : undefined
                })()
          const declaration =
            unresolvedCapability === undefined
              ? undefined
              : memberByNominal(modules, unresolvedCapability)
          if (
            unresolvedCapability !== undefined &&
            Type.equals(unresolvedCapability, Type.copyCapability)
          )
            return Object.freeze({
              _tag: 'ResolvedBound' as const,
              spelling: bound.spelling,
              path: bound.path,
              application: copyApplication(parameter.type),
            })
          if (
            unresolvedCapability === undefined ||
            (declaration?._tag !== 'InterfaceDeclaration' &&
              declaration?._tag !== 'ServiceDeclaration') ||
            declaration.canonical._tag !== 'Canonical'
          )
            return bound
          const application = interfaceApplication(
            declaration,
            unresolvedCapability,
            parameter.type,
          )
          return application === undefined
            ? bound
            : Object.freeze({
                _tag: 'ResolvedBound' as const,
                spelling: bound.spelling,
                path: bound.path,
                application,
              })
        }),
      )
      const seen = new Set<string>()
      for (const bound of bounds) {
        if (bound._tag !== 'ResolvedBound') continue
        const key = Type.key(bound.application.capability)
        if (seen.has(key))
          diagnostics.push(
            Diagnostic.invalidConformance(
              `duplicate bound ${bound.spelling}`,
              bound.path.syntax.span,
            ),
          )
        else seen.add(key)
      }
      return Object.freeze({ ...parameter, bounds })
    }),
  )
}

/** Refreshes resolved bound applications once every interface header has its completed contracts. */
export const refreshInterfaceApplications = (
  typeParameters: ReadonlyArray<TypeParameterFact>,
  modules: ReadonlyArray<ModuleHeaders>,
): ReadonlyArray<TypeParameterFact> =>
  Object.freeze(
    typeParameters.map((parameter): TypeParameterFact => {
      return Object.freeze({
        ...parameter,
        bounds: Object.freeze(
          parameter.bounds.map((bound): BoundFact => {
            if (bound._tag !== 'ResolvedBound') return bound
            const declaration = memberByNominal(modules, bound.application.capability)
            if (
              declaration?._tag !== 'InterfaceDeclaration' &&
              declaration?._tag !== 'ServiceDeclaration'
            )
              return bound
            const application = interfaceApplication(
              declaration,
              bound.application.capability,
              parameter.type,
            )
            return application === undefined ? bound : Object.freeze({ ...bound, application })
          }),
        ),
      })
    }),
  )

/**
 * Reads one conformance's requirements as the interface applications proof search will follow.
 *
 * A requirement that resolved to something other than an applied interface, or that never stated
 * its provider, contributes nothing here: header validation reports it, and admitting it as a
 * descent step would let a damaged fact stand in for a proof obligation.
 */
export const declaredRequirements = (
  modules: ReadonlyArray<ModuleHeaders>,
  conformance: ConformanceFact,
): ReadonlyArray<ConformanceHead.Requirement> =>
  Object.freeze(
    conformance.requirements.flatMap((requirement): ReadonlyArray<ConformanceHead.Requirement> => {
      if (requirement.capability._tag !== 'Resolved') return []
      const capability = requirement.capability.type
      if (!Type.isNominal(capability)) return []
      if (
        !Type.equals(capability, Type.copyCapability) &&
        memberByNominal(modules, capability) === undefined
      )
        return []
      return Object.freeze([Object.freeze({ capability, provider: requirement.parameter })])
    }),
  )

/** Positional specialization shared by interface and service witness validation. */
export const witnessBinding = (
  implementation: DeclarationFact,
  declaredParameters: ReadonlyArray<Type.Parameter>,
): {
  readonly binders: ReadonlyArray<TypeParameterFact>
  readonly parameters: ReadonlyArray<Type.Parameter>
  readonly substitution: Type.Substitution | undefined
} => {
  const binders = implementation.typeParameters.filter(
    (parameter) => parameter.duplicateOf === undefined,
  )
  const parameters = binders.map((parameter) => parameter.type)
  return Object.freeze({
    binders,
    parameters,
    substitution:
      parameters.length === 0
        ? new Map<string, Type.GenericArgument>()
        : TypeInference.substitution(parameters, declaredParameters.map(Type.parameterArgument)),
  })
}

/** Infers an interface witness declaration's own binders without assuming header position. */
export const inferInterfaceWitnessTarget = (
  implementation: DeclarationFact,
  contract: InterfaceOperationApplicationFact | undefined,
): InterfaceWitnessInference.Inference | undefined => {
  if (
    contract === undefined ||
    implementation.parameters.length !== contract.operands.length ||
    implementation.returnType._tag !== 'Resolved'
  )
    return undefined
  const binders = implementation.typeParameters
    .filter((parameter) => parameter.duplicateOf === undefined)
    .map((parameter) => parameter.type)
  if (binders.length === 0)
    return Object.freeze({
      _tag: 'Inferred',
      arguments: Object.freeze([]),
      substitution: new Map<string, Type.GenericArgument>(),
    })
  const constraints: Array<InterfaceWitnessInference.Constraint> = []
  for (const [ordinal, operand] of contract.operands.entries()) {
    const pattern = implementation.parameters.at(ordinal)?.declaredType
    if (pattern?._tag !== 'Resolved' || operand.type._tag !== 'Resolved') return undefined
    const name =
      operand.parameter.name._tag === 'Present'
        ? operand.parameter.name.spelling
        : `#${ordinal + 1}`
    constraints.push(
      Object.freeze({
        label: Type.equals(
          Type.isReference(operand.type.type) ? operand.type.type.target : operand.type.type,
          contract.provider,
        )
          ? `receiver ${name}`
          : `parameter ${name}`,
        pattern: pattern.type,
        actual: operand.type.type,
      }),
    )
  }
  constraints.push(
    Object.freeze({
      label: 'success',
      pattern: implementation.returnType.type,
      actual: contract.success._tag === 'Resolved' ? contract.success.type : 'never',
    }),
  )
  const covered = new Set(
    constraints.flatMap((constraint) => Type.parameters(constraint.pattern).map(Type.key)),
  )
  if (binders.some((binder) => !covered.has(Type.key(binder))))
    constraints.push(
      Object.freeze({
        label: 'failure and requirement rows',
        pattern: Type.effectWithRows(
          Type.unit,
          implementation.failureRow.row,
          'Shared',
          implementation.requirementRow.row,
        ),
        actual: Type.effectWithRows(
          Type.unit,
          contract.failureRow.row,
          'Shared',
          contract.requirementRow.row,
        ),
      }),
    )
  return InterfaceWitnessInference.infer(binders, constraints)
}

const compatibilityOperand = (
  parameter: ParameterFact,
  type: Type.Type,
  provider: Type.Type,
): InterfaceWitnessCompatibility.Operand =>
  Object.freeze({
    name: parameter.name._tag === 'Present' ? parameter.name.spelling : '_',
    type,
    receiver: Type.equals(Type.isReference(type) ? type.target : type, provider),
  })

/** Checks one source witness against the complete applied interface contract it will implement. */
export const interfaceWitnessCompatibility = (
  contract: InterfaceOperationApplicationFact | undefined,
  implementation: DeclarationFact,
  substitution: Type.Substitution,
): InterfaceWitnessCompatibility.Compatibility | undefined => {
  if (contract === undefined || contract.success._tag !== 'Resolved') return undefined
  const contractOperands = contract.operands.flatMap((operand) =>
    operand.type._tag === 'Resolved'
      ? [compatibilityOperand(operand.parameter, operand.type.type, contract.provider)]
      : [],
  )
  const witnessOperands = implementation.parameters.flatMap((parameter) =>
    parameter.declaredType._tag === 'Resolved'
      ? [
          compatibilityOperand(
            parameter,
            Type.substitute(parameter.declaredType.type, substitution),
            contract.provider,
          ),
        ]
      : [],
  )
  if (
    contractOperands.length !== contract.operands.length ||
    witnessOperands.length !== implementation.parameters.length ||
    implementation.returnType._tag !== 'Resolved'
  )
    return undefined
  const witnessRows = Type.substitute(
    Type.effectWithRows(
      Type.unit,
      implementation.failureRow.row,
      'Shared',
      implementation.requirementRow.row,
    ),
    substitution,
  )
  if (!Type.isEffect(witnessRows)) return undefined
  return InterfaceWitnessCompatibility.check(
    Object.freeze({
      functionKind: contract.functionKind,
      unsafe: contract.unsafe,
      operands: Object.freeze(contractOperands),
      success: contract.success.type,
      failures: Object.freeze([
        ...contract.failureRow.failures,
        ...Type.failureMemberParameters(contract.failureRow.row),
      ]),
      requirements: contract.requirementRow.requirements,
      requirementParameters: contract.requirementRow.parameters,
    }),
    Object.freeze({
      functionKind: implementation.functionKind,
      unsafe: implementation.unsafe,
      operands: Object.freeze(witnessOperands),
      success: Type.substitute(implementation.returnType.type, substitution),
      failures: Object.freeze([
        ...Type.failureMembers(witnessRows),
        ...Type.failureMemberParameters(witnessRows),
      ]),
      requirements: Type.requirementMembers(witnessRows),
      requirementParameters: Type.requirementRowParameters(witnessRows),
    }),
  )
}

/** Checks a sealed witness against the interface's literal operand ownership contract. */
export const sealedWitnessCompatibility = (
  contract: InterfaceOperationApplicationFact | undefined,
  parameters: ReadonlyArray<Type.Type>,
  result: Type.Type,
): InterfaceWitnessCompatibility.Compatibility | undefined => {
  if (contract === undefined || contract.success._tag !== 'Resolved') return undefined
  const operands = contract.operands.flatMap((operand) =>
    operand.type._tag === 'Resolved'
      ? [compatibilityOperand(operand.parameter, operand.type.type, contract.provider)]
      : [],
  )
  if (operands.length !== contract.operands.length) return undefined
  return InterfaceWitnessCompatibility.check(
    Object.freeze({
      functionKind: contract.functionKind,
      unsafe: contract.unsafe,
      operands: Object.freeze(operands),
      success: contract.success.type,
      failures: Object.freeze([
        ...contract.failureRow.failures,
        ...Type.failureMemberParameters(contract.failureRow.row),
      ]),
      requirements: contract.requirementRow.requirements,
      requirementParameters: contract.requirementRow.parameters,
    }),
    Object.freeze({
      functionKind: 'Ordinary',
      unsafe: false,
      operands: Object.freeze(
        parameters.map((type, ordinal) =>
          Object.freeze({ name: operands.at(ordinal)?.name ?? '_', type, receiver: false }),
        ),
      ),
      success: result,
      failures: Object.freeze([]),
      requirements: Object.freeze([]),
      requirementParameters: Object.freeze([]),
    }),
  )
}

/** Finds the first implementation bound the conformance header never promises. */
export const unpromisedWitnessBound = (
  binding: ReturnType<typeof witnessBinding>,
  arguments_: ReadonlyArray<Type.GenericArgument>,
  conformance: ConformanceFact,
): { readonly binder: TypeParameterFact; readonly bound: BoundFact } | undefined => {
  for (const [position, binder] of binding.binders.entries()) {
    const argument = arguments_.at(position)
    const header =
      argument !== undefined && Type.isTypeArgument(argument) && Type.isParameter(argument)
        ? conformance.typeParameters.find((parameter) => Type.equals(parameter.type, argument))
            ?.type
        : undefined
    for (const bound of binder.bounds) {
      if (bound._tag !== 'ResolvedBound') return { binder, bound }
      if (
        header === undefined ||
        !conformance.requirements.some(
          (requirement) =>
            requirement.capability._tag === 'Resolved' &&
            Type.isNominal(requirement.capability.type) &&
            requirement.capability.type.module === bound.application.capability.module &&
            requirement.capability.type.name === bound.application.capability.name &&
            Type.equals(requirement.parameter, header),
        )
      )
        return { binder, bound }
    }
  }
  return undefined
}

/** Resolves one retained type fact through a supplied module resolver and complete index. */
export const resolveTypeFact = (
  index: Index,
  module: string,
  fact: DeclaredTypeFact,
  resolver: TypeResolver,
): TypeResolution =>
  resolveDeclaredType(
    module,
    fact,
    ResolutionSeams.make(resolver, () => Object.freeze({ _tag: 'Missing' })),
    index.modules,
  )

const resolveRequirementRole = (
  module: string,
  role: RequirementRoleFact,
  resolvers: ResolutionSeams.ResolutionSeams,
): {
  readonly fact: RequirementRoleFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  if (role._tag !== 'UnresolvedRole')
    return Object.freeze({ fact: role, diagnostics: Object.freeze([]) })
  const resolution = resolvers.item(module, role.path)
  const declaration =
    resolution._tag === 'Resolved' || resolution._tag === 'Inaccessible'
      ? resolution.declaration
      : resolution._tag === 'Unavailable'
        ? resolution.declaration
        : undefined
  if (
    resolution._tag === 'Resolved' &&
    declaration?._tag === 'RoleDeclaration' &&
    declaration.canonical._tag === 'Canonical'
  )
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'ResolvedRole',
        role: RequirementRow.declaredRole(
          declaration.canonical.id.module,
          declaration.canonical.id.name,
        ),
        path: role.path,
        declaration: declaration.canonical.id,
      }),
      diagnostics: Object.freeze([]),
    })
  return Object.freeze({
    fact: role,
    diagnostics: Object.freeze([
      Diagnostic.invalidRequirementType(`role ${role.path.spelling}`, role.path.syntax.span),
    ]),
  })
}

const resolveRowExpressionFact = (
  module: string,
  fact: RowExpressionFact,
  resolvers: ResolutionSeams.ResolutionSeams,
  modules: ReadonlyArray<ModuleHeaders>,
): {
  readonly fact: RowExpressionFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  switch (fact._tag) {
    case 'EmptyRowExpression':
    case 'RowParameterExpression':
    case 'UnavailableRowExpression':
      return Object.freeze({ fact, diagnostics: Object.freeze([]) })
    case 'FailureMemberExpression': {
      const member = resolveDeclaredType(module, fact.member, resolvers, modules)
      return Object.freeze({
        fact: Object.freeze({ ...fact, member: member.fact }),
        diagnostics: member.diagnostics,
      })
    }
    case 'RequirementMemberExpression': {
      const capability = resolveDeclaredType(module, fact.capability, resolvers, modules)
      const role = resolveRequirementRole(module, fact.role, resolvers)
      return Object.freeze({
        fact: Object.freeze({ ...fact, capability: capability.fact, role: role.fact }),
        diagnostics: Object.freeze([...capability.diagnostics, ...role.diagnostics]),
      })
    }
    case 'UnionRowExpression': {
      const operands = fact.operands.map((operand) =>
        resolveRowExpressionFact(module, operand, resolvers, modules),
      )
      return Object.freeze({
        fact: Object.freeze({
          ...fact,
          operands: Object.freeze(operands.map((operand) => operand.fact)),
        }),
        diagnostics: Object.freeze(operands.flatMap((operand) => operand.diagnostics)),
      })
    }
    case 'WithoutRowExpression': {
      const source = resolveRowExpressionFact(module, fact.source, resolvers, modules)
      const selected = resolveRowExpressionFact(module, fact.selected, resolvers, modules)
      return Object.freeze({
        fact: Object.freeze({ ...fact, source: source.fact, selected: selected.fact }),
        diagnostics: Object.freeze([...source.diagnostics, ...selected.diagnostics]),
      })
    }
  }
}

export const resolveConstraintFacts = (
  module: string,
  constraints: ReadonlyArray<ConstraintFact>,
  resolvers: ResolutionSeams.ResolutionSeams,
  modules: ReadonlyArray<ModuleHeaders>,
): {
  readonly facts: ReadonlyArray<ConstraintFact>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const facts = constraints.map((constraint): ConstraintFact => {
    const selected = resolveRowExpressionFact(module, constraint.selected, resolvers, modules)
    const source = resolveRowExpressionFact(module, constraint.source, resolvers, modules)
    diagnostics.push(...selected.diagnostics, ...source.diagnostics)
    if (constraint._tag === 'MembershipConstraint')
      return Object.freeze({ ...constraint, selected: selected.fact, source: source.fact })
    const provider = resolveDeclaredType(module, constraint.provider, resolvers, modules)
    diagnostics.push(...provider.diagnostics)
    return Object.freeze({
      ...constraint,
      provider: provider.fact,
      selected: selected.fact,
      source: source.fact,
    })
  })
  return Object.freeze({ facts: Object.freeze(facts), diagnostics: Object.freeze(diagnostics) })
}

const semanticFailureRow = (fact: RowExpressionFact): Type.FailureRow => {
  switch (fact._tag) {
    case 'EmptyRowExpression':
    case 'RequirementMemberExpression':
    case 'UnavailableRowExpression':
      return RowAlgebra.concrete(Type.failureRowPolicy(), [])
    case 'RowParameterExpression':
      return RowAlgebra.concrete(Type.failureRowPolicy(), [])
    case 'FailureMemberExpression':
      if (fact.member._tag !== 'Resolved') return RowAlgebra.concrete(Type.failureRowPolicy(), [])
      if (Type.isParameter(fact.member.type) && fact.member.type.kind === 'Value')
        return RowAlgebra.singleton(
          Type.failureRowPolicy(),
          Type.failureMemberShape(fact.member.type),
          fact.syntax.span,
        )
      return Type.isRuntimeConcrete(fact.member.type)
        ? RowAlgebra.concrete(Type.failureRowPolicy(), [fact.member.type])
        : RowAlgebra.concrete(Type.failureRowPolicy(), [])
    case 'UnionRowExpression':
      return fact.operands.reduce<Type.FailureRow>(
        (row, operand) =>
          RowAlgebra.union(Type.failureRowPolicy(), row, semanticFailureRow(operand)),
        RowAlgebra.concrete(Type.failureRowPolicy(), []),
      )
    case 'WithoutRowExpression':
      return RowAlgebra.without(
        Type.failureRowPolicy(),
        semanticFailureRow(fact.source),
        semanticFailureRow(fact.selected),
      )
  }
}

const semanticRequirementRow = (fact: RowExpressionFact): Type.RequirementsRow => {
  switch (fact._tag) {
    case 'EmptyRowExpression':
    case 'FailureMemberExpression':
    case 'UnavailableRowExpression':
      return RowAlgebra.concrete(Type.requirementRowPolicy(), [])
    case 'RowParameterExpression':
      return fact.parameter.kind === 'RequirementRow'
        ? RowAlgebra.parameter<Type.Requirement, Type.Parameter, Type.RequirementMemberShape>(
            fact.parameter,
          )
        : RowAlgebra.concrete(Type.requirementRowPolicy(), [])
    case 'RequirementMemberExpression': {
      if (fact.capability._tag !== 'Resolved')
        return RowAlgebra.concrete(Type.requirementRowPolicy(), [])
      const role = requirementRoleIdentity(fact.role)
      if (role === undefined) return RowAlgebra.concrete(Type.requirementRowPolicy(), [])
      if (Type.isNominal(fact.capability.type))
        return RowAlgebra.concrete(Type.requirementRowPolicy(), [
          Object.freeze({
            capability: fact.capability.type,
            access: fact.access,
            role,
          }),
        ])
      if (Type.isParameter(fact.capability.type) && fact.capability.type.kind === 'Value')
        return RowAlgebra.singleton(
          Type.requirementRowPolicy(),
          Type.requirementMemberShape(fact.capability.type, fact.access, role),
          fact.syntax.span,
        )
      return RowAlgebra.concrete(Type.requirementRowPolicy(), [])
    }
    case 'UnionRowExpression':
      return fact.operands.reduce<Type.RequirementsRow>(
        (row, operand) =>
          RowAlgebra.union(Type.requirementRowPolicy(), row, semanticRequirementRow(operand)),
        RowAlgebra.concrete(Type.requirementRowPolicy(), []),
      )
    case 'WithoutRowExpression':
      return RowAlgebra.without(
        Type.requirementRowPolicy(),
        semanticRequirementRow(fact.source),
        semanticRequirementRow(fact.selected),
      )
  }
}

export const semanticConstraints = (
  constraints: ReadonlyArray<ConstraintFact>,
): ReadonlyArray<Constraint.Constraint> =>
  Object.freeze(
    constraints.flatMap((constraint): ReadonlyArray<Constraint.Constraint> => {
      if (constraint._tag === 'ProviderConstraint') {
        if (constraint.provider._tag !== 'Resolved') return []
        const provider = Type.isReference(constraint.provider.type)
          ? constraint.provider.type.target
          : constraint.provider.type
        return [
          Constraint.providerSelection(
            constraint.mode,
            provider,
            semanticRequirementRow(constraint.selected),
            semanticRequirementRow(constraint.source),
          ),
        ]
      }
      if (constraint.domain === 'Requirement')
        return [
          Constraint.requirementSubset(
            semanticRequirementRow(constraint.selected),
            semanticRequirementRow(constraint.source),
          ),
        ]
      return [
        Constraint.failureSubset(
          semanticFailureRow(constraint.selected),
          semanticFailureRow(constraint.source),
        ),
      ]
    }),
  )

export const resolveFailureRow = (
  module: string,
  row: FailureRowFact,
  resolvers: ResolutionSeams.ResolutionSeams,
  modules: ReadonlyArray<ModuleHeaders>,
): {
  readonly fact: FailureRowFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  if (row.syntax === undefined) return Object.freeze({ fact: row, diagnostics: Object.freeze([]) })
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const expression = resolveRowExpressionFact(module, row.expression, resolvers, modules)
  // Legacy member facts and the symbolic expression share the same source nodes. Resolve the
  // expression for semantic shape, while the member pass below remains the single diagnostic owner.
  const members = row.members.map((member) => {
    const resolved = resolveDeclaredType(module, member, resolvers, modules)
    diagnostics.push(...resolved.diagnostics)
    return resolved.fact
  })
  const failures = new Map<string, Type.Type>()
  let available = row.parameters.length === 0
  for (const member of members) {
    if (
      member._tag !== 'Resolved' ||
      !(
        Type.isRuntimeConcrete(member.type) ||
        (Type.isParameter(member.type) && member.type.kind === 'Value')
      )
    ) {
      available = false
      if (member._tag === 'Resolved')
        diagnostics.push(
          Diagnostic.invalidFailureType(Type.encode(member.type), member.syntax.span),
        )
      continue
    }
    if (!Type.isParameter(member.type)) failures.set(Type.key(member.type), member.type)
  }
  return Object.freeze({
    fact: Object.freeze({
      ...row,
      members: Object.freeze(members),
      failures: Object.freeze([...failures.values()].sort(Type.compare)),
      expression: expression.fact,
      row: semanticFailureRow(expression.fact),
      available,
    }),
    diagnostics: Object.freeze(diagnostics),
  })
}

export const resolveRequirementRow = (
  module: string,
  row: RequirementRowFact,
  resolvers: ResolutionSeams.ResolutionSeams,
  modules: ReadonlyArray<ModuleHeaders>,
): {
  readonly fact: RequirementRowFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  if (row.syntax === undefined) return Object.freeze({ fact: row, diagnostics: Object.freeze([]) })
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const expression = resolveRowExpressionFact(module, row.expression, resolvers, modules)
  // The entry pass below owns diagnostics for these same source nodes.
  const entries = row.entries.map((entry) => {
    const capability = resolveDeclaredType(module, entry.capability, resolvers, modules)
    const role = resolveRequirementRole(module, entry.role, resolvers)
    diagnostics.push(...capability.diagnostics, ...role.diagnostics)
    return Object.freeze({ ...entry, capability: capability.fact, role: role.fact })
  })
  const requirements: Array<Type.Requirement> = []
  let available = row.parameters.length === 0
  for (const entry of entries) {
    if (
      entry.capability._tag === 'Resolved' &&
      requirementRoleIdentity(entry.role) !== undefined &&
      ((Type.isNominal(entry.capability.type) &&
        dependencyEligible(modules, entry.capability.type)) ||
        (Type.isParameter(entry.capability.type) && entry.capability.type.kind === 'Value'))
    ) {
      requirements.push(
        Object.freeze({
          capability: entry.capability.type,
          role: requirementRoleIdentity(entry.role) ?? RequirementRow.defaultRole,
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
  const normalized = Type.requirementMembers(Type.effect('never', [], 'Shared', requirements))
  return Object.freeze({
    fact: Object.freeze({
      ...row,
      entries: Object.freeze(entries),
      requirements: normalized,
      expression: expression.fact,
      row: semanticRequirementRow(expression.fact),
      available,
    }),
    diagnostics: Object.freeze(diagnostics),
  })
}

export const attachExposure = (
  fact: DeclaredTypeFact,
  modules: ReadonlyArray<ModuleHeaders>,
  diagnostics: Array<Diagnostic.Diagnostic>,
): DeclaredTypeFact => {
  if (fact._tag !== 'Resolved') return fact
  // An exact representation names a callable declaration rather than a nominal, so the private
  // leak it can create is invisible to the nominal walk below and is reported on its own terms.
  const leaked = Type.exactRepresentationDeclarations(fact.type).find((target) => {
    const owner = modules.find((candidate) => candidate.module === target.module)
    const found = lookupDeclaration(owner?.declarations ?? [], target.name)
    return found._tag === 'Resolved' && found.declaration.visibility === 'Private'
  })
  if (leaked !== undefined) {
    const diagnostic = Diagnostic.privateExactRepresentationLeak(leaked.name, fact.token.span)
    diagnostics.push(diagnostic)
    return Object.freeze({ ...fact, exposureCause: Diagnostic.identity(diagnostic) })
  }
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

/**
 * The type parameters each canonical struct reaches inline, keyed by canonical struct key. A
 * parameter absent from a struct's set is one the struct only ever holds behind an indirection.
 * A struct missing from the map is one this index cannot see, and its arguments are all treated
 * as inline.
 */
type InlineParameters = ReadonlyMap<string, ReadonlySet<number>>

/**
 * Walks the nominals and parameters whose layout one type's layout actually requires.
 *
 * A struct embeds its fields, so a field reaches everything its type names outside an indirecting
 * position. `RawBuffer<T>` and `Slot<T>` are the compiler-owned indirections: their
 * representations are `{$allocation, count}` and `{$address}` whatever `T` is, so the walk names
 * them and stops rather than descending into the element. Every other nominal is entered only
 * through the arguments its own declaration reaches inline, which `inlineParameters` supplies.
 * Arrays, slices, references, callables, effects, and unions descend exactly as `Type.nominals`
 * does, so this graph is narrower than the reported dependency graph and never wider.
 */
const inlineReach = (
  self: Type.Type,
  inlineParameters: InlineParameters,
  visit: (reached: Type.Nominal | Type.Parameter) => void,
): void => {
  const descend = (type: Type.Type): void => {
    if (Type.isNominal(type)) {
      visit(type)
      if (Type.isRawBuffer(type) || Type.isSlot(type)) return
      const inline = inlineParameters.get(`${type.module}.${type.name}`)
      for (const [ordinal, argument] of type.arguments.entries())
        if ((inline === undefined || inline.has(ordinal)) && Type.isTypeArgument(argument))
          descend(argument)
      return
    }
    if (Type.isParameter(type)) {
      visit(type)
      return
    }
    if (Type.isFixedArray(type) || Type.isSlice(type)) {
      descend(type.element)
      return
    }
    if (Type.isReference(type)) {
      descend(type.target)
      return
    }
    if (Type.isCallable(type)) {
      for (const parameter of type.parameters) descend(parameter)
      descend(type.result)
      return
    }
    if (Type.isEffect(type)) {
      descend(type.success)
      for (const failure of Type.failureMembers(type)) descend(failure)
      for (const requirement of Type.requirementMembers(type)) descend(requirement.capability)
      return
    }
    if (Type.isUnion(type)) for (const member of type.members) descend(member)
  }
  descend(self)
}

/**
 * The monotone least fixed point of "struct `S` reaches its own parameter `i` inline".
 *
 * Every parameter starts indirected. A round marks a parameter inline as soon as one of the
 * struct's own fields reaches it under `inlineReach`, and marking a parameter can only open more
 * descents, so the sets only grow and the loop terminates. Because it is the least fixed point,
 * the answer does not depend on the order structs or modules arrive in.
 */
export const inlineParametersOf = (structs: ReadonlyArray<StructFact>): InlineParameters => {
  const declarations = new Map<string, StructFact>()
  for (const struct of structs)
    if (struct.canonical._tag === 'Canonical')
      declarations.set(canonicalKey(struct.canonical.id), struct)
  const inline = new Map<string, Set<number>>()
  for (const key of declarations.keys()) inline.set(key, new Set())
  for (let growing = true; growing; ) {
    growing = false
    for (const [key, struct] of declarations) {
      const reached = inline.get(key)
      if (reached === undefined || struct.typeParameters.length === 0) continue
      // Keyed by position, matching how `TypeInference.substitution` binds arguments to parameters.
      const own = new Map(
        struct.typeParameters.map(
          (parameter, position) => [Type.key(parameter.type), position] as const,
        ),
      )
      for (const field of struct.fields) {
        if (field.declaredType._tag !== 'Resolved') continue
        inlineReach(field.declaredType.type, inline, (member) => {
          if (!Type.isParameter(member)) return
          const ordinal = own.get(Type.key(member))
          if (ordinal === undefined || reached.has(ordinal)) return
          reached.add(ordinal)
          growing = true
        })
      }
    }
  }
  return inline
}

/** Names every canonical struct one field reaches inline, for cycle detection only. */
export const inlineNeighbors = (
  field: FieldFact,
  inlineParameters: InlineParameters,
): ReadonlyArray<string> => {
  if (field.declaredType._tag !== 'Resolved') return Object.freeze([])
  const reached: Array<string> = []
  inlineReach(field.declaredType.type, inlineParameters, (member) => {
    if (Type.isParameter(member)) return
    reached.push(`${member.module}.${member.name}`)
  })
  return Object.freeze(reached)
}

export const stronglyConnected = (
  structs: ReadonlyArray<StructFact>,
  inlineParameters: InlineParameters,
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
  return Object.freeze(
    Graph.stronglyConnected(byKey.keys(), (key) => {
      const struct = byKey.get(key)
      return (struct?.fields ?? [])
        .flatMap((field) => inlineNeighbors(field, inlineParameters))
        .filter((neighbor) => byKey.has(neighbor))
        .sort()
    }).map((component) =>
      Object.freeze(
        component
          .flatMap((memberKey) => {
            const member = byKey.get(memberKey)
            return member === undefined ? [] : [member]
          })
          .sort((left, right) => {
            if (left.canonical._tag !== 'Canonical' || right.canonical._tag !== 'Canonical')
              return 0
            return canonicalKey(left.canonical.id).localeCompare(canonicalKey(right.canonical.id))
          }),
      ),
    ),
  )
}

export const resolveOpaqueResult = (
  module: string,
  opaqueResult: OpaqueResultFact | undefined,
  resolvers: ResolutionSeams.ResolutionSeams,
  modules: ReadonlyArray<ModuleHeaders>,
  diagnostics: Array<Diagnostic.Diagnostic>,
): OpaqueResultFact | undefined => {
  if (opaqueResult === undefined) return undefined
  const binder = resolveBounds(module, [opaqueResult.binder], resolvers, modules, diagnostics).at(0)
  if (binder === undefined) return undefined
  if (binder.type.kind !== 'CallableRepresentation' && binder.type.kind !== 'EffectRepresentation')
    diagnostics.push(
      Diagnostic.invalidOpaqueResultBinder(
        binder.name._tag === 'Present' ? binder.name.spelling : binder.type.name,
        binder.type.kind,
        binder.syntax.span,
      ),
    )
  return Object.freeze({ ...opaqueResult, binder })
}

const opaqueEnclosingArgument = (parameter: Type.Parameter): Type.GenericArgument => {
  if (parameter.kind === 'RequirementRow') return Type.requirementRowArgument([], [parameter])
  if (parameter.kind === 'CallableRepresentation' || parameter.kind === 'EffectRepresentation')
    return Type.representationParameterArgument(parameter)
  return parameter
}

export const closeOpaqueReturnType = (
  fact: ReturnTypeFact,
  opaqueResult: OpaqueResultFact | undefined,
  enclosing: ReadonlyArray<TypeParameterFact>,
): { readonly fact: ReturnTypeFact; readonly opaqueResult?: OpaqueResultFact } => {
  const bound = opaqueResult?.binder.type.representationBound
  if (fact._tag !== 'Resolved' || opaqueResult === undefined || bound === undefined)
    return Object.freeze({ fact, ...(opaqueResult === undefined ? {} : { opaqueResult }) })
  const argument = Type.opaqueRepresentationArgument(
    opaqueResult.family,
    bound,
    enclosing.map((parameter) => opaqueEnclosingArgument(parameter.type)),
  )
  const closed = Type.substitute(
    fact.type,
    new Map([[Type.key(opaqueResult.binder.type), argument]]),
  )
  return Object.freeze({
    fact: Object.freeze({ ...fact, type: closed, spelling: Type.encode(closed) }),
    opaqueResult: Object.freeze({
      ...opaqueResult,
      publicSignature: Object.freeze({
        bound: Type.key(bound),
        result: Type.key(closed),
        enclosingKinds: Object.freeze(enclosing.map((parameter) => parameter.type.kind)),
      }),
    }),
  })
}

/** Resolves all retained type paths and validates public exposure and inline dependencies. */
