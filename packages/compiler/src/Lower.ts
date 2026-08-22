import * as CleanupPlan from './CleanupPlan.js'
import * as DeclarationIndex from './DeclarationIndex.js'
import * as Hir from './Hir.js'
import * as Instances from './Instances.js'
import type * as Layout from './Layout.js'
import type * as Match from './Match.js'
import * as Mir from './Mir.js'
import type * as OpaqueRealization from './OpaqueRealization.js'
import type * as Ownership from './Ownership.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Type from './Type.js'

/**
 * Lowering preserves source control as canonical acyclic regions. Repetition is represented by a
 * loop region plus lexical repeat/exit outcomes; backend-private CFGs are derived later.
 */

export const i32: Extract<Mir.Type, { readonly _tag: 'i32' }> = Object.freeze({ _tag: 'i32' })
export const usize: Extract<Mir.Type, { readonly _tag: 'usize' }> = Object.freeze({ _tag: 'usize' })
export const bool: Extract<Mir.Type, { readonly _tag: 'bool' }> = Object.freeze({ _tag: 'bool' })
export const character: Extract<Mir.Type, { readonly _tag: 'char' }> = Object.freeze({
  _tag: 'char',
})

export const isOsOperation = (
  operation: Hir.BuiltinOperation,
): operation is Extract<Hir.BuiltinOperation, `Os${string}`> => operation.startsWith('Os')

export const mirType = (
  type: Type.Type,
  substitution: Type.Substitution = new Map(),
): Mir.Type | undefined => {
  const specialized = Type.substitute(type, substitution)
  if (!Type.isRuntimeConcrete(specialized)) return undefined
  return typeof specialized === 'string'
    ? Type.isBuiltin(specialized)
      ? Object.freeze({ _tag: specialized })
      : Type.isString(specialized)
        ? Object.freeze({ _tag: 'String', type: specialized })
        : Type.isNever(specialized)
          ? Object.freeze({ _tag: 'Bottom', type: specialized })
          : undefined
    : Type.isNominal(specialized)
      ? Object.freeze({ _tag: 'Nominal', type: specialized })
      : Type.isFixedArray(specialized)
        ? Object.freeze({ _tag: 'FixedArray', type: specialized })
        : Type.isSlice(specialized)
          ? Object.freeze({ _tag: 'Slice', type: specialized })
          : Type.isReference(specialized)
            ? Object.freeze({ _tag: 'Reference', type: specialized })
            : Type.isUnion(specialized)
              ? Object.freeze({ _tag: 'Union', type: specialized })
              : Type.isEffect(specialized)
                ? Object.freeze({ _tag: 'EffectOutcome', type: specialized })
                : undefined
}

export const local = (ordinal: number): Mir.LocalId => Object.freeze({ _tag: 'Local', ordinal })

export const spanKey = (span: SourceSpan.SourceSpan): string => `${span.start}:${span.end}`
export const patternKey = (binding: Match.BindingId): string =>
  `${spanKey(binding.arm.match.span)}:${binding.arm.ordinal}:${binding.ordinal}`
export const borrowKey = (borrow: Hir.BorrowId): string =>
  `${borrow.function.sourceId}:${borrow.function.ordinal}:${borrow.callSpan.start}:${borrow.callSpan.end}:${borrow.ordinal}`

export interface ProvidedRequirement {
  readonly capability: Type.Nominal
  readonly providerType: Type.Nominal
  readonly witness: DeclarationIndex.ConformanceWitness
  readonly role: string
  readonly requirementAccess: Type.Requirement['access']
  readonly access: Type.CallableMode
  readonly local?: Mir.LocalId
}

export type ExecutableEffectType = Extract<
  Mir.Type,
  { readonly _tag: 'EffectValue' | 'EffectComposite' }
>

export const specializeProvider = (
  fn: FunctionLowering,
  provider: Extract<Hir.Expression, { readonly _tag: 'EffectBindRequirement' }>['provider'],
): ProvidedRequirement | undefined => {
  const proof = Instances.requirementSelection(fn.owner, provider)
  if (proof === undefined) return undefined
  const selected = proof.selected
  const capability = selected.capability
  const providerType = proof.provider
  if (capability === undefined || !Type.isNominal(capability) || !Type.isNominal(providerType))
    return undefined
  const witness = provider.witness ?? DeclarationIndex.witness(fn.index, providerType, capability)
  if (witness === undefined) return undefined
  return Object.freeze({
    capability,
    providerType,
    witness,
    role: selected.role,
    requirementAccess: selected.access,
    access: provider.selectionAccess,
  })
}

import type { FunctionLowering } from './FunctionLowering.js'
export interface DelayedEffectState {
  readonly recipes: ReadonlyMap<number, Hir.Expression>
  readonly loanEnds: ReadonlyMap<number, ReadonlyArray<Hir.BorrowId>>
  readonly loanLocals: ReadonlyMap<string, Mir.LocalId>
}

import { generated } from './CleanupEmission.js'
import {
  lowerCatchEffectRunner,
  lowerEffectRunner,
  lowerInstance,
  lowerWitnessEffectRunner,
  returnedEffectBlock,
} from './EntryAssembly.js'
import type {} from './Forwarding.js'
import type { GeneratedEffectRunner } from './ValueType.js'
import {
  baseRunnerKey,
  effectEntryAdapterId,
  effectValueType,
  instanceText,
  representedValueType,
  unitEntryAdapterId,
} from './ValueType.js'
export const lowerProgram = (
  discovery: Instances.Discovery,
  ownership: ReadonlyMap<string, Ownership.ModuleOwnership>,
  layout: Layout.Plan,
  index: DeclarationIndex.Index,
  opaqueRealizations: OpaqueRealization.Catalog,
): Mir.Module => {
  const staticDataById = new Map<
    string,
    Extract<
      Hir.Expression,
      { readonly _tag: 'StaticStringLiteral' | 'StaticByteViewLiteral' }
    >['data']
  >()
  for (const instance of discovery.instances) {
    for (const expression of instance.function.statements
      .flatMap(Hir.statementExpressions)
      .flatMap(Hir.expressionTree)) {
      if (expression._tag === 'StaticStringLiteral' || expression._tag === 'StaticByteViewLiteral')
        staticDataById.set(expression.data.id, expression.data)
    }
  }
  const staticData = Object.freeze(
    [...staticDataById.values()].sort((left, right) => left.id.localeCompare(right.id)),
  )
  const effectResults = new Map<string, ExecutableEffectType>()
  const generatedRunners: Array<GeneratedEffectRunner> = []
  for (const instance of discovery.instances) {
    const resultKey = instanceText(instance.key.declaration, instance.key.typeArguments)
    const block = returnedEffectBlock(instance.function)
    const type = block === undefined ? undefined : effectValueType(layout, instance.key, block)
    if (type !== undefined && block !== undefined) {
      effectResults.set(resultKey, type)
      generatedRunners.push(
        Object.freeze({
          _tag: 'BlockEffectRunner',
          id: Hir.effectRunnerId(instance.key.declaration, block.site),
          owner: instance,
          block,
          type,
          specializationKey: baseRunnerKey(instance.key, block.site),
          providedRequirements: Object.freeze([]),
        }),
      )
      continue
    }
    const terminal = instance.function.statements.at(-1)
    const returned =
      terminal?._tag === 'Return' && 'type' in terminal.expression
        ? representedValueType(
            layout,
            opaqueRealizations,
            terminal.expression.type,
            instance.substitution,
          )
        : undefined
    if (returned?._tag === 'EffectComposite') effectResults.set(resultKey, returned)
  }
  const functions = discovery.instances.map((instance) =>
    lowerInstance(
      instance,
      ownership.get(instance.key.declaration.module),
      layout,
      index,
      discovery.instances,
      discovery.calls,
      effectResults,
      generatedRunners,
      opaqueRealizations,
    ),
  )
  const loweredRunners: Array<{
    readonly spec: GeneratedEffectRunner
    readonly runner: Mir.MirFunction
  }> = []
  for (let ordinal = 0; ordinal < generatedRunners.length; ordinal += 1) {
    const generated = generatedRunners.at(ordinal)
    if (generated === undefined) continue
    const runner =
      generated._tag === 'BlockEffectRunner'
        ? lowerEffectRunner(
            generated,
            ownership.get(generated.owner.key.declaration.module),
            layout,
            index,
            discovery.instances,
            discovery.calls,
            effectResults,
            generatedRunners,
            opaqueRealizations,
          )
        : generated._tag === 'CatchEffectRunner'
          ? lowerCatchEffectRunner(
              generated,
              ownership.get(generated.owner.key.declaration.module),
              layout,
              index,
              discovery.instances,
              discovery.calls,
              effectResults,
              generatedRunners,
              opaqueRealizations,
            )
          : lowerWitnessEffectRunner(
              generated,
              ownership.get(generated.owner.key.declaration.module),
              layout,
              index,
              discovery.instances,
              discovery.calls,
              effectResults,
              generatedRunners,
              opaqueRealizations,
            )
    if (runner !== undefined) loweredRunners.push(Object.freeze({ spec: generated, runner }))
  }
  // Lowering a provided parent can discover provided children after their open bases were already
  // visited. Filter only after the worklist reaches its fixed point so backends never compile an
  // unreachable open runner that still calls another open runner without provider arguments.
  const unresolvedOpenBase = (spec: GeneratedEffectRunner): boolean => {
    const entryOwnsRunner =
      discovery.entry._tag === 'Resolved' &&
      discovery.entry.kind === 'Effect' &&
      instanceText(spec.owner.key.declaration, spec.owner.key.typeArguments) ===
        instanceText(discovery.entry.key.declaration, discovery.entry.key.typeArguments)
    return (
      !entryOwnsRunner &&
      spec.providedRequirements.length === 0 &&
      Type.requirementMembers(spec.type.type).length > 0
    )
  }
  const runnerKey = (
    declaration: DeclarationIndex.CanonicalId,
    typeArguments: ReadonlyArray<Type.GenericArgument>,
  ): string => instanceText(declaration, typeArguments)
  const retainedRunners = new Set(
    loweredRunners
      .filter(({ spec }) => !unresolvedOpenBase(spec))
      .map(({ spec }) => runnerKey(spec.id, spec.owner.key.typeArguments)),
  )
  const retainReferencedRunners = (fn: Mir.MirFunction): boolean => {
    let changed = false
    for (const operation of Mir.operations(fn)) {
      if (
        operation._tag !== 'RunEffectValue' &&
        operation._tag !== 'RunStaticEffect' &&
        operation._tag !== 'ReifyEffect'
      )
        continue
      const key = runnerKey(operation.runner, operation.runnerTypeArguments)
      if (!retainedRunners.has(key)) {
        retainedRunners.add(key)
        changed = true
      }
    }
    return changed
  }
  for (const fn of functions) retainReferencedRunners(fn)
  let retainedChanged = true
  while (retainedChanged) {
    retainedChanged = false
    for (const { spec, runner } of loweredRunners) {
      if (!retainedRunners.has(runnerKey(spec.id, spec.owner.key.typeArguments))) continue
      if (retainReferencedRunners(runner)) retainedChanged = true
    }
  }
  functions.push(
    ...loweredRunners.flatMap(({ spec, runner }) => {
      return retainedRunners.has(runnerKey(spec.id, spec.owner.key.typeArguments)) ? [runner] : []
    }),
  )
  if (discovery.entry._tag !== 'Resolved') {
    return Object.freeze({
      _tag: 'MirModule',
      module: discovery.rootModule,
      intrinsics: discovery.intrinsics,
      entry: Object.freeze({ _tag: 'UnavailableEntry', reason: discovery.entry.reason }),
      layout,
      staticData,
      functions: Object.freeze(functions),
    })
  }
  const resolvedEntry = discovery.entry
  let entry: Mir.Entry
  if (resolvedEntry.kind === 'Ordinary') {
    if (resolvedEntry.result === 'Status') {
      entry = Object.freeze({
        _tag: 'OrdinaryEntry',
        target: resolvedEntry.key,
        machine: resolvedEntry.key,
      })
    } else {
      const target = functions.find((fn) =>
        Mir.matchesInstance(fn, resolvedEntry.key.declaration, resolvedEntry.key.typeArguments),
      )
      if (target === undefined) throw new RangeError('Unit entry lowering lost its target')
      const span = target.regions
        .flatMap((region) =>
          region._tag === 'OperationRegion'
            ? region.operations.map((operation) => operation.provenance.span)
            : region._tag === 'CleanupRegion'
              ? region.releases.map((operation) => operation.provenance.span)
              : [region.provenance.span],
        )
        .at(0)
      if (span === undefined) throw new RangeError('Unit entry lowering lost source provenance')
      const adapterId = unitEntryAdapterId(discovery.rootModule)
      const adapterKey: Instances.InstanceKey = Object.freeze({
        _tag: 'InstanceKey',
        declaration: adapterId,
        typeArguments: Object.freeze([]),
        contractRow: Object.freeze(['generated:unit-entry']),
      })
      functions.push(
        Object.freeze({
          _tag: 'MirFunction',
          id: adapterId,
          instance: adapterKey,
          parameterCount: 0,
          localTypes: Object.freeze([i32, target.result]),
          result: i32,
          entry: Object.freeze({ _tag: 'Region', ordinal: 0 }),
          regions: Object.freeze([
            Object.freeze({
              _tag: 'OperationRegion' as const,
              id: Object.freeze({ _tag: 'Region' as const, ordinal: 0 }),
              operations: Object.freeze([
                Object.freeze({
                  _tag: 'Call' as const,
                  destination: local(1),
                  target: resolvedEntry.key.declaration,
                  typeArguments: resolvedEntry.key.typeArguments,
                  arguments: Object.freeze([]),
                  type: target.result,
                  provenance: generated(span),
                }),
                Object.freeze({
                  _tag: 'Literal' as const,
                  destination: local(0),
                  type: i32,
                  value: 0,
                  provenance: generated(span),
                }),
              ]),
              outcome: Object.freeze({
                _tag: 'Return' as const,
                value: local(0),
                provenance: generated(span),
              }),
            }),
          ]),
        }),
      )
      entry = Object.freeze({
        _tag: 'OrdinaryEntry',
        target: resolvedEntry.key,
        machine: adapterKey,
      })
    }
  } else {
    const target = functions.find(
      (fn) =>
        instanceText(fn.instance.declaration, fn.instance.typeArguments) ===
        instanceText(resolvedEntry.key.declaration, resolvedEntry.key.typeArguments),
    )
    const runnerSpec = generatedRunners.find(
      (candidate) =>
        instanceText(candidate.owner.key.declaration, candidate.owner.key.typeArguments) ===
        instanceText(resolvedEntry.key.declaration, resolvedEntry.key.typeArguments),
    )
    const runner =
      runnerSpec === undefined
        ? undefined
        : functions.find((fn) =>
            Mir.matchesInstance(fn, runnerSpec.id, resolvedEntry.key.typeArguments),
          )
    if (target?.result._tag !== 'EffectValue' || runner?.result._tag !== 'EffectOutcome') {
      throw new RangeError('Effect entry lowering lost its constructor or runner')
    }
    const adapterId = effectEntryAdapterId(discovery.rootModule)
    const adapterKey: Instances.InstanceKey = Object.freeze({
      _tag: 'InstanceKey',
      declaration: adapterId,
      typeArguments: Object.freeze([]),
      contractRow: Object.freeze(['generated:effect-entry']),
    })
    const span = target.regions
      .flatMap((region) =>
        region._tag === 'OperationRegion'
          ? region.operations.map((operation) => operation.provenance.span)
          : region._tag === 'CleanupRegion'
            ? region.releases.map((operation) => operation.provenance.span)
            : [region.provenance.span],
      )
      .at(0)
    if (span === undefined) throw new RangeError('Effect entry lowering lost source provenance')
    const failures = resolvedEntry.failures.map((failure, ordinal) =>
      Object.freeze({
        tag: ordinal + 1,
        type: failure.type,
        identity: failure.identity,
        payload: local(ordinal + 3),
        cleanup: CleanupPlan.cleanupPlan(index, failure.type),
      }),
    )
    const failurePayloadTypes = failures.map((failure) => {
      const type = mirType(failure.type)
      if (type === undefined)
        throw new RangeError(`Effect entry failure ${Type.encode(failure.type)} has no MIR type`)
      return type
    })
    const effect = local(1)
    const outcome = local(2)
    const status = local(0)
    functions.push(
      Object.freeze({
        _tag: 'MirFunction',
        id: adapterId,
        instance: adapterKey,
        parameterCount: 0,
        localTypes: Object.freeze([i32, target.result, runner.result, ...failurePayloadTypes]),
        result: i32,
        entry: Object.freeze({ _tag: 'Region', ordinal: 0 }),
        regions: Object.freeze([
          Object.freeze({
            _tag: 'OperationRegion' as const,
            id: Object.freeze({ _tag: 'Region' as const, ordinal: 0 }),
            operations: Object.freeze([
              Object.freeze({
                _tag: 'CloseEffectEntry' as const,
                destination: status,
                effect,
                outcome,
                target: resolvedEntry.key.declaration,
                runner: runner.id,
                typeArguments: resolvedEntry.key.typeArguments,
                effectType: target.result,
                outcomeType: runner.result,
                failures: Object.freeze(failures),
                type: i32,
                provenance: generated(span),
              }),
            ]),
            outcome: Object.freeze({
              _tag: 'Return' as const,
              value: status,
              provenance: generated(span),
            }),
          }),
        ]),
      }),
    )
    entry = Object.freeze({
      _tag: 'EffectEntry',
      target: resolvedEntry.key,
      machine: adapterKey,
      requirements: resolvedEntry.requirements,
      failures: Object.freeze(
        resolvedEntry.failures.map((failure, ordinal) =>
          Object.freeze({ tag: ordinal + 1, type: failure.type, identity: failure.identity }),
        ),
      ),
    })
  }
  return Object.freeze({
    _tag: 'MirModule',
    module: discovery.rootModule,
    intrinsics: discovery.intrinsics,
    entry,
    layout,
    staticData,
    functions: Object.freeze(functions),
  })
}
