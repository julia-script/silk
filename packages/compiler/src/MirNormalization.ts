import * as CleanupPlan from './CleanupPlan.js'
import type * as DeclarationFacts from './DeclarationFacts.js'
import * as Mir from './Mir.js'
import * as ProvisionalMir from './ProvisionalMir.js'

/**
 * Shared, backend-neutral normalization of representation-only Effect construction and dispatch.
 * The deliberately small first slice never clones control flow or transfers affine ownership.
 */

interface ConstructorShape {
  readonly fn: Mir.MirFunction
  readonly construction: Extract<Mir.Operation, { readonly _tag: 'MakeEffect' }>
}

const sameRegion = (left: Mir.RegionId, right: Mir.RegionId): boolean =>
  left.ordinal === right.ordinal

/**
 * A constructor is one effect construction returned as is. Its function either returns the
 * construction from the entry region or forwards to one cleanup region that only drops parameters
 * whose cleanup has no effect: a service operation's outer function ends by dropping its borrowed
 * `self`, and that drop is nothing the fold could lose.
 */
const constructorShape = (fn: Mir.MirFunction | undefined): ConstructorShape | undefined => {
  if (fn === undefined || fn.regions.length === 0 || fn.regions.length > 2) return undefined
  const region = fn.regions.at(0)
  if (
    region?._tag !== 'OperationRegion' ||
    !sameRegion(region.id, fn.entry) ||
    region.operations.length !== 1
  )
    return undefined
  const construction = region.operations.at(0)
  if (construction?._tag !== 'MakeEffect') return undefined
  if (fn.regions.length === 1) {
    return region.outcome._tag === 'Return' &&
      region.outcome.value.ordinal === construction.destination.ordinal
      ? Object.freeze({ fn, construction })
      : undefined
  }
  const cleanup = fn.regions.at(1)
  if (
    region.outcome._tag !== 'Forward' ||
    cleanup?._tag !== 'CleanupRegion' ||
    !sameRegion(region.outcome.target, cleanup.id) ||
    cleanup.outcome._tag !== 'Return' ||
    cleanup.outcome.value.ordinal !== construction.destination.ordinal ||
    !cleanup.releases.every(
      (release) =>
        release._tag === 'Drop' &&
        release.local.ordinal < fn.parameterCount &&
        !CleanupPlan.hasEffect(release.cleanup),
    )
  )
    return undefined
  return Object.freeze({ fn, construction })
}

/** The region guard a fold proved: one region, or one trailing cleanup region of trivial drops. */
type ConstructorGuard = 'SingleRegion' | 'TrivialCleanup'

const constructorGuardOf = (shape: ConstructorShape): ConstructorGuard =>
  shape.fn.regions.length === 1 ? 'SingleRegion' : 'TrivialCleanup'

const directTarget = (
  program: Mir.Module,
  operation: Extract<Mir.Operation, { readonly _tag: 'Call' | 'ApplyCallable' }>,
): ConstructorShape | undefined => {
  if (operation._tag === 'Call') {
    return constructorShape(
      program.functions.find((candidate) =>
        Mir.matchesInstance(
          candidate,
          operation.target,
          operation.typeArguments,
          operation.staticArguments,
        ),
      ),
    )
  }
  const target = operation.target
  if (target?._tag !== 'DeclarationCallableTarget') return undefined
  return constructorShape(
    program.functions.find((candidate) =>
      Mir.matchesInstance(candidate, target.declaration, operation.typeArguments),
    ),
  )
}

const hasConcreteTarget = (
  program: Mir.Module,
  operation: Extract<Mir.Operation, { readonly _tag: 'Call' | 'ApplyCallable' }>,
): boolean => {
  let declaration: DeclarationFacts.CanonicalId | undefined
  if (operation._tag === 'Call') {
    declaration = operation.target
  } else if (operation.target?._tag === 'DeclarationCallableTarget') {
    declaration = operation.target.declaration
  }
  return (
    declaration !== undefined &&
    program.functions.some((candidate) =>
      Mir.matchesInstance(candidate, declaration, operation.typeArguments),
    )
  )
}

const parametersFor = (
  operation: Extract<Mir.Operation, { readonly _tag: 'Call' | 'ApplyCallable' }>,
  count: number,
): ReadonlyArray<Mir.LocalId> | undefined => {
  if (operation._tag === 'Call')
    return operation.arguments.length === count ? operation.arguments : undefined
  const ordered: Array<Mir.LocalId | undefined> = Array.from({ length: count })
  for (const capture of operation.captures) {
    if (ordered.at(capture.parameterOrdinal) !== undefined) return undefined
    ordered[capture.parameterOrdinal] = capture.source
  }
  for (const argument of operation.arguments) {
    const ordinal = ordered.indexOf(undefined)
    if (ordinal < 0) return undefined
    ordered[ordinal] = argument
  }
  return ordered.every((local) => local !== undefined)
    ? Object.freeze(ordered.filter((local): local is Mir.LocalId => local !== undefined))
    : undefined
}

const foldConstructor = (
  program: Mir.Module,
  operation: Mir.Operation,
): Extract<Mir.Operation, { readonly _tag: 'MakeEffect' }> | undefined => {
  if (operation._tag !== 'Call' && operation._tag !== 'ApplyCallable') return undefined
  const shape = directTarget(program, operation)
  if (shape === undefined) return undefined
  const parameters = parametersFor(operation, shape.fn.parameterCount)
  if (parameters === undefined) return undefined
  const captures = shape.construction.captures.map((capture) => {
    const source = parameters.at(capture.source.ordinal)
    return source === undefined ? undefined : Object.freeze({ ...capture, source })
  })
  if (captures.some((capture) => capture === undefined)) return undefined
  return Object.freeze({
    _tag: 'MakeEffect',
    destination: operation.destination,
    runner: shape.construction.runner,
    runnerTypeArguments: shape.construction.runnerTypeArguments,
    captures: Object.freeze(
      captures.filter(
        (
          capture,
        ): capture is Extract<Mir.Operation, { readonly _tag: 'MakeEffect' }>['captures'][number] =>
          capture !== undefined,
      ),
    ),
    type: shape.construction.type,
    provenance: operation.provenance,
  })
}

const containsLocal = (value: unknown, ordinal: number, seen = new Set<object>()): boolean => {
  if (typeof value !== 'object' || value === null) return false
  if (seen.has(value)) return false
  seen.add(value)
  if ('_tag' in value && value._tag === 'Local' && 'ordinal' in value && value.ordinal === ordinal)
    return true
  return Object.values(value).some((entry) => containsLocal(entry, ordinal, seen))
}

const usesOf = (
  fn: Mir.MirFunction,
  definition: Extract<Mir.Operation, { readonly _tag: 'MakeEffect' }>,
): ReadonlyArray<{ readonly region: Mir.Region; readonly operation?: Mir.Operation }> => {
  const uses: Array<{ readonly region: Mir.Region; readonly operation?: Mir.Operation }> = []
  for (const region of fn.regions) {
    if (region._tag === 'OperationRegion') {
      for (const operation of region.operations) {
        if (operation !== definition && containsLocal(operation, definition.destination.ordinal))
          uses.push(Object.freeze({ region, operation }))
      }
      if (containsLocal(region.outcome, definition.destination.ordinal))
        uses.push(Object.freeze({ region }))
    } else if (region._tag === 'CleanupRegion') {
      if (
        region.releases.some((operation) =>
          containsLocal(operation, definition.destination.ordinal),
        ) ||
        containsLocal(region.outcome, definition.destination.ordinal)
      )
        uses.push(Object.freeze({ region }))
    } else if (containsLocal(region, definition.destination.ordinal)) {
      uses.push(Object.freeze({ region }))
    }
  }
  return Object.freeze(uses)
}

const rejection = (
  fn: Mir.MirFunction,
  region: Mir.RegionId,
  construction: Extract<Mir.Operation, { readonly _tag: 'MakeEffect' }>,
  reason: Mir.NormalizationRejection,
): Mir.NormalizationVerdict =>
  Object.freeze({
    _tag: 'Rejected',
    reason,
    function: fn.id,
    region,
    local: construction.destination,
    provenance: construction.provenance,
  })

const suspensionReason = (
  classification: ProvisionalMir.Classification,
): Extract<Mir.NormalizationRejection, 'SuspendableRunner' | 'SuspensionUnknown'> | undefined => {
  if (classification === 'Suspendable') return 'SuspendableRunner'
  if (classification === 'Unknown') return 'SuspensionUnknown'
  return undefined
}

const operationClassification = (
  provisional: ProvisionalMir.Module,
  fn: Mir.MirFunction,
  operation: Extract<
    Mir.Operation,
    { readonly _tag: 'RunEffect' | 'RunEffectValue' | 'CatchEffect' | 'CloseEffectEntry' }
  >,
): ProvisionalMir.Classification =>
  operation._tag === 'CloseEffectEntry'
    ? ProvisionalMir.classificationOfRunner(provisional, operation.runner, operation.typeArguments)
    : ProvisionalMir.classificationOfRun(provisional, fn.instance, operation.provenance.span)

/** Normalizes one target-aware MIR module from exact provisional runner facts. */
export const normalize = (program: Mir.Module, provisional: ProvisionalMir.Module): Mir.Module => {
  if (program.normalization !== undefined) return program
  const verdicts: Array<Mir.NormalizationVerdict> = []
  let changed = false
  const functions = program.functions.map((fn) => {
    let functionChanged = false
    const constructorGuards = new Map<number, ConstructorGuard>()
    const foldedRegions = fn.regions.map((region) => {
      if (region._tag !== 'OperationRegion') return region
      const operations = region.operations.map((operation) => {
        const target =
          operation._tag === 'Call' || operation._tag === 'ApplyCallable'
            ? directTarget(program, operation)
            : undefined
        const targetSuspension =
          target === undefined
            ? undefined
            : suspensionReason(
                ProvisionalMir.classificationOfExecution(provisional, target.fn.instance),
              )
        const folded =
          targetSuspension === undefined ? foldConstructor(program, operation) : undefined
        if (folded === undefined) {
          if (
            (operation._tag === 'Call' || operation._tag === 'ApplyCallable') &&
            operation.type._tag === 'EffectValue'
          ) {
            verdicts.push(
              Object.freeze({
                _tag: 'Rejected',
                reason:
                  targetSuspension ??
                  (hasConcreteTarget(program, operation) ? 'ComplexConstructor' : 'DynamicTarget'),
                function: fn.id,
                region: region.id,
                local: operation.destination,
                provenance: operation.provenance,
              }),
            )
          }
          return operation
        }
        functionChanged = true
        changed = true
        const guard = target === undefined ? 'SingleRegion' : constructorGuardOf(target)
        constructorGuards.set(folded.destination.ordinal, guard)
        verdicts.push(
          Object.freeze({
            _tag: 'Normalized',
            kind: 'FoldedConstructor',
            function: fn.id,
            region: region.id,
            local: folded.destination,
            guards: Object.freeze(['DirectTarget', guard, 'Synchronous'] as const),
            provenance: folded.provenance,
          }),
        )
        return folded
      })
      return functionChanged
        ? Object.freeze({ ...region, operations: Object.freeze(operations) })
        : region
    })
    const folded = functionChanged
      ? Object.freeze({ ...fn, regions: Object.freeze(foldedRegions) })
      : fn
    for (const region of folded.regions) {
      if (region._tag !== 'OperationRegion') continue
      for (const operation of region.operations) {
        if (
          operation._tag !== 'RunEffect' &&
          operation._tag !== 'CatchEffect' &&
          operation._tag !== 'CloseEffectEntry'
        )
          continue
        const reason = suspensionReason(operationClassification(provisional, folded, operation))
        if (reason === undefined) continue
        verdicts.push(
          Object.freeze({
            _tag: 'Rejected',
            reason,
            function: folded.id,
            region: region.id,
            local: operation.destination,
            provenance: operation.provenance,
          }),
        )
      }
    }
    let directChanged = false
    const directRegions = folded.regions.map((region) => {
      if (region._tag !== 'OperationRegion') return region
      const removed = new Set<Mir.Operation>()
      const replacements = new Map<Mir.Operation, Mir.Operation>()
      for (const construction of region.operations) {
        if (construction._tag !== 'MakeEffect') continue
        const uses = usesOf(folded, construction)
        const use = uses.at(0)
        const run = use?.operation
        const runSuspension =
          run?._tag === 'RunEffectValue'
            ? suspensionReason(operationClassification(provisional, folded, run))
            : undefined
        let reason: Mir.NormalizationRejection | undefined = runSuspension
        if (reason === undefined) {
          if (uses.length === 0) reason = 'EffectEscapes'
          else if (uses.length > 1) reason = 'EffectReused'
          else if (use === undefined || !sameRegion(use.region.id, region.id)) {
            reason = 'CrossRegionUse'
          } else if (
            run?._tag !== 'RunEffectValue' ||
            run.effect.ordinal !== construction.destination.ordinal
          ) {
            reason = 'EffectEscapes'
          } else if (
            construction.captures.some(
              (capture, ordinal) =>
                (capture.access !== 'Copy' && capture.access !== 'Shared') ||
                construction.type.environment.fields.at(ordinal)?.representation === 'Borrow',
            )
          ) {
            reason = 'AffineCapture'
          }
        }
        if (reason !== undefined) {
          verdicts.push(rejection(folded, region.id, construction, reason))
          continue
        }
        if (run?._tag !== 'RunEffectValue') continue
        removed.add(construction)
        replacements.set(
          run,
          Object.freeze({
            _tag: 'RunStaticEffect',
            destination: run.destination,
            outcome: run.outcome,
            runner: run.runner,
            runnerTypeArguments: run.runnerTypeArguments,
            ...(run.runnerStaticArguments === undefined
              ? {}
              : { runnerStaticArguments: run.runnerStaticArguments }),
            captures: Object.freeze(
              construction.captures.map((capture) =>
                Object.freeze({
                  source: capture.source,
                  access: capture.access as 'Copy' | 'Shared',
                }),
              ),
            ),
            arguments: run.arguments,
            outcomeType: run.outcomeType,
            ...(run.propagationType === undefined ? {} : { propagationType: run.propagationType }),
            tagMappings: run.tagMappings,
            propagationLaneCount: run.propagationLaneCount,
            ...(run.failureLoanEnds === undefined ? {} : { failureLoanEnds: run.failureLoanEnds }),
            ...(run.releases === undefined ? {} : { releases: run.releases }),
            type: run.type,
            provenance: run.provenance,
          }),
        )
        verdicts.push(
          Object.freeze({
            _tag: 'Normalized',
            kind: 'DirectStaticRun',
            function: folded.id,
            region: region.id,
            local: run.destination,
            guards: Object.freeze([
              'DirectTarget',
              constructorGuards.get(construction.destination.ordinal) ?? 'SingleRegion',
              'SingleUse',
              'Synchronous',
              'CopyOrShared',
            ] as const),
            provenance: run.provenance,
          }),
        )
        directChanged = true
        changed = true
      }
      if (removed.size === 0 && replacements.size === 0) return region
      return Object.freeze({
        ...region,
        operations: Object.freeze(
          region.operations.flatMap((operation) => {
            if (removed.has(operation)) return []
            return [replacements.get(operation) ?? operation]
          }),
        ),
      })
    })
    return directChanged
      ? Object.freeze({ ...folded, regions: Object.freeze(directRegions) })
      : folded
  })
  return Object.freeze({
    ...program,
    functions: changed ? Object.freeze(functions) : program.functions,
    normalization: Object.freeze(verdicts),
  })
}
