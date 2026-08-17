import * as Diagnostic from './Diagnostic.js'
import type * as Instances from './Instances.js'
import * as Intrinsic from './Intrinsic.js'

/** A validated, deterministic intrinsic inventory for one execution surface. */
export interface Inventory {
  readonly _tag: 'ValidatedIntrinsicInventory'
  readonly target: Intrinsic.ExecutionTarget
  readonly operations: ReadonlyArray<Intrinsic.OperationId>
  readonly calls: ReadonlyArray<Instances.IntrinsicCall>
}

/** Availability planning either publishes a validated inventory or stable source diagnostics. */
export type Selection =
  | { readonly _tag: 'Available'; readonly inventory: Inventory }
  | {
      readonly _tag: 'Unavailable'
      readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
    }

const operations = (): ReadonlyArray<Intrinsic.Operation> =>
  Intrinsic.all().flatMap((actor) => actor.operations)

const operationKey = (id: Intrinsic.OperationId): string => Intrinsic.operationText(id)

const originKey = (
  call: Instances.IntrinsicCall,
  origin: Instances.IntrinsicCall['span'],
): string =>
  `${origin.sourceId}\u0000${origin.start}\u0000${origin.end}\u0000${operationKey(call.operation)}`

/** Reports target-independent analysis-only dependencies retained by instance discovery. */
export const analysisOnlyDiagnostics = (
  calls: ReadonlyArray<Instances.IntrinsicCall>,
  catalog: ReadonlyArray<Intrinsic.Operation> = operations(),
): ReadonlyArray<Diagnostic.Diagnostic> => {
  const byIdentity = new Map(catalog.map((operation) => [operationKey(operation.id), operation]))
  const diagnostics = new Map<string, Diagnostic.Diagnostic>()
  for (const call of calls) {
    const operation = byIdentity.get(operationKey(call.operation))
    if (operation === undefined)
      throw new RangeError(
        `Executable closure retained unknown intrinsic ${operationKey(call.operation)}`,
      )
    if (operation.availability._tag !== 'AnalysisOnly') continue
    for (const origin of call.origins) {
      const key = `${originKey(call, origin)}\u0000${operation.availability.diagnostic}`
      if (!diagnostics.has(key))
        diagnostics.set(
          key,
          Diagnostic.analysisOnlyConstruct(operation.availability.construct, origin),
        )
    }
  }
  return Diagnostic.merge([...diagnostics.values()])
}

/** Maps the explicit backend choice to the matching intrinsic execution surface. */
export const backendTarget = (backend: 'llvm' | 'wasm'): Intrinsic.ExecutionTarget =>
  backend === 'llvm' ? 'LLVM' : 'Wasm'

/**
 * Validates only calls retained by executable closure. The optional catalog exists for focused
 * compiler tests; production callers always use the sealed default catalog.
 */
export const select = (
  calls: ReadonlyArray<Instances.IntrinsicCall>,
  target: Intrinsic.ExecutionTarget,
  catalog: ReadonlyArray<Intrinsic.Operation> = operations(),
): Selection => {
  const byIdentity = new Map(catalog.map((operation) => [operationKey(operation.id), operation]))
  const retained = new Map<string, Intrinsic.OperationId>()
  const unavailable = new Map<string, Diagnostic.Diagnostic>()
  for (const call of calls) {
    const key = operationKey(call.operation)
    const operation = byIdentity.get(key)
    if (operation === undefined)
      throw new RangeError(`Executable closure retained unknown intrinsic ${key}`)
    if (operation.availability._tag === 'AnalysisOnly') continue
    retained.set(key, operation.id)
    if (!operation.availability.targets.includes(target) && !unavailable.has(key))
      unavailable.set(key, Diagnostic.intrinsicTargetUnavailable(key, target, call.span))
  }
  if (unavailable.size > 0)
    return Object.freeze({
      _tag: 'Unavailable',
      diagnostics: Diagnostic.merge([...unavailable.values()]),
    })
  return Object.freeze({
    _tag: 'Available',
    inventory: Object.freeze({
      _tag: 'ValidatedIntrinsicInventory',
      target,
      operations: Object.freeze(
        [...retained.entries()]
          .sort(([left], [right]) => left.localeCompare(right))
          .map(([, id]) => id),
      ),
      calls: Object.freeze(Array.from(calls)),
    }),
  })
}

/** Deterministic textual encoding for inventory tests, manifests, and diagnostics fixtures. */
export const encode = (self: Inventory): string =>
  [
    `target ${self.target}`,
    ...self.operations.map((operation) => `intrinsic ${operationKey(operation)}`),
    ...self.calls.map(
      (call) =>
        `call ${operationKey(call.operation)} ${call.span.sourceId}:${call.span.start}:${call.span.end}`,
    ),
    '',
  ].join('\n')
