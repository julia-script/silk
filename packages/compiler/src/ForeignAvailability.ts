/**
 * Native-only availability for reachable foreign (`extern "C"`) calls. The sibling of
 * `IntrinsicAvailability.select`: it reads the foreign inventory discovery carried onto
 * `Instances.Discovery` and `Mir.Module` and answers with stable source diagnostics, so no
 * backend or evaluator ever meets a `ForeignCall` it cannot host.
 */
import * as CAbi from './CAbi.js'
import * as Diagnostic from './Diagnostic.js'
import type * as Instances from './Instances.js'
import type * as Intrinsic from './Intrinsic.js'
import type * as Target from './Target.js'

/**
 * Rejects every reachable foreign call under the evaluator, the direct WebAssembly backend, or
 * LLVM emission of a non-native target (one diagnostic per symbol at its first call), and every
 * pair of reachable declarations of one symbol whose classified C signatures differ. An empty
 * result means the inventory is available.
 */
export const select = (
  calls: ReadonlyArray<Instances.ForeignCall>,
  executionTarget: Intrinsic.ExecutionTarget,
  target: Target.Target,
): ReadonlyArray<Diagnostic.Diagnostic> => {
  if (calls.length === 0) return Object.freeze([])
  let surface: string | undefined
  if (executionTarget !== 'LLVM') surface = executionTarget
  else if (target.kind !== 'Native') surface = target.id
  const unavailable = new Map<string, Diagnostic.Diagnostic>()
  if (surface !== undefined)
    for (const call of calls)
      if (!unavailable.has(call.symbol))
        unavailable.set(
          call.symbol,
          Diagnostic.foreignFunctionTargetUnavailable(call.symbol, surface, call.callSpan),
        )
  const conflicts: Array<Diagnostic.Diagnostic> = []
  for (const [ordinal, call] of calls.entries())
    for (const other of calls.slice(ordinal + 1))
      if (
        other.symbol === call.symbol &&
        CAbi.signatureKey(other.signature) !== CAbi.signatureKey(call.signature)
      )
        conflicts.push(
          Diagnostic.conflictingForeignSignature(
            call.symbol,
            other.declarationSpan,
            call.declarationSpan,
          ),
        )
  return Diagnostic.merge([...unavailable.values()], conflicts)
}
