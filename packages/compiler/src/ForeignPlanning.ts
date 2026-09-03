/**
 * Executable-wide planning checks for native exports (`export "C"`) over `Mir.Module`, run at the
 * same gates as `ForeignAvailability.select` before any backend is constructed: the closure-wide
 * symbol map over imports and exports, the non-native rejection, and the post-MIR suspension
 * restriction. The evaluator never calls it; exports are inert there.
 */
import * as Diagnostic from './Diagnostic.js'
import type * as Intrinsic from './Intrinsic.js'
import * as Mir from './Mir.js'
import * as MirVerification from './MirVerification.js'
import type * as SourceSpan from './SourceSpan.js'
import type * as Target from './Target.js'

/**
 * Reports one conflicting-foreign-signature diagnostic per export whose symbol an import or an
 * earlier export already claims, one target-unavailable diagnostic per export off a native
 * target, and one export-suspends diagnostic per export whose MIR function is not synchronous.
 */
export const check = (
  program: Mir.Module,
  executionTarget: Intrinsic.ExecutionTarget,
  target: Target.Target,
): ReadonlyArray<Diagnostic.Diagnostic> => {
  if (program.foreignExports.length === 0 && program.foreignStatics.length === 0)
    return Object.freeze([])
  const surface = executionTarget === 'LLVM' ? target.id : executionTarget
  const claimed = new Map<string, SourceSpan.SourceSpan>(
    program.foreignCalls.map((call) => [call.symbol, call.declarationSpan]),
  )
  const callbackAddresses = program.functions.flatMap((fn) =>
    MirVerification.operations(fn).filter(
      (operation) => operation._tag === 'ForeignFunctionAddress',
    ),
  )
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  for (const record of program.foreignExports) {
    const other = claimed.get(record.symbol)
    if (other === undefined) claimed.set(record.symbol, record.declarationSpan)
    else
      diagnostics.push(
        Diagnostic.conflictingForeignSignature(record.symbol, record.declarationSpan, other),
      )
    if (target.kind !== 'Native')
      diagnostics.push(
        Diagnostic.foreignFunctionTargetUnavailable(record.symbol, surface, record.declarationSpan),
      )
    const fn = program.functions.find((candidate) => Mir.matchesInstanceKey(candidate, record.key))
    if (fn?.suspension !== undefined && fn.suspension.classification !== 'Synchronous') {
      diagnostics.push(
        Diagnostic.exportSuspends(
          record.symbol,
          record.declarationSpan,
          fn.suspension.regions.at(0)?.provenance.span,
        ),
      )
      for (const callback of callbackAddresses) {
        if (
          callback.target._tag === 'DeclarationCallableTarget' &&
          callback.target.declaration.module === record.declaration.module &&
          callback.target.declaration.name === record.declaration.name
        )
          diagnostics.push(
            Diagnostic.invalidForeignCallback(
              record.declaration.name,
              'the exported function may suspend and has no synchronous C callback address',
              callback.provenance.span,
            ),
          )
      }
    }
  }
  for (const record of program.foreignStatics) {
    const other = claimed.get(record.symbol)
    if (other === undefined) claimed.set(record.symbol, record.declarationSpan)
    else
      diagnostics.push(
        Diagnostic.conflictingForeignSignature(record.symbol, record.declarationSpan, other),
      )
  }
  return Diagnostic.merge(diagnostics)
}
