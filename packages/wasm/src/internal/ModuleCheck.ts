/**
 * Module-level validation shared by both emitters: constraints that are not facts about a single
 * declaration and are only decidable once the whole module is known.
 *
 * Most module rules are enforced when declarations are committed (export-name uniqueness,
 * segment offsets, start signature, limits). This pass checks the two facts that can change
 * after a declaration: every declared function has a definition, and every `ref.func` inside a
 * body targets a function declared outside function bodies (in an element segment, a global
 * initializer, or an export).
 *
 * @internal
 */
import * as Result from 'effect/Result'
import type * as Instr from '../Instr.js'
import { validationFailed, type WasmError } from '../WasmError.js'
import * as Handle from './Handle.js'
import type * as ModuleState from './ModuleState.js'

/** @internal */
export const check = (
  snapshot: ModuleState.Snapshot,
  operation: string,
): Result.Result<void, WasmError> =>
  Result.gen(function* () {
    const declaredFuncs = new Set<number>()
    const collect = (expr: ReadonlyArray<Instr.Instr>) =>
      Result.gen(function* () {
        for (const instr of expr) {
          if (instr._tag === 'RefFunc') {
            declaredFuncs.add(yield* Handle.resolve(snapshot.owner, instr.func, 'Func', operation))
          }
        }
      })
    for (const global of snapshot.globals) {
      if (global.init !== undefined) yield* collect(global.init)
    }
    for (const elem of snapshot.elems) {
      for (const item of elem.items) yield* collect(item)
    }
    for (const moduleExport of snapshot.exports) {
      if (moduleExport.kind === 'func') declaredFuncs.add(moduleExport.entryIndex)
    }
    for (const [index, func] of snapshot.funcs.entries()) {
      if (func.importSource === undefined && func.definition === undefined) {
        return yield* Result.fail(
          validationFailed({
            operation,
            message: `The declared function ${func.name ?? index} was never defined`,
            detail: index,
          }),
        )
      }
      if (func.definition !== undefined) {
        for (const target of func.definition.refFuncs) {
          if (!declaredFuncs.has(target)) {
            return yield* Result.fail(
              validationFailed({
                operation,
                message:
                  'ref.func inside a body requires the target function to be declared in an ' +
                  'element segment, global initializer, or export',
                detail: { func: index, target },
              }),
            )
          }
        }
      }
    }
  })
