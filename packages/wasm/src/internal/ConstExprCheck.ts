import * as Result from 'effect/Result'
import type * as Instr from '../Instr.js'
import * as ValType from '../ValType.js'
import { validationFailed, type WasmError } from '../WasmError.js'
import * as Handle from './Handle.js'
import type * as ModuleState from './ModuleState.js'
import type * as OwnedHandle from './OwnedHandle.js'
import * as Subtype from './Subtype.js'

const extendedConstMnemonics = new Set([
  'i32.add',
  'i32.sub',
  'i32.mul',
  'i64.add',
  'i64.sub',
  'i64.mul',
])

/**
 * Validates a constant expression against the specification's constant instruction set and
 * checks that it produces exactly one value of the expected type.
 *
 * @internal
 */
export const check = (
  state: ModuleState.MutableState,
  owner: OwnedHandle.Owner,
  expr: ReadonlyArray<Instr.Instr>,
  expected: ValType.ValType,
  operation: string,
): Result.Result<void, WasmError> =>
  Result.gen(function* () {
    const invalid = (message: string, detail: unknown) =>
      Result.fail(validationFailed({ operation, message, detail }))
    const stack: Array<ValType.ValType> = []
    for (const instr of expr) {
      switch (instr._tag) {
        case 'I32Const':
          stack.push(ValType.i32)
          break
        case 'I64Const':
          stack.push(ValType.i64)
          break
        case 'F32Const':
          stack.push(ValType.f32)
          break
        case 'F64Const':
          stack.push(ValType.f64)
          break
        case 'RefNull':
          stack.push(ValType.refNull(instr.heapType))
          break
        case 'RefFunc': {
          const index = yield* Handle.resolve(owner, instr.func, 'Func', operation)
          const typeHandle = state.typeHandles[state.funcs[index]?.typeIndex ?? -1]
          stack.push(typeHandle === undefined ? ValType.funcref : ValType.ref(typeHandle))
          break
        }
        case 'GlobalGet': {
          const index = yield* Handle.resolve(owner, instr.global, 'Global', operation)
          const entry = state.globals[index]
          if (entry === undefined) {
            return yield* invalid('Global table entry is missing', instr)
          }
          if (entry.importSource === undefined) {
            return yield* invalid('A constant expression may only read an imported global', instr)
          }
          if (entry.mutable) {
            return yield* invalid('A constant expression may only read an immutable global', instr)
          }
          stack.push(entry.valType)
          break
        }
        case 'Op': {
          if (!extendedConstMnemonics.has(instr.mnemonic)) {
            return yield* invalid(`The instruction ${instr.mnemonic} is not constant`, instr)
          }
          const operand = instr.mnemonic.startsWith('i32') ? ValType.i32 : ValType.i64
          const right = stack.pop()
          const left = stack.pop()
          if (
            right === undefined ||
            left === undefined ||
            !ValType.equals(right, operand) ||
            !ValType.equals(left, operand)
          ) {
            return yield* invalid(
              `The instruction ${instr.mnemonic} requires two ${ValType.text(operand)} operands`,
              instr,
            )
          }
          stack.push(operand)
          break
        }
        default:
          return yield* invalid(`The instruction ${instr._tag} is not constant`, instr)
      }
    }
    const produced = stack.length === 1 ? stack[0] : undefined
    if (produced === undefined || !Subtype.matches(state, produced, expected)) {
      return yield* invalid(
        `A constant expression here must produce exactly one ${ValType.text(expected)}`,
        { expected: ValType.text(expected), produced: stack.map(ValType.text) },
      )
    }
  })
