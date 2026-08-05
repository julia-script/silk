/**
 * Constant expressions used by global initializers and active segment offsets.
 *
 * A constant expression is an ordinary instruction sequence restricted to the specification's
 * constant instruction set: `i32.const`, `i64.const`, `f32.const`, `f64.const`, `ref.null`,
 * `ref.func`, `global.get` of an imported immutable global, and — via the extended constant
 * expressions feature — `i32.add`, `i32.sub`, `i32.mul` and their `i64` counterparts.
 *
 * Build constant expressions with the `Instr` constructors; the consuming declaration
 * (`Global.make`, `Elem.active`, `Data.active`, …) validates the sequence against its expected
 * result type when it is committed.
 *
 * @since 0.0.0
 */
import type * as Instr from './Instr.js'

/**
 * An instruction sequence restricted to constant instructions.
 *
 * @category constant expressions
 * @since 0.0.0
 */
export type ConstExpr = ReadonlyArray<Instr.Instr>
