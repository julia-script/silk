/** Explicit host bindings for evaluator foreign calls and direct WebAssembly imports. */
import * as Result from 'effect/Result'
import type { Value } from './BootstrapValue.js'

/**
 * Canonical public spelling of one C ABI class.
 *
 * @category models
 * @since 0.0.0
 */
export type Class =
  | 'void'
  | 'i8'
  | 'u8'
  | 'i16'
  | 'u16'
  | 'i32'
  | 'u32'
  | 'i64'
  | 'u64'
  | 'f32'
  | 'f64'
  | '*const'
  | '*mut'

/**
 * The exact C-class signature a host function agrees to implement.
 *
 * @category models
 * @since 0.0.0
 */
export interface Signature {
  readonly parameters: ReadonlyArray<Class>
  readonly result: Class
}

/**
 * A successful host invocation. `value` is absent exactly for a `void` result.
 *
 * @category models
 * @since 0.0.0
 */
export interface Returned {
  readonly _tag: 'Returned'
  readonly value?: Value
}

/**
 * A typed host refusal or operational failure.
 *
 * @category errors
 * @since 0.0.0
 */
export interface Failed {
  readonly _tag: 'Failed'
  readonly message: string
}

/**
 * The closed result of invoking one evaluator host binding.
 *
 * @category models
 * @since 0.0.0
 */
export type Invocation = Returned | Failed

/**
 * One symbol implementation supplied to a single evaluation.
 *
 * @category models
 * @since 0.0.0
 */
export interface Binding {
  readonly symbol: string
  readonly signature: Signature
  readonly invoke: (arguments_: ReadonlyArray<Value>) => Invocation
}

/**
 * An immutable, symbol-keyed collection of evaluator bindings.
 *
 * @category models
 * @since 0.0.0
 */
export interface Table {
  readonly _tag: 'ForeignHostTable'
  readonly bindings: ReadonlyArray<Binding>
}

/**
 * A duplicate symbol rejected while constructing a host table.
 *
 * @category errors
 * @since 0.0.0
 */
export interface DuplicateSymbol {
  readonly _tag: 'DuplicateForeignHostSymbol'
  readonly symbol: string
}

/**
 * The private versioned module from which direct Wasm artifacts import foreign symbols.
 *
 * @category constants
 * @since 0.0.0
 */
export const wasmModule = 'silk:runtime/foreign@v1'

/**
 * Creates a canonical immutable host signature.
 *
 * @category constructors
 * @since 0.0.0
 */
export const signature = (parameters: ReadonlyArray<Class>, result: Class): Signature =>
  Object.freeze({ parameters: Object.freeze([...parameters]), result })

/**
 * Creates a successful host result, omitting the value for a `void` signature.
 *
 * @category constructors
 * @since 0.0.0
 */
export const returned = (value?: Value): Returned =>
  Object.freeze({ _tag: 'Returned', ...(value === undefined ? {} : { value }) })

/**
 * Creates a typed host-call failure.
 *
 * @category constructors
 * @since 0.0.0
 */
export const failed = (message: string): Failed => Object.freeze({ _tag: 'Failed', message })

/**
 * Creates an immutable table and rejects duplicate symbol ownership.
 *
 * **Details**
 *
 * The constructor snapshots and sorts every binding, signature, and parameter list. Later changes
 * to the input array cannot alter an evaluation that uses the returned table.
 *
 * @category constructors
 * @since 0.0.0
 */
export const make = (bindings: ReadonlyArray<Binding>): Result.Result<Table, DuplicateSymbol> => {
  const seen = new Set<string>()
  const copied: Array<Binding> = []
  for (const binding of bindings) {
    if (seen.has(binding.symbol))
      return Result.fail(
        Object.freeze({ _tag: 'DuplicateForeignHostSymbol', symbol: binding.symbol }),
      )
    seen.add(binding.symbol)
    copied.push(
      Object.freeze({
        symbol: binding.symbol,
        signature: signature(binding.signature.parameters, binding.signature.result),
        invoke: binding.invoke,
      }),
    )
  }
  copied.sort((left, right) => left.symbol.localeCompare(right.symbol, 'en'))
  return Result.succeed(
    Object.freeze({ _tag: 'ForeignHostTable', bindings: Object.freeze(copied) }),
  )
}

/**
 * The immutable table with no foreign host bindings.
 *
 * @category constants
 * @since 0.0.0
 */
export const empty: Table = Object.freeze({
  _tag: 'ForeignHostTable',
  bindings: Object.freeze([]),
})

/**
 * Returns the binding for a symbol when the table owns one.
 *
 * @category accessors
 * @since 0.0.0
 */
export const resolve = (self: Table, symbol: string): Binding | undefined =>
  self.bindings.find((binding) => binding.symbol === symbol)

/**
 * Encodes a host signature as its canonical comparison key.
 *
 * @category encoding
 * @since 0.0.0
 */
export const key = (self: Signature): string => `(${self.parameters.join(',')})->${self.result}`

/**
 * Checks whether two host signatures describe the same C ABI classes.
 *
 * @category comparisons
 * @since 0.0.0
 */
export const matches = (left: Signature, right: Signature): boolean => key(left) === key(right)
