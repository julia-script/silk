/** JSON serialization helpers for compiler-test diagnostics. */

/** Preserves exact evaluator integers in diagnostic JSON without making BigInt serialization fail. */
export const bigIntReplacer = (_key: string, value: unknown): unknown =>
  typeof value === 'bigint' ? value.toString() : value
