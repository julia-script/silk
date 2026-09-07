/** Spelling and reservation rules for the native symbol named by a foreign function. */
import * as CoroutineRuntime from './CoroutineRuntime.js'
import * as OsRuntime from './OsRuntime.js'

/** Symbols the compiler owns outright: the process entry and every runtime import it declares. */
export const reservedSymbols: ReadonlyArray<string> = Object.freeze([
  'main',
  'silk_main',
  '__silk_foreign_personality',
  ...OsRuntime.symbols,
  ...CoroutineRuntime.symbols,
  'silk_host_argc_v1',
  'silk_host_argv_v1',
])

/** Generated symbol shapes: suspension thunks and `silk_<module>_<name>__<instance>`. */
export const reservedPatterns: ReadonlyArray<RegExp> = Object.freeze([
  /^silk_suspend_/,
  /^silk_.*__/,
])

export const isValidSpelling = (symbol: string): boolean => /^[A-Za-z_][A-Za-z0-9_]*$/.test(symbol)

export const isReserved = (symbol: string): boolean =>
  reservedSymbols.includes(symbol) || reservedPatterns.some((pattern) => pattern.test(symbol))
