import * as Inspectable from 'effect/Inspectable'

type Replacer = (key: string, value: unknown) => unknown

/** Renders compiler-test values, including bigint payloads, for assertion diagnostics. */
export const stringify = (
  value: unknown,
  formatting?: number | Replacer | ReadonlyArray<string | number> | null,
  whitespace?: number,
): string =>
  Inspectable.toStringUnknown(
    value,
    whitespace ?? (typeof formatting === 'number' ? formatting : 0),
  )
