import * as Option from 'effect/Option'
import type * as SourceFile from './SourceFile.js'

const SourceSpanTypeId: unique symbol = Symbol.for('@silk-effect/compiler/SourceSpan')

/**
 * A half-open byte range owned by one logical source identity.
 *
 * The private type marker prevents callers from constructing unchecked spans. Use {@link make}.
 */
export interface SourceSpan {
  readonly [SourceSpanTypeId]: typeof SourceSpanTypeId
  readonly sourceId: string
  readonly start: number
  readonly end: number
}

/** Creates a span when both offsets form an in-bounds half-open range for the source. */
export const make = (
  source: SourceFile.SourceFile,
  start: number,
  end: number,
): Option.Option<SourceSpan> =>
  Number.isSafeInteger(start) &&
  Number.isSafeInteger(end) &&
  start >= 0 &&
  start <= end &&
  end <= source.bytes.length
    ? Option.some(makeValid(source.id, start, end))
    : Option.none()

const makeValid = (sourceId: string, start: number, end: number): SourceSpan => {
  const span: SourceSpan = {
    [SourceSpanTypeId]: SourceSpanTypeId,
    sourceId,
    start,
    end,
  }
  return Object.freeze(span)
}

/** Returns the logical source identity that owns the span. */
export const sourceId = (self: SourceSpan): string => self.sourceId

/** Returns the inclusive byte offset at which the span begins. */
export const start = (self: SourceSpan): number => self.start

/** Returns the exclusive byte offset at which the span ends. */
export const end = (self: SourceSpan): number => self.end

/** Returns the number of bytes covered by the span. */
export const length = (self: SourceSpan): number => self.end - self.start

/** Tests structural equality, including source ownership. */
export const equals = (self: SourceSpan, other: SourceSpan): boolean =>
  self.sourceId === other.sourceId && self.start === other.start && self.end === other.end
