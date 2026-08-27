import type * as SourceSpan from '@silk-lang/compiler/SourceSpan'
import type { Position, Range } from 'vscode-languageserver-types'

const decoder = new TextDecoder()
const encoder = new TextEncoder()

/** A byte-oriented line map translating compiler spans into UTF-16 protocol positions. */
export interface LineIndex {
  readonly _tag: 'LineIndex'
  readonly bytes: Uint8Array
  readonly lineStarts: ReadonlyArray<number>
}

/** Builds the line map of one immutable source byte sequence. */
export const make = (bytes: Uint8Array): LineIndex => {
  const lineStarts = [0]
  for (let index = 0; index < bytes.length; index += 1) {
    if (bytes[index] === 0x0a) lineStarts.push(index + 1)
  }
  return Object.freeze({
    _tag: 'LineIndex',
    bytes: Uint8Array.from(bytes),
    lineStarts: Object.freeze(lineStarts),
  })
}

/** Returns the zero-based line owning one clamped byte offset. */
export const lineOf = (self: LineIndex, offset: number): number => {
  let low = 0
  let high = self.lineStarts.length - 1
  while (low < high) {
    const middle = Math.ceil((low + high) / 2)
    if ((self.lineStarts[middle] ?? 0) <= offset) low = middle
    else high = middle - 1
  }
  return low
}

const lineEnd = (self: LineIndex, line: number): number =>
  self.lineStarts[line + 1] ?? self.bytes.length

/** Translates one byte offset into a protocol position with a UTF-16 character column. */
export const positionOf = (self: LineIndex, offset: number): Position => {
  const clamped = Math.max(0, Math.min(offset, self.bytes.length))
  const line = lineOf(self, clamped)
  const start = self.lineStarts[line] ?? 0
  const character = decoder.decode(self.bytes.subarray(start, clamped)).length
  return { line, character }
}

/** Translates one protocol position into a clamped byte offset. */
export const offsetOf = (self: LineIndex, position: Position): number => {
  if (position.line < 0) return 0
  if (position.line >= self.lineStarts.length) return self.bytes.length
  const start = self.lineStarts[position.line] ?? 0
  const text = decoder.decode(self.bytes.subarray(start, lineEnd(self, position.line)))
  const units = Math.max(0, Math.min(position.character, text.length))
  return start + encoder.encode(text.slice(0, units)).length
}

/** Translates one compiler source span into a protocol range. */
export const rangeOf = (self: LineIndex, span: SourceSpan.SourceSpan): Range => ({
  start: positionOf(self, span.start),
  end: positionOf(self, span.end),
})

/** The whole document as one protocol range, for full-document replacements. */
export const fullRange = (self: LineIndex): Range => ({
  start: { line: 0, character: 0 },
  end: positionOf(self, self.bytes.length),
})
