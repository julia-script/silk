import { assert, it } from '@effect/vitest'
import * as Option from 'effect/Option'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceSpan from '../src/SourceSpan.js'

const span = (source: SourceFile.SourceFile, start: number, end: number): SourceSpan.SourceSpan =>
  Option.getOrThrowWith(
    SourceSpan.make(source, start, end),
    () => new Error(`invalid test span [${start}, ${end})`),
  )

it('preserves arbitrary bytes and isolates snapshots from caller mutation', () => {
  const input = Uint8Array.of(0xff, 0x00, 0xc3, 0x28, 0x0d, 0x0a)
  const source = SourceFile.make('memory://invalid-utf8.silk', input)
  input[0] = 0x61

  assert.deepEqual(
    SourceFile.toUint8Array(source),
    Uint8Array.of(0xff, 0x00, 0xc3, 0x28, 0x0d, 0x0a),
  )
  const returned = SourceFile.toUint8Array(source)
  returned[1] = 0x62
  assert.strictEqual(SourceFile.toUint8Array(source)[1], 0x00)
})

it('uses explicit logical identity instead of byte equality', () => {
  const left = SourceFile.make('memory://left.silk', Uint8Array.of(0x61))
  const same = SourceFile.make('memory://left.silk', Uint8Array.of(0x61))
  const right = SourceFile.make('memory://right.silk', Uint8Array.of(0x61))

  assert.strictEqual(SourceFile.equals(left, same), true)
  assert.strictEqual(SourceFile.equals(left, right), false)
  assert.strictEqual(SourceFile.identity(left), 'memory://left.silk')
})

it('models half-open ranges, empty files, and EOF positions', () => {
  const empty = SourceFile.make('memory://empty.silk', new Uint8Array())
  const eof = span(empty, 0, 0)
  const sameIdentity = SourceFile.make('memory://empty.silk', new Uint8Array())
  const other = SourceFile.make('memory://other.silk', new Uint8Array())

  assert.strictEqual(SourceFile.length(empty), 0)
  assert.strictEqual(SourceSpan.length(eof), 0)
  assert.strictEqual(SourceSpan.start(eof), 0)
  assert.strictEqual(SourceSpan.end(eof), 0)
  assert.strictEqual(SourceSpan.sourceId(eof), SourceFile.identity(empty))
  assert.strictEqual(SourceSpan.equals(eof, span(sameIdentity, 0, 0)), true)
  assert.strictEqual(SourceSpan.equals(eof, span(other, 0, 0)), false)

  assert.strictEqual(Option.isNone(SourceSpan.make(empty, -1, 0)), true)
  assert.strictEqual(Option.isNone(SourceSpan.make(empty, 1, 0)), true)
  assert.strictEqual(Option.isNone(SourceSpan.make(empty, 0.5, 1)), true)
  assert.strictEqual(Option.isNone(SourceSpan.make(empty, 0, 1)), true)
})

it('returns exact slice copies only for spans owned by the source', () => {
  const source = SourceFile.make('memory://slice.silk', Uint8Array.of(0x61, 0x00, 0x62))
  const foreign = SourceFile.make('memory://foreign.silk', Uint8Array.of(0x61, 0x00, 0x62))
  const longerSameIdentity = SourceFile.make(
    'memory://slice.silk',
    Uint8Array.of(0x61, 0x00, 0x62, 0x63),
  )
  const selected = Option.getOrThrow(SourceFile.slice(source, span(source, 1, 3)))
  selected[0] = 0xff

  assert.deepEqual(
    Option.getOrThrow(SourceFile.slice(source, span(source, 1, 3))),
    Uint8Array.of(0x00, 0x62),
  )
  assert.strictEqual(Option.isNone(SourceFile.slice(source, span(foreign, 1, 3))), true)
  assert.strictEqual(Option.isNone(SourceFile.slice(source, span(longerSameIdentity, 0, 4))), true)
})
