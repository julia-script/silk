import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as MirEncoding from '../src/MirEncoding.js'
import { nativeCorpus } from './support/corpus.js'

const encoder = new TextEncoder()

const sourceOf = (name: string): string => {
  const program = nativeCorpus.find((candidate) => candidate.name === name)
  if (program === undefined) throw new RangeError(`Missing native corpus case ${name}`)
  return program.source
}

it.effect('lowers raw-storage copies to defined overlapping moves', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'bulk-memory/copy-range',
      encoder.encode(sourceOf('raw-buffer-copy-range')),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.include(MirEncoding.encode(Analysis.loweredMir(snapshot)), 'raw-buffer-copy')
    const llvm = yield* Analysis.codegen(snapshot, { mode: 'release' })
    assert.include(llvm.ir, 'llvm.memmove')
  }),
)

it.effect('lowers raw-storage fills to LLVM memset', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'bulk-memory/fill-range',
      encoder.encode(sourceOf('raw-buffer-fill-range')),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.include(MirEncoding.encode(Analysis.loweredMir(snapshot)), 'raw-buffer-fill')
    const llvm = yield* Analysis.codegen(snapshot, { mode: 'release' })
    assert.include(llvm.ir, 'llvm.memset')
  }),
)
