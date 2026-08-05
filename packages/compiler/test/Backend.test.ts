import { createHash } from 'node:crypto'
import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Analysis from '../src/Analysis.js'
import * as Backend from '../src/Backend.js'
import type * as Mir from '../src/Mir.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const layout: Mir.TargetLayout = {
  _tag: 'TargetLayout',
  triple: 'arm64-apple-darwin',
  pointerWidth: 64,
  endianness: 'little',
  i32: { size: 4, alignment: 4 },
}

const nestedSource = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`

const emit = (text: string, request: Backend.CodegenRequest): Backend.Artifact => {
  const snapshot = Analysis.ofSource('golden://program.silk', ascii(text))
  return Backend.LlvmBackend.emit(Analysis.loweredMir(snapshot), layout, request)
}

const golden = (name: string): string =>
  readFileSync(new URL(`./goldens/${name}`, import.meta.url), 'utf8')

it('emits one artifact per program with deterministic symbols', () => {
  const artifact = emit(nestedSource, { mode: 'release' })

  assert.strictEqual(artifact.module, 'golden://program.silk')
  assert.deepEqual(
    artifact.symbols.map((entry) => entry.symbol),
    ['silk_main', 'silk_1_identity'],
  )
  assert.strictEqual(artifact.symbols.at(0)?.declaration.name, 'main')
})

it('matches the IR golden and the bitcode digest golden', () => {
  const artifact = emit(nestedSource, { mode: 'release' })

  assert.strictEqual(artifact.ir, golden('program.ll.txt'))
  assert.strictEqual(
    `${createHash('sha256').update(artifact.bitcode).digest('hex')}\n`,
    golden('program.bc.sha256'),
  )
})

it('emits byte-identical bitcode across repeated fresh runs', () => {
  const first = emit(nestedSource, { mode: 'release' })
  const second = emit(nestedSource, { mode: 'release' })

  assert.deepEqual(first.bitcode, second.bitcode)
  assert.strictEqual(first.ir, second.ir)
})

it('lowers trap bodies to an unreachable trap sequence', () => {
  const artifact = emit('pub fn main() -> I32 { return missing() }', { mode: 'release' })

  assert.include(artifact.ir, '@llvm.trap()')
  assert.include(artifact.ir, 'unreachable')
  assert.notInclude(artifact.ir, 'ret i32 0')
})

it('emits native debug metadata only for debug requests', () => {
  const debug = emit(nestedSource, {
    mode: 'debug',
    sources: new Map([['golden://program.silk', ascii(nestedSource)]]),
  })
  const release = emit(nestedSource, { mode: 'release' })

  assert.include(debug.ir, '!DICompileUnit(')
  assert.include(debug.ir, '!DISubprogram(')
  assert.include(debug.ir, '!dbg')
  assert.notInclude(release.ir, 'DICompileUnit')
  assert.notInclude(release.ir, '!dbg')
})

const arithmeticSource = 'pub fn main() -> I32 { return I32.subtract(I32.multiply(6, 7), 0) }'

it('emits checked arithmetic through overflow intrinsics and guarded division', () => {
  const artifact = emit(arithmeticSource, { mode: 'release' })
  const division = emit('pub fn main() -> I32 { return I32.divide(1, 0) }', { mode: 'release' })

  assert.include(artifact.ir, 'llvm.smul.with.overflow')
  assert.include(artifact.ir, 'llvm.ssub.with.overflow')
  assert.include(artifact.ir, 'arith_trap')
  assert.include(division.ir, 'sdiv')
  assert.include(division.ir, 'icmp eq')
  assert.include(division.ir, '@llvm.trap()')
})

it('matches the arithmetic IR golden and stays deterministic', () => {
  const first = emit(arithmeticSource, { mode: 'release' })
  const second = emit(arithmeticSource, { mode: 'release' })

  assert.strictEqual(first.ir, golden('arithmetic.ll.txt'))
  assert.deepEqual(first.bitcode, second.bitcode)
})
