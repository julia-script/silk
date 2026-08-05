import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Analysis from '../src/Analysis.js'
import * as Mir from '../src/Mir.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const snapshot = (text: string): Analysis.Snapshot =>
  Analysis.ofSource('golden://program.silk', ascii(text))

const golden = (name: string): string =>
  readFileSync(new URL(`./goldens/${name}`, import.meta.url), 'utf8')

const nestedSource = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`

it('discovers a call chain once each in discovery order', () => {
  const discovery = Analysis.instancesOf(snapshot(nestedSource))

  assert.strictEqual(discovery.entry._tag, 'Resolved')
  assert.deepEqual(
    discovery.instances.map((instance) => instance.key.declaration.name),
    ['main', 'identity'],
  )
  assert.deepEqual(discovery.instances.at(0)?.key.typeArguments, [])
  assert.deepEqual(discovery.instances.at(0)?.key.contractRow, [])
})

it('terminates on direct and mutual recursion', () => {
  const direct = Analysis.instancesOf(snapshot('pub fn main() -> I32 { return main() }'))
  const mutual = Analysis.instancesOf(
    snapshot(`pub fn main() -> I32 { return other() }
pub fn other() -> I32 { return main() }`),
  )

  assert.deepEqual(
    direct.instances.map((instance) => instance.key.declaration.name),
    ['main'],
  )
  assert.deepEqual(
    mutual.instances.map((instance) => instance.key.declaration.name),
    ['main', 'other'],
  )
})

it('excludes unreachable declarations', () => {
  const discovery = Analysis.instancesOf(
    snapshot(`pub fn unused() -> I32 { return 1 }
pub fn main() -> I32 { return 42 }`),
  )

  assert.deepEqual(
    discovery.instances.map((instance) => instance.key.declaration.name),
    ['main'],
  )
})

it('reports unavailable entries without recording instances', () => {
  const missing = Analysis.instancesOf(snapshot('pub fn answer() -> I32 { return 42 }'))
  const parameterized = Analysis.instancesOf(
    snapshot('pub fn main(value: I32) -> I32 { return value }'),
  )

  assert.deepEqual(missing.entry, { _tag: 'Unavailable', reason: 'MissingEntry' })
  assert.deepEqual(missing.instances, [])
  assert.deepEqual(parameterized.entry, { _tag: 'Unavailable', reason: 'ParameterizedEntry' })
})

it('lowers discovered instances into a verifier-clean program', () => {
  const program = Analysis.loweredMir(snapshot(nestedSource))

  assert.deepEqual(Mir.verify(program), [])
  assert.deepEqual(
    program.functions.map((fn) => fn.id.name),
    ['main', 'identity'],
  )
  const main = program.functions.at(0)
  assert.deepEqual(
    main?.blocks.at(0)?.operations.map((operation) => operation._tag),
    ['Literal', 'Call', 'Call'],
  )
})

it('lowers unavailable bodies to generated traps', () => {
  const program = Analysis.loweredMir(snapshot('pub fn main() -> I32 { return missing() }'))
  const main = program.functions.at(0)

  assert.strictEqual(main?.blocks.at(0)?.terminator._tag, 'Trap')
  assert.strictEqual(main?.blocks.at(0)?.terminator.provenance.generated, true)
  assert.deepEqual(Mir.verify(program), [])
})

it('matches the lowered golden encoding byte-for-byte across runs', () => {
  const first = Mir.encode(Analysis.loweredMir(snapshot(nestedSource)))
  const second = Mir.encode(Analysis.loweredMir(snapshot(nestedSource)))

  assert.strictEqual(first, golden('lowered.mir.txt'))
  assert.strictEqual(first, second)
})
