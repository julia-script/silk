import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Analysis from '../src/Analysis.js'
import * as Mir from '../src/Mir.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const snapshot = (text: string): Analysis.Snapshot =>
  Analysis.ofSource('golden/program', ascii(text))

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

const bindingSource = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { let value = identity(42) let extra = 1 return value }`

it('lowers bindings to typed locals with generated exit drops in release order', () => {
  const program = Analysis.loweredMir(snapshot(bindingSource))
  const main = program.functions.at(0)

  assert.deepEqual(Mir.verify(program), [])
  const operations = main?.blocks.at(0)?.operations ?? []
  assert.deepEqual(
    operations.map((operation) => operation._tag),
    ['Literal', 'Call', 'Move', 'Literal', 'Move', 'Drop', 'Drop'],
  )
  const drops = operations.filter((operation) => operation._tag === 'Drop')
  assert.strictEqual(
    drops.every((drop) => drop.provenance.generated),
    true,
  )
  const moves = operations.flatMap((operation) =>
    operation._tag === 'Move' ? [operation.destination.ordinal] : [],
  )
  assert.deepEqual(
    drops.map((drop) => (drop._tag === 'Drop' ? drop.local.ordinal : -1)),
    [moves.at(1), moves.at(0)],
  )
})

it('lowers an ownership violation to a generated trap', () => {
  const program = Analysis.loweredMir(
    snapshot(`pub fn choose(left: I32, right: I32) -> I32 { return right }
pub fn main() -> I32 { let value = 42 return choose(move value, value) }`),
  )
  const main = program.functions.at(0)

  assert.strictEqual(main?.blocks.at(0)?.terminator._tag, 'Trap')
  const terminator = main?.blocks.at(0)?.terminator
  assert.strictEqual(terminator?._tag === 'Trap' ? terminator.reason : '', 'ownership violation')
  assert.strictEqual(terminator?.provenance.generated, true)
  assert.deepEqual(Mir.verify(program), [])
})

it('matches the binding lowered golden encoding byte-for-byte', () => {
  const encoded = Mir.encode(Analysis.loweredMir(snapshot(bindingSource)))

  assert.strictEqual(encoded, golden('bindings.mir.txt'))
})

it('lowers built-in calls to trapping binary operations', () => {
  const program = Analysis.loweredMir(
    snapshot('pub fn main() -> I32 { return I32.subtract(I32.multiply(6, 7), 0) }'),
  )
  const main = program.functions.at(0)

  assert.deepEqual(Mir.verify(program), [])
  const operations = main?.blocks.at(0)?.operations ?? []
  assert.deepEqual(
    operations.map((operation) =>
      operation._tag === 'Binary' ? `Binary:${operation.operator}` : operation._tag,
    ),
    ['Literal', 'Literal', 'Binary:Multiply', 'Literal', 'Binary:Subtract'],
  )
  assert.strictEqual(Mir.encode(program).includes('= multiply'), true)
})

const branchProgram =
  'pub fn main() -> I32 { let base = 40 if I32.equals(base, 40) { let bonus = 2 return I32.add(base, bonus) } return 0 }'

it('lowers conditionals to branch diamonds with arm drops', () => {
  const program = Analysis.loweredMir(snapshot(branchProgram))
  const main = program.functions.at(0)

  assert.deepEqual(Mir.verify(program), [])
  const blocks = main?.blocks ?? []
  assert.strictEqual(blocks.length, 3)
  assert.strictEqual(blocks.at(0)?.terminator._tag, 'Branch')
  const entryTerminator = blocks.at(0)?.terminator
  if (entryTerminator?._tag === 'Branch') {
    assert.strictEqual(entryTerminator.taken.ordinal, 1)
    assert.strictEqual(entryTerminator.otherwise.ordinal, 2)
    assert.strictEqual(entryTerminator.provenance.generated, false)
  }
  assert.strictEqual(blocks.at(1)?.terminator._tag, 'Return')
  const armDrops = blocks.at(1)?.operations.filter((operation) => operation._tag === 'Drop')
  assert.strictEqual(armDrops?.length, 2)
  assert.strictEqual(blocks.at(2)?.terminator._tag, 'Return')
})

it('matches the branching lowered golden encoding byte-for-byte', () => {
  const first = Mir.encode(Analysis.loweredMir(snapshot(branchProgram)))
  const second = Mir.encode(Analysis.loweredMir(snapshot(branchProgram)))

  assert.strictEqual(first, golden('branch-program.mir.txt'))
  assert.strictEqual(first, second)
})
