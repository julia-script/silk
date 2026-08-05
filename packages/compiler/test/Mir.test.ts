import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Mir from '../src/Mir.js'

const golden = (name: string): string =>
  readFileSync(new URL(`./goldens/${name}`, import.meta.url), 'utf8')

const raise = (message: string): never => {
  throw new Error(message)
}

it('verifies the hand-built samples clean', () => {
  for (const sample of Mir.samples()) {
    assert.deepEqual(Mir.verify(sample), [])
  }
})

it('reports broken graphs deterministically as data', () => {
  const [straight] = Mir.samples()
  const fn = straight?.functions.at(1) ?? raise('expected the sample main function')
  const broken: Mir.Module = {
    _tag: 'MirModule',
    module: 'sample://broken.silk',
    layout: straight?.layout ?? raise('expected the sample layout'),
    functions: [
      { ...fn, blocks: [] },
      {
        ...fn,
        blocks: fn.blocks.map((block) => ({
          ...block,
          terminator: {
            _tag: 'Jump',
            target: { _tag: 'Block', ordinal: 9 },
            provenance:
              block.terminator._tag === 'Return'
                ? { span: block.terminator.provenance.span, generated: true }
                : raise('expected a return terminator'),
          },
          operations: block.operations.map((operation) =>
            operation._tag === 'Call'
              ? { ...operation, arguments: [{ _tag: 'Local' as const, ordinal: 7 }] }
              : operation,
          ),
        })),
      },
    ],
  }

  const violations = Mir.verify(broken)
  assert.deepEqual(
    violations.map((violation) => violation.rule),
    ['MissingEntryBlock', 'UnknownBlockTarget', 'UndeclaredLocal'],
  )
  assert.deepEqual(Mir.verify(broken), violations)
})

it('carries and encodes exactly one compiler-owned target layout plan', () => {
  const [straight] = Mir.samples()
  const sample = straight ?? raise('expected sample')

  assert.strictEqual(sample.layout.entries.at(0)?.size, 4)
  assert.include(Mir.encode(sample), 'target aarch64-apple-darwin')
  assert.include(Mir.encode(sample), 'layout I32 size=4 align=4 repr=signed-i32')
})

it('marks generated operations and preserves programmer provenance', () => {
  const [, branching] = Mir.samples()
  const encoded = Mir.encode(branching ?? raise('expected branching sample'))

  assert.include(encoded, 'drop %0 [34, 45) generated')
  assert.include(encoded, '%1 = move %0 [41, 45)\n')
  assert.notInclude(encoded, 'move %0 [41, 45) generated')
})

it('matches the MIR golden encodings byte-for-byte', () => {
  const [straight, branching] = Mir.samples()

  assert.strictEqual(Mir.encode(straight ?? raise('expected sample')), golden('straight.mir.txt'))
  assert.strictEqual(Mir.encode(branching ?? raise('expected sample')), golden('branching.mir.txt'))
})

it('constructs and encodes byte-identically across repeated runs', () => {
  const first = Mir.samples()
  const second = Mir.samples()

  assert.deepEqual(first, second)
  assert.deepEqual(first.map(Mir.encode), second.map(Mir.encode))
})
