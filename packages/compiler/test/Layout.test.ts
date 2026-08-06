import { assert, it } from '@effect/vitest'
import * as Analysis from '../src/Analysis.js'
import * as Layout from '../src/Layout.js'
import * as Target from '../src/Target.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

it('plans only concrete types reached through discovered instances', () => {
  const snapshot = Analysis.ofSource(
    'layout/program',
    ascii(`pub fn unused(value: Bool) -> Bool { return value }
pub fn main() -> I32 { return 42 }`),
  )
  const plan = Layout.plan(Target.aarch64AppleDarwin, Analysis.instancesOf(snapshot))

  assert.deepEqual(
    plan.entries.map((candidate) => candidate.type),
    ['I32'],
  )
  assert.deepEqual(Layout.verify(plan), [])
})

it('orders and encodes canonical scalar entries identically on every target', () => {
  for (const target of Target.all) {
    const first = Layout.make(target, ['I32', 'Bool', 'I32'])
    const second = Layout.make(target, ['Bool', 'I32'])
    assert.deepEqual(
      first.entries.map((candidate) => candidate.type),
      ['Bool', 'I32'],
    )
    assert.strictEqual(Layout.encode(first), Layout.encode(second))
    assert.deepEqual(Layout.verify(first), [])
  }
})

it('reports malformed target, order, duplicates, and scalar facts as data', () => {
  const canonical = Layout.make(Target.aarch64AppleDarwin, ['Bool', 'I32'])
  const bool = canonical.entries.at(0)
  const i32 = canonical.entries.at(1)
  if (bool === undefined || i32 === undefined) throw new Error('expected scalar layouts')
  const malformed: Layout.Plan = {
    _tag: 'LayoutPlan',
    target: { ...Target.aarch64AppleDarwin, pointerSize: 4 },
    entries: [i32, { ...bool, size: 1 }, bool],
  }

  assert.deepEqual(
    Layout.verify(malformed).map((violation) => violation.rule),
    ['NonCanonicalTarget', 'NonCanonicalOrder', 'InvalidScalar', 'DuplicateType'],
  )
})
