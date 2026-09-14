import { assert, it } from '@effect/vitest'
import * as AncestorHistory from '../src/AncestorHistory.js'

const assignments = (history: AncestorHistory.History): ReadonlyArray<string> => {
  const visit = (
    node: AncestorHistory.History,
  ): ReadonlyArray<ReadonlyArray<readonly [string, string]>> => {
    if (node.variable === undefined) return node.id === 0 ? [] : [[]]
    const variable = node.variable
    return [...node.branches].flatMap(([value, child]) =>
      visit(child).map((suffix) =>
        value === undefined ? suffix : [[variable, value] as const, ...suffix],
      ),
    )
  }
  return visit(history)
    .map((entries) => JSON.stringify(entries))
    .sort()
}

it('preserves exact correlated assignments across union, overwrite, and partition', () => {
  const catalog = AncestorHistory.make()
  let history = catalog.empty
  let oracle: Array<Map<string, string>> = [
    new Map(), // All declarations absent.
    new Map([
      ['a', '0'],
      ['b', '0'],
    ]), // Correlated first path.
    new Map([
      ['a', '1'],
      ['b', '1'],
    ]), // Correlated second path.
    new Map([
      ['a', '0'],
      ['b', '1'],
    ]), // Collapses with the first path on overwrite.
    new Map([['c', '0']]), // Missing earlier declarations, present later declaration.
  ]
  for (const entry of oracle) {
    let branch = catalog.initial
    for (const [variable, value] of entry)
      branch = AncestorHistory.set(catalog, branch, variable, value)
    history = AncestorHistory.union(catalog, history, branch)
  }
  const expected = (rows: ReadonlyArray<ReadonlyMap<string, string>>) =>
    [
      ...new Set(
        rows.map((row) => JSON.stringify([...row].sort(([a], [b]) => a.localeCompare(b)))),
      ),
    ].sort()
  assert.deepEqual(assignments(history), expected(oracle))
  assert.strictEqual(AncestorHistory.union(catalog, history, history), history)
  for (const variable of ['a', 'b', 'c', 'absent']) {
    const parts = AncestorHistory.partition(catalog, history, variable)
    let reconstructed = catalog.empty
    for (const [value, part] of parts) {
      assert.deepEqual(
        assignments(part),
        expected(oracle.filter((row) => row.get(variable) === value)),
      )
      reconstructed = AncestorHistory.union(catalog, reconstructed, part)
    }
    assert.strictEqual(reconstructed, history)
  }
  history = AncestorHistory.set(catalog, history, 'b', 'replacement')
  oracle = oracle.map((row) => new Map(row).set('b', 'replacement'))
  assert.deepEqual(assignments(history), expected(oracle))

  const first = AncestorHistory.set(
    catalog,
    AncestorHistory.set(catalog, catalog.initial, 'a', '0'),
    'b',
    '0',
  )
  const second = AncestorHistory.set(
    catalog,
    AncestorHistory.set(catalog, catalog.initial, 'a', '1'),
    'b',
    '1',
  )
  const diagonal = AncestorHistory.union(catalog, first, second)
  const filtered = AncestorHistory.partition(catalog, diagonal, 'a').get('0')
  assert.ok(filtered)
  assert.deepEqual([...AncestorHistory.partition(catalog, filtered, 'b').keys()], ['0'])
  assert.strictEqual(
    AncestorHistory.set(
      catalog,
      AncestorHistory.union(catalog, first, AncestorHistory.set(catalog, first, 'a', '1')),
      'a',
      '0',
    ),
    first,
  )
})

it('shares diamond histories without enumerating their combinations', () => {
  const catalog = AncestorHistory.make()
  let history = catalog.initial
  for (let ordinal = 0; ordinal < 3; ordinal += 1) {
    const variable = String(ordinal).padStart(2, '0')
    history = AncestorHistory.union(
      catalog,
      AncestorHistory.set(catalog, history, variable, 'left'),
      AncestorHistory.set(catalog, history, variable, 'right'),
    )
  }
  const reachable = new Set<number>()
  const visit = (node: AncestorHistory.History): void => {
    if (reachable.has(node.id)) return
    reachable.add(node.id)
    for (const child of node.branches.values()) visit(child)
  }
  visit(history)
  assert.strictEqual(reachable.size, 4)
  const selected = AncestorHistory.partition(catalog, history, '01').get('left')
  assert.ok(selected)
  assert.strictEqual(AncestorHistory.partition(catalog, selected, '01').size, 1)
  assert.strictEqual(AncestorHistory.partition(catalog, selected, '02').size, 2)
})
