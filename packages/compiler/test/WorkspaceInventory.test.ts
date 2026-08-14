import { assert, it } from '@effect/vitest'
import * as Lexer from '../src/Lexer.js'
import * as ModuleSummary from '../src/ModuleSummary.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import * as WorkspaceInventory from '../src/WorkspaceInventory.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const summary = (module: string, text: string): ModuleSummary.ModuleSummary =>
  ModuleSummary.make(Parser.parse(Lexer.lex(SourceFile.make(module, ascii(text)))))

it('keeps tiers separate and exact lookup deterministic', () => {
  const inventory = WorkspaceInventory.make({
    project: [
      ['z.local', summary('z.local', 'pub fn find() -> i32 { return 1 }')],
      ['a.local', summary('a.local', 'pub fn find() -> i32 { return 2 }')],
    ],
    toolchain: [['silk.search', summary('silk.search', 'pub fn find() -> i32 { return 3 }')]],
  })

  assert.deepEqual(
    WorkspaceInventory.candidates(inventory, 'find').map(({ tier, module }) => ({ tier, module })),
    [
      { tier: 'Project', module: 'a.local' },
      { tier: 'Project', module: 'z.local' },
      { tier: 'Toolchain', module: 'silk.search' },
    ],
  )
  assert.deepEqual(WorkspaceInventory.candidates(inventory, 'Find'), [])
})

it('revises only changed modules and preserves every unaffected summary', () => {
  const first = summary('a', 'pub fn first() -> i32 { return 1 }')
  const second = summary('b', 'pub fn second() -> i32 { return 2 }')
  const initial = WorkspaceInventory.make({
    project: [
      ['a', first],
      ['b', second],
    ],
  })
  const replacement = summary('a', 'pub fn revised() -> i32 { return 1 }')
  const revised = WorkspaceInventory.revise(initial, {
    project: [['a', replacement]],
    removeProject: ['missing'],
  })

  assert.strictEqual(revised.project.get('b'), second)
  assert.strictEqual(revised.project.get('a'), replacement)
  assert.deepEqual(WorkspaceInventory.candidates(revised, 'first'), [])
  assert.deepEqual(
    WorkspaceInventory.candidates(revised, 'revised').map(({ module }) => module),
    ['a'],
  )

  const reused = WorkspaceInventory.revise(revised, { project: [['a', replacement]] })
  assert.strictEqual(reused, revised)
})
