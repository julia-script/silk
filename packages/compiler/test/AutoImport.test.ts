import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as AutoImport from '../src/AutoImport.js'
import * as Lexer from '../src/Lexer.js'
import * as ModuleSummary from '../src/ModuleSummary.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import * as WorkspaceInventory from '../src/WorkspaceInventory.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const summary = (module: string, text: string): ModuleSummary.ModuleSummary =>
  ModuleSummary.make(Parser.parse(Lexer.lex(SourceFile.make(module, ascii(text)))))

it.effect('discovers separate project and toolchain candidates and resolves the chosen plan', () =>
  Effect.gen(function* () {
    const rootText = 'fn main() -> i32 { return calculate() }'
    const snapshot = yield* Analysis.ofSource('main', ascii(rootText))
    const inventory = WorkspaceInventory.make({
      project: [
        ['main', summary('main', rootText)],
        ['app.math', summary('app.math', 'pub fn calculate() -> i32 { return 1 }')],
      ],
      toolchain: [['silk.math', summary('silk.math', 'pub fn calculate() -> i32 { return 2 }')]],
    })
    const offset = rootText.indexOf('calculate') + 1
    const actions = Analysis.autoImportsAt(snapshot, inventory, 'main', offset)

    assert.deepEqual(
      actions.map((action) => action.descriptor.title),
      ['Import calculate from app.math', 'Import calculate from silk.math'],
    )
    const selected = actions.at(0)
    assert.notStrictEqual(selected, undefined)
    if (selected === undefined) return
    const plan = Analysis.resolveAutoImport(snapshot, inventory, 'main', offset, selected.candidate)
    assert.deepEqual(
      plan.pipe((value) => value._tag),
      'Some',
    )
  }),
)

it.effect('filters candidates by semantic namespace and omits private declarations', () =>
  Effect.gen(function* () {
    const rootText = 'fn accept(value: Wanted) -> () {}'
    const snapshot = yield* Analysis.ofSource('main', ascii(rootText))
    const inventory = WorkspaceInventory.make({
      project: [
        ['main', summary('main', rootText)],
        ['wrong', summary('wrong', 'pub fn Wanted() -> i32 { return 1 }')],
        ['right', summary('right', 'pub union Wanted { Value }')],
        ['private', summary('private', 'struct Wanted {}')],
      ],
    })

    const actions = AutoImport.discover({
      snapshot,
      inventory,
      module: 'main',
      byteOffset: rootText.indexOf('Wanted') + 1,
    })
    assert.deepEqual(
      actions.map((action) => action.candidate.module),
      ['right'],
    )
  }),
)

it.effect('does not offer an import when the occurrence is already resolved', () =>
  Effect.gen(function* () {
    const rootText = 'fn calculate() -> i32 { return 1 }\nfn main() -> i32 { return calculate() }'
    const snapshot = yield* Analysis.ofSource('main', ascii(rootText))
    const inventory = WorkspaceInventory.make({
      project: [['other', summary('other', 'pub fn calculate() -> i32 { return 2 }')]],
    })
    assert.deepEqual(
      Analysis.autoImportsAt(snapshot, inventory, 'main', rootText.lastIndexOf('calculate') + 1),
      [],
    )
  }),
)
