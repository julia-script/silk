import { mkdirSync, mkdtempSync, renameSync, rmSync, symlinkSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { pathToFileURL } from 'node:url'
import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as WorkspaceInventory from '@silk-lang/compiler/WorkspaceInventory'
import * as Effect from 'effect/Effect'
import * as Document from '../src/Document.js'
import * as WorkspaceCatalog from '../src/WorkspaceCatalog.js'

const encoder = new TextEncoder()

const fixture = (): string => {
  const root = mkdtempSync(join(tmpdir(), 'silk-catalog-'))
  mkdirSync(join(root, 'nested'))
  writeFileSync(join(root, 'Main.silk'), 'pub fn main() -> i32 { return 1 }')
  writeFileSync(join(root, 'nested', 'Util.silk'), 'pub fn diskValue() -> i32 { return 2 }')
  symlinkSync(join(root, 'nested', 'Util.silk'), join(root, 'Alias.silk'))
  return root
}

it.effect(
  'enumerates canonical files in order, prefers open bytes, and includes toolchain summaries',
  () =>
    Effect.gen(function* () {
      const root = fixture()
      const util = Document.make({
        uri: pathToFileURL(join(root, 'nested', 'Util.silk')).href,
        version: 2,
        workspace: 'fixture',
        module: 'nested/Util',
        sourceRoot: root,
        bytes: encoder.encode('pub fn bufferedValue() -> i32 { return 3 }'),
      })
      const inventory = yield* WorkspaceCatalog.refresh({
        sourceRoot: root,
        documents: [util],
        invalidation: { dirtyPaths: [], rediscover: true },
      })

      assert.deepEqual([...inventory.project.keys()], ['Main', 'nested/Util'])
      assert.deepEqual(
        WorkspaceInventory.candidates(inventory, 'bufferedValue').map(
          (candidate) => candidate.module,
        ),
        ['nested/Util'],
      )
      assert.deepEqual(WorkspaceInventory.candidates(inventory, 'diskValue'), [])
      assert.isAtLeast(WorkspaceInventory.candidates(inventory, 'Bytes').length, 1)
      assert.strictEqual(inventory.project.get('nested/Util')?.source.origin._tag, 'Memory')
      assert.strictEqual(inventory.integrity._tag, 'Matched')
      assert.strictEqual(inventory.distribution.digest.length, 64)
    }).pipe(Effect.provide(NodeServices.layer)),
)

it.effect('revises exact dirty files, removes deletions, and reuses unrelated summaries', () =>
  Effect.gen(function* () {
    const root = fixture()
    const initial = yield* WorkspaceCatalog.refresh({
      sourceRoot: root,
      documents: [],
      invalidation: { dirtyPaths: [], rediscover: true },
    })
    const main = initial.project.get('Main')
    const util = initial.project.get('nested/Util')
    writeFileSync(join(root, 'Main.silk'), 'pub fn revised() -> i32 { return 4 }')
    const revised = yield* WorkspaceCatalog.refresh({
      sourceRoot: root,
      documents: [],
      previous: initial,
      invalidation: { dirtyPaths: [join(root, 'Main.silk')], rediscover: false },
    })
    assert.strictEqual(revised.distribution, initial.distribution)
    assert.isFalse(revised.project.get('Main') === main)
    assert.isTrue(revised.project.get('nested/Util') === util)
    assert.deepEqual(
      WorkspaceInventory.candidates(revised, 'revised').map((value) => value.module),
      ['Main'],
    )

    rmSync(join(root, 'nested', 'Util.silk'))
    const removed = yield* WorkspaceCatalog.refresh({
      sourceRoot: root,
      documents: [],
      previous: revised,
      invalidation: { dirtyPaths: [join(root, 'nested', 'Util.silk')], rediscover: false },
    })
    assert.strictEqual(removed.project.has('nested/Util'), false)
    assert.strictEqual(removed.observation.removed, 1)

    renameSync(join(root, 'Main.silk'), join(root, 'Renamed.silk'))
    const renamed = yield* WorkspaceCatalog.refresh({
      sourceRoot: root,
      documents: [],
      previous: removed,
      invalidation: {
        dirtyPaths: [join(root, 'Main.silk'), join(root, 'Renamed.silk')],
        rediscover: false,
      },
    })
    assert.deepEqual([...renamed.project.keys()], ['Renamed'])
  }).pipe(Effect.provide(NodeServices.layer)),
)
