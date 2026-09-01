import { mkdirSync, mkdtempSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { pathToFileURL } from 'node:url'
import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as Analysis from '@silklang/compiler/Analysis'
import * as WorkspaceInventory from '@silklang/compiler/WorkspaceInventory'
import * as Effect from 'effect/Effect'
import * as Inspectable from 'effect/Inspectable'
import * as Workspace from '../src/Workspace.js'
import * as WorkspaceCatalog from '../src/WorkspaceCatalog.js'

const encoder = new TextEncoder()

it.effect(
  'keeps exact lookup header-only and reuses every unaffected module in a large workspace',
  () =>
    Effect.gen(function* () {
      const root = mkdtempSync(join(tmpdir(), 'silk-auto-import-scale-'))
      const sourceRoot = join(root, 'src')
      mkdirSync(sourceRoot)
      writeFileSync(
        join(root, 'silk.toml'),
        '[package]\nname = "scale"\nversion = "0.1.0"\nroot = "src/Main.silk"\n',
      )
      const moduleCount = 200
      for (let ordinal = 0; ordinal < moduleCount; ordinal += 1)
        writeFileSync(
          join(sourceRoot, `Module${ordinal}.silk`),
          `pub fn symbol${ordinal}() -> i32 { return ${ordinal} }`,
        )
      const mainText = 'pub fn main() -> i32 { return symbol137() }'
      const mainPath = join(sourceRoot, 'Main.silk')
      writeFileSync(mainPath, mainText)
      const document = yield* Workspace.open({
        uri: pathToFileURL(mainPath).href,
        version: 1,
        bytes: encoder.encode(mainText),
      })
      const analyzed = yield* Workspace.analyzeProject([document])
      const session = analyzed.get(document.uri)
      assert.isDefined(session)
      if (session === undefined) return

      assert.deepEqual(
        Analysis.modules(session.snapshot).map((module) => module.name),
        ['Main'],
      )
      const queryStartedAt = performance.now()
      const actions = Analysis.autoImportsAt(
        session.snapshot,
        session.inventory,
        'Main',
        mainText.indexOf('symbol137') + 1,
      )
      const queryElapsedMs = performance.now() - queryStartedAt
      assert.deepEqual(
        actions.map((action) => action.candidate.module),
        ['Module137'],
      )

      const changedPath = join(sourceRoot, 'Module137.silk')
      writeFileSync(changedPath, 'pub fn revised137() -> i32 { return 137 }')
      const revised = yield* WorkspaceCatalog.refresh({
        sourceRoot,
        documents: [document],
        previous: session.inventory,
        invalidation: { dirtyPaths: [changedPath], rediscover: false },
      })
      for (let ordinal = 0; ordinal < moduleCount; ordinal += 1) {
        const module = `Module${ordinal}`
        if (ordinal === 137)
          assert.isFalse(revised.project.get(module) === session.inventory.project.get(module))
        else assert.isTrue(revised.project.get(module) === session.inventory.project.get(module))
      }
      assert.deepEqual(WorkspaceInventory.candidates(revised, 'symbol137'), [])
      assert.deepEqual(
        WorkspaceInventory.candidates(revised, 'revised137').map((candidate) => candidate.module),
        ['Module137'],
      )
      assert.strictEqual(revised.observation.scanned, 2)
      assert.strictEqual(revised.observation.reused, 1)
      assert.strictEqual(revised.observation.revised, 1)
      assert.strictEqual(revised.observation.removed, 0)
      assert.strictEqual(
        revised.observation.indexedModules,
        moduleCount + 1 + revised.toolchain.size,
      )
      assert.isAtLeast(revised.observation.indexedExports, moduleCount + 1)
      if (process.env.SILK_AUTO_IMPORT_MEASURE === '1')
        process.stderr.write(
          `${Inspectable.toStringUnknown({
            modules: moduleCount + 1,
            initial: session.inventory.observation,
            incremental: revised.observation,
            queryElapsedMs,
          })}\n`,
        )
    }).pipe(Effect.provide(NodeServices.layer)),
)
