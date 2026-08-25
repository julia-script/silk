import { readdirSync, readFileSync } from 'node:fs'
import { join } from 'node:path'
import { fileURLToPath } from 'node:url'
import { createContext, runInContext } from 'node:vm'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Model from '../src/Model.js'
import * as Search from '../src/Search.js'
import * as Site from '../src/Site.js'
import { encoded, manifestModules } from './support/stdlibDocumentation.js'

const packageRoot = fileURLToPath(new URL('..', import.meta.url))

const sourceFiles = (directory: string): ReadonlyArray<string> =>
  readdirSync(directory, { withFileTypes: true }).flatMap((entry) => {
    const path = join(directory, entry.name)
    if (entry.isDirectory()) return sourceFiles(path)
    if (entry.name.endsWith('.ts')) return [path]
    return []
  })

/**
 * The requirement that the renderer reads no compiler internal type, checked structurally.
 *
 * An import scan alone catches the obvious violation and misses a re-export. The stronger form is
 * that there is no workspace package installed under this one to import at runtime at all, which a
 * future import cannot pass without a deliberate, visible edit to `package.json`. Development
 * dependencies are exempt on purpose: the tests generate real documentation JSON to render, and
 * that generation never ships.
 */
it('declares no workspace runtime dependency', () => {
  const manifest: unknown = JSON.parse(readFileSync(join(packageRoot, 'package.json'), 'utf8'))
  assert.isTrue(typeof manifest === 'object' && manifest !== null)
  const dependencies =
    typeof manifest === 'object' && manifest !== null && 'dependencies' in manifest
      ? Object.keys(manifest.dependencies as Record<string, string>)
      : []
  assert.isAbove(dependencies.length, 0, 'the renderer is expected to have runtime dependencies')
  assert.deepStrictEqual(
    dependencies.filter((name) => name.startsWith('@silk-effect/')),
    [],
    'the renderer must read the documentation JSON and nothing else',
  )
})

it('imports no compiler or workspace module from its own sources', () => {
  const offenders = sourceFiles(join(packageRoot, 'src'))
    .filter((file) => readFileSync(file, 'utf8').includes('@silk-effect/'))
    .map((file) => file.slice(packageRoot.length))
  assert.deepStrictEqual(offenders, [])
})

it.effect(
  'writes a page for every module of real standard-library documentation JSON',
  () =>
    Effect.gen(function* () {
      // Parsed from text, so the renderer reads what `silk doc` writes rather than what the
      // emitter happens to hold in memory.
      const parsed: unknown = JSON.parse(yield* encoded)
      const decoded = Model.decode(parsed)
      assert.strictEqual(decoded._tag, 'Decoded')
      if (decoded._tag !== 'Decoded') return

      assert.deepStrictEqual(
        decoded.documentation.modules.map((module) => module.name),
        manifestModules,
        'coverage follows the shipped manifest rather than a fixed module list',
      )

      const site = Site.render(decoded.documentation, { title: 'Silk standard library' })
      const paths = site.files.map((file) => file.path)
      assert.deepStrictEqual(
        paths.filter((path) => path.endsWith('.html')).length,
        manifestModules.length + 1,
        'one page per module, plus the index',
      )
      assert.include(paths, 'index.html')
      assert.include(paths, 'style.css')
      assert.include(paths, 'search-index.js')
      assert.deepStrictEqual(
        paths.filter((path, index) => paths.indexOf(path) !== index),
        [],
        'every rendered file must have a distinct path',
      )

      const index = site.files.find((file) => file.path === 'index.html')
      for (const module of manifestModules)
        assert.include(index?.contents ?? '', module, `${module} is missing from the index page`)

      const option = site.files.find((file) => file.path === 'silk-option.html')
      assert.isDefined(option)
      assert.include(option.contents, 'unwrapOr')
      assert.include(
        option.contents,
        'pub fn unwrapOr&lt;T&gt;(self: Option&lt;T&gt;, fallback: T) -&gt; T',
        'a signature must be escaped, not emitted as markup',
      )
      assert.notInclude(
        option.contents,
        '<T>',
        'no documentation value may reach the page as an element',
      )
    }),
  240_000,
)

it.effect(
  'renders the same bytes twice',
  () =>
    Effect.gen(function* () {
      const parsed: unknown = JSON.parse(yield* encoded)
      const decoded = Model.decode(parsed)
      if (decoded._tag !== 'Decoded') return assert.fail('expected documentation JSON')
      const first = Site.render(decoded.documentation)
      const second = Site.render(JSON.parse(JSON.stringify(decoded.documentation)))
      assert.deepStrictEqual(
        second.files.map((file) => [file.path, file.contents]),
        first.files.map((file) => [file.path, file.contents]),
      )
    }),
  240_000,
)

/**
 * The whole search path over real data: the emitted index file, evaluated in a JavaScript engine,
 * searched by the same matcher the pages embed, resolving to a page the site actually wrote.
 */
it.effect(
  'finds a standard-library declaration by name through the emitted index',
  () =>
    Effect.gen(function* () {
      const decoded = Model.decode(JSON.parse(yield* encoded))
      if (decoded._tag !== 'Decoded') return assert.fail('expected documentation JSON')
      const site = Site.render(decoded.documentation)
      const indexFile = site.files.find((file) => file.path === 'search-index.js')
      assert.isDefined(indexFile)

      const context = createContext({})
      runInContext(indexFile.contents, context)
      const loaded = runInContext(`globalThis.${Search.globalName}`, context)
      assert.isTrue(Array.isArray(loaded))
      if (!Array.isArray(loaded)) return
      assert.isAbove(loaded.length, 0)

      const found = Search.query(loaded, 'unwrapOr').at(0)
      assert.strictEqual(found?.name, 'unwrapOr')
      assert.strictEqual(found?.module, 'silk/option')

      const [page, anchor] = (found?.href ?? '').split('#')
      const target = site.files.find((file) => file.path === page)
      assert.isDefined(target, `the index points at ${String(page)}, which the site did not write`)
      assert.include(target.contents, `id="${String(anchor)}"`)
      assert.include(target.contents, '<script src="search-index.js"></script>')
    }),
  240_000,
)
