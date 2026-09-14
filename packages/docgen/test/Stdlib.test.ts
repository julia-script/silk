import { assert, it } from '@effect/vitest'
import * as CompilerStdlib from '@silklang/compiler/Stdlib'
import * as Effect from 'effect/Effect'
import * as Example from '../src/Example.js'
import * as Json from '../src/Json.js'
import * as Model from '../src/Model.js'
import * as Site from '../src/Site.js'
import * as Stdlib from '../src/Stdlib.js'

// One real toolchain analysis owns manifest coverage and the emitter-to-renderer JSON boundary.
// Writer details and determinism use a small synthetic document in Site.test.ts.
it.effect(
  'documents the shipped manifest and renders its encoded documentation',
  () =>
    Effect.gen(function* () {
      const documentation = yield* Stdlib.documentation('aarch64-apple-darwin')
      const modules = CompilerStdlib.manifest.map((entry) => entry.module)
      assert.deepStrictEqual(
        documentation.modules.map((module) => module.name),
        modules,
      )
      const parsed = Json.decodeSync(Json.encode(documentation))
      const examples = Example.collect(parsed)
      assert.isAbove(examples.length, 0)
      assert.deepStrictEqual(examples, Example.collect(documentation))
      const decoded = Model.decode(parsed)
      assert.strictEqual(decoded._tag, 'Decoded')
      if (decoded._tag !== 'Decoded') return
      assert.deepStrictEqual(
        decoded.documentation.modules.map((module) => module.name),
        modules,
      )
      const site = Site.render(decoded.documentation, { title: 'Silk standard library' })
      const pages = site.files.filter((file) => file.path.endsWith('.html'))
      assert.lengthOf(pages, modules.length + 1)
      const index = pages.find((file) => file.path === 'index.html')
      assert.isDefined(index)
      for (const module of modules) assert.include(index.contents, module)
      const option = pages.find((file) => file.path === 'silk-option.html')
      assert.isDefined(option)
      assert.include(option.contents, 'unwrapOr')
    }),
  420_000,
)
