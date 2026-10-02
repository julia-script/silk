import { assert, it } from '@effect/vitest'
import * as CompilerStdlib from '@silklang/compiler/Stdlib'
import * as Effect from 'effect/Effect'
import * as Example from '../src/Example.js'
import * as Json from '../src/Json.js'
import * as Model from '../src/Model.js'
import * as Site from '../src/Site.js'
import * as Stdlib from '../src/Stdlib.js'

// A two-entry slice of the real manifest owns manifest coverage and the emitter-to-renderer JSON
// boundary. `documentation:examples` (`silk doctest --stdlib`) documents and decodes the whole
// shipped manifest, so analyzing all of it again here would repeat that work. `silk/base64` is
// outside every module's implicit dependency closure, so it is documented only because it is a
// manifest root. Writer details and determinism use a synthetic document in Site.test.ts.
const manifest = CompilerStdlib.manifest.filter(
  (entry) => entry.module === 'silk/base64' || entry.module === 'silk/option',
)

it.effect('documents every manifest root and renders its encoded documentation', () =>
  Effect.gen(function* () {
    const roots = manifest.map((entry) => entry.module)
    assert.deepStrictEqual(roots, ['silk/base64', 'silk/option'])
    const documentation = yield* Stdlib.documentation('wasm32-unknown-unknown', manifest)
    const modules = documentation.modules.map((module) => module.name)
    assert.deepStrictEqual(
      modules.filter((module) => roots.includes(module)),
      roots,
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
)
