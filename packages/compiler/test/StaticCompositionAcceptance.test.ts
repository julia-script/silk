import { NodeServices } from '@effect/platform-node'
import { assert, layer } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Path from 'effect/Path'
import * as Analysis from '../src/Analysis.js'

const decoder = new TextDecoder()
const encoder = new TextEncoder()

const fixture = Effect.fnUntraced(function* (name: string) {
  const fileSystem = yield* FileSystem.FileSystem
  const path = yield* Path.Path
  const file = yield* path.fromFileUrl(
    new URL(`./fixtures/static-composition/${name}.silk`, import.meta.url),
  )
  return decoder.decode(yield* fileSystem.readFile(file))
})

layer(NodeServices.layer)('static composition acceptance', (it) => {
  it.effect('reports the first representation divergence and exposes complete tooling facts', () =>
    Effect.gen(function* () {
      const source = yield* fixture('static-composition-acceptance')
      const divergent = `${source}
struct FirstBranch {}
struct SecondBranch {}
fn divergent(input: FirstBranch | SecondBranch) -> i32 {
  let represented = match move input {
    FirstBranch {} => MappedSchema<NumberSchema> {
      source: NumberSchema { value: 1 },
      transform: add(1)
    }
    SecondBranch {} => MappedSchema<NumberSchema> {
      source: NumberSchema { value: 2 },
      transform: add(2)
    }
  }
  return 0
}
`
      const divergentSnapshot = yield* Analysis.ofSource(
        'static-composition/divergent',
        encoder.encode(divergent),
      )
      const diagnostic = Analysis.diagnostics(divergentSnapshot).find(
        (candidate) => candidate.code === 'SEM0105',
      )
      assert.strictEqual(diagnostic?.reason._tag, 'DivergentRepresentationJoin')
      assert.deepEqual(
        diagnostic?.relatedSpans?.map((related) => related.label),
        ['first representation originates here', 'divergent representation originates here'],
      )

      const module = 'static-composition/tooling'
      const snapshot = yield* Analysis.ofSource(module, encoder.encode(source))
      const hover = Analysis.hoverSubjectAt(snapshot, module, source.indexOf('leftSchema ='))
      assert.include(hover?.presentation.text ?? '', 'MappedSchema<NumberSchema, typeof(')
      assert.include(hover?.presentation.text ?? '', '.add@declaration:')
      const operationOffset =
        source.indexOf('return CompleteDecoder.decode') + 'return CompleteDecoder.'.length
      const navigation = Analysis.semanticOccurrenceAt(snapshot, module, operationOffset)
      assert.strictEqual(navigation?.role, 'Operation')
      assert.strictEqual(navigation?.resolution._tag, 'Available')
      assert.strictEqual(navigation?.declaration?.module, module)
    }),
  )
})
