import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as DocumentationWorkflow from '../src/DocumentationWorkflow.js'

const project = Effect.fnUntraced(function* (source: string) {
  const fileSystem = yield* FileSystem.FileSystem
  const root = yield* fileSystem.makeTempDirectoryScoped()
  yield* fileSystem.writeFileString(
    `${root}/silk.toml`,
    '[package]\nname = "documented"\nversion = "0.1.0"\nroot = "src/Main.silk"\nsource-root = "src"\n',
  )
  yield* fileSystem.makeDirectory(`${root}/src`, { recursive: true })
  yield* fileSystem.writeFileString(`${root}/src/Main.silk`, source)
  return Object.freeze({ root, manifestPath: `${root}/silk.toml` })
})

it.effect(
  'writes deterministic public documentation and includes private declarations on request',
  () =>
    Effect.gen(function* () {
      const fileSystem = yield* FileSystem.FileSystem
      const fixture = yield* project(`//! Module docs.
/// Public docs.
pub fn recover() -> i32 { return 1 }
/// Private docs.
fn helper() -> i32 { return 0 }
`)
      const publicPath = `${fixture.root}/public.json`
      const privatePath = `${fixture.root}/private.json`
      assert.strictEqual(
        yield* DocumentationWorkflow.run({
          manifestPath: fixture.manifestPath,
          output: publicPath,
        }),
        0,
      )
      const first = yield* fileSystem.readFileString(publicPath)
      assert.include(first, 'recover')
      assert.notInclude(first, 'helper')
      assert.isTrue(first.endsWith('\n'))

      assert.strictEqual(
        yield* DocumentationWorkflow.run({
          manifestPath: fixture.manifestPath,
          output: publicPath,
        }),
        0,
      )
      assert.strictEqual(yield* fileSystem.readFileString(publicPath), first)

      assert.strictEqual(
        yield* DocumentationWorkflow.run({
          manifestPath: fixture.manifestPath,
          output: privatePath,
          includePrivate: true,
        }),
        0,
      )
      assert.include(yield* fileSystem.readFileString(privatePath), 'helper')
    }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('refuses damaged source without creating a destination', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const fixture = yield* project('pub fn main() -> Missing { return 1 }')
    const destination = `${fixture.root}/documentation.json`
    assert.strictEqual(
      yield* DocumentationWorkflow.run({
        manifestPath: fixture.manifestPath,
        output: destination,
      }),
      1,
    )
    assert.strictEqual(yield* fileSystem.exists(destination), false)
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)
