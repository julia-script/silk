import { assert, it } from '@effect/vitest'
import * as Analysis from '@silk-effect/compiler/Analysis'
import * as Effect from 'effect/Effect'
import * as Json from '../src/Json.js'
import * as Project from '../src/Project.js'

const encode = (value: string): Uint8Array => new TextEncoder().encode(value)

it.effect('builds public-first project JSON with first-class child documentation', () =>
  Effect.gen(function* () {
    const source = `//! Recovery utilities.

/// Default recovery code.
pub const defaultCode: i32 = 7

/// Public recovery.
pub fn recover(
  /// The failure to inspect.
  problem: Problem
) -> i32 { return problem.code }

/// Not part of public output.
fn helper() -> i32 { return 0 }

/// A recoverable problem.
pub struct Problem {
  /// Stable numeric code.
  pub code: i32
  privateCode: i32
}
`
    const snapshot = yield* Analysis.ofSource('project/main', encode(source))
    const publicProject = Project.make(snapshot)
    const module = publicProject.modules.at(0)
    assert.isDefined(module)
    assert.strictEqual(module.documentation?.markdown, 'Recovery utilities.')
    assert.deepStrictEqual(
      module.items.filter((item) => item.kind !== 'Implementation').map((item) => item.name),
      ['defaultCode', 'recover', 'Problem'],
    )
    const defaultCode = module.items.find((item) => item.name === 'defaultCode')
    assert.strictEqual(defaultCode?.kind, 'Constant')
    assert.strictEqual(defaultCode?.signature.text, 'pub const defaultCode: i32')
    assert.strictEqual(defaultCode?.documentation?.markdown, 'Default recovery code.')
    const recover = module.items.find((item) => item.name === 'recover')
    assert.strictEqual(recover?.children.at(0)?.documentation?.markdown, 'The failure to inspect.')
    const problem = module.items.find((item) => item.name === 'Problem')
    assert.deepStrictEqual(
      problem?.children.map((item) => item.name),
      ['code'],
    )

    const privateProject = Project.make(snapshot, { includePrivate: true })
    assert.isTrue(privateProject.modules[0]?.items.some((item) => item.name === 'helper'))
    assert.isTrue(
      privateProject.modules[0]?.items
        .find((item) => item.name === 'Problem')
        ?.children.some((item) => item.name === 'privateCode'),
    )

    const json = Json.encode(publicProject)
    assert.strictEqual(json, Json.encode(publicProject))
    assert.match(json, /"schema": "silk-documentation"/)
    assert.isTrue(json.endsWith('\n'))
  }),
)
