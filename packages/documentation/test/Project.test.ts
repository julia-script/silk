import { assert, it } from '@effect/vitest'
import * as Analysis from '@silk-effect/compiler/Analysis'
import * as ProjectAnalysis from '@silk-effect/compiler/ProjectAnalysis'
import * as SourceFile from '@silk-effect/compiler/SourceFile'
import * as SourceResolver from '@silk-effect/compiler/SourceResolver'
import * as Effect from 'effect/Effect'
import * as Json from '../src/Json.js'
import * as Project from '../src/Project.js'

const encode = (value: string): Uint8Array => new TextEncoder().encode(value)

it.effect('builds public-first project JSON with first-class child documentation', () =>
  Effect.gen(function* () {
    const source = `//! Recovery utilities.

/// Default recovery code.
pub const defaultCode: i32 = 7

/// Primary dependency position.
pub role Primary

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
      ['defaultCode', 'Primary', 'recover', 'Problem'],
    )
    const defaultCode = module.items.find((item) => item.name === 'defaultCode')
    assert.strictEqual(defaultCode?.kind, 'Constant')
    assert.strictEqual(defaultCode?.signature.text, 'pub const defaultCode: i32')
    assert.strictEqual(defaultCode?.documentation?.markdown, 'Default recovery code.')
    const primary = module.items.find((item) => item.name === 'Primary')
    assert.strictEqual(primary?.kind, 'Role')
    assert.strictEqual(primary?.signature.text, 'pub role Primary')
    assert.strictEqual(primary?.documentation?.markdown, 'Primary dependency position.')
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

it.effect('builds one documentation model from a shared multi-root analysis', () =>
  Effect.gen(function* () {
    const roots = [
      SourceFile.make(
        'app/A',
        encode(`import shared.Core

/// Public A.
pub fn a() -> i32 { return Core.answer() }

/// Private A helper.
fn helper() -> i32 { return 0 }
`),
      ),
      SourceFile.make(
        'app/B',
        encode(`import shared.Core

/// Public B.
pub fn b() -> i32 { return Core.answer() }
`),
      ),
    ]
    const analysis = yield* ProjectAnalysis.make(roots).pipe(
      Effect.provide(
        SourceResolver.memory(
          new Map([
            [
              'shared/Core',
              encode(`/// Shared answer.
pub fn answer() -> i32 { return 42 }
`),
            ],
          ]),
        ),
      ),
    )

    const publicProject = Project.fromProjectAnalysis(analysis)
    assert.deepStrictEqual(
      publicProject.modules.map((module) => module.name),
      ['app/A', 'app/B', 'shared/Core'],
    )
    assert.strictEqual(new Set(publicProject.modules.map((module) => module.name)).size, 3)
    assert.isFalse(
      publicProject.modules
        .find((module) => module.name === 'app/A')
        ?.items.some((item) => item.name === 'helper') ?? true,
    )

    const privateProject = Project.fromProjectAnalysis(analysis, { includePrivate: true })
    assert.isTrue(
      privateProject.modules
        .find((module) => module.name === 'app/A')
        ?.items.some((item) => item.name === 'helper') ?? false,
    )
    const closure = ProjectAnalysis.phases(analysis).find((phase) => phase.phase === 'closure')
    assert.strictEqual(closure?.inputs, 2)
    assert.strictEqual(closure?.outputs, 3)
    assert.strictEqual(closure?.diagnostics, 0)
  }),
)
