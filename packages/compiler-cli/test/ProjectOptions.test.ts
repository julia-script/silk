import { assert, it } from '@effect/vitest'
import * as Result from 'effect/Result'
import * as ProjectOptions from '../src/ProjectOptions.js'

it('defaults project commands to the debug profile', () => {
  assert.deepStrictEqual(
    ProjectOptions.resolve({ release: false }),
    Result.succeed({ profile: 'debug' }),
  )
})

it('uses release as a profile shorthand', () => {
  assert.deepStrictEqual(
    ProjectOptions.resolve({ release: true }),
    Result.succeed({ profile: 'release' }),
  )
  assert.deepStrictEqual(
    ProjectOptions.resolve({ release: true, profile: 'release' }),
    Result.succeed({ profile: 'release' }),
  )
})

it('preserves explicit project selection flags', () => {
  assert.deepStrictEqual(
    ProjectOptions.resolve({
      release: false,
      manifestPath: 'nested/silk.toml',
      target: 'aarch64-apple-darwin',
      profile: 'release-with-debug',
    }),
    Result.succeed({
      manifestPath: 'nested/silk.toml',
      target: 'aarch64-apple-darwin',
      profile: 'release-with-debug',
    }),
  )
})

it('rejects a release shorthand that contradicts an explicit profile', () => {
  const resolved = ProjectOptions.resolve({ release: true, profile: 'debug' })
  assert.strictEqual(Result.isFailure(resolved), true)
  if (Result.isFailure(resolved)) {
    assert.strictEqual(resolved.failure.reason._tag, 'ConflictingProfile')
  }
})
