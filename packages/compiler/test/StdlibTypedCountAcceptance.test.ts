import { assert, it } from '@effect/vitest'
import * as Stdlib from '../src/Stdlib.js'

const decoder = new TextDecoder()

/** The five modules that used to copy a private `counted` identity to type their own literals. */
const previousHolders = [
  'silk/vector',
  'silk/bytes',
  'silk/string',
  'silk/filesystem',
  'silk/os_filesystem',
] as const

const sourceText = (module: string): string => {
  const bytes = Stdlib.sources.get(module)
  assert.isDefined(bytes, module)
  return decoder.decode(bytes)
}

/**
 * `silk/usize` owns the shared typed zero and one, so no module has to reintroduce an identity
 * call to keep a bare count off the `i32` default.
 */
it('declares the shared usize counts once and ships no private counted identity', () => {
  for (const entry of Stdlib.manifest)
    assert.notMatch(
      sourceText(entry.module),
      /\bfn\s+counted\b/,
      `${entry.module} still declares a counted identity`,
    )

  const usize = sourceText('silk/usize')
  assert.include(usize, 'pub const ZERO: usize = 0')
  assert.include(usize, 'pub const ONE: usize = 1')

  for (const module of previousHolders)
    assert.match(
      sourceText(module),
      /\busize\.(ZERO|ONE)\b/,
      `${module} names no shared typed count`,
    )
})
