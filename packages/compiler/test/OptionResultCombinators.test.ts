import { readFileSync } from 'node:fs'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'

const stdlibSource = (module: string): string =>
  readFileSync(fileURLToPath(new URL(`../stdlib/silk/${module}.silk`, import.meta.url)), 'utf8')

it('keeps both modules free of unsafe blocks and intrinsic calls', () => {
  for (const module of ['option', 'result']) {
    const source = stdlibSource(module)
    assert.notMatch(source, /\bunsafe\b/, `${module}.silk declares an unsafe block`)
    assert.notMatch(source, /\bIntrinsic\./, `${module}.silk calls an intrinsic`)
  }
})
