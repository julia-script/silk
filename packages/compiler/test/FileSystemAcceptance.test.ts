import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const encoder = new TextEncoder()

it.effect(
  'retains unprovided FileSystem and Allocator requirements instead of selecting ambient services',
  () =>
    Effect.gen(function* () {
      const source = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.filesystem { FileError, FileSystem, Path }
pub effect fn main() -> () ! FileError | OutOfMemoryError ? &mut FileSystem | &mut Allocator {
  let path = run Path.root()
  let info = run FileSystem.stat(&path)
  return ()
}`
      const snapshot = yield* Analysis.ofSourceRealized(
        'file-system-acceptance/unprovided',
        encoder.encode(source),
      )
      assert.deepEqual(
        Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
        ['SEM0204'],
      )
      const mir = Analysis.loweredMir(snapshot)
      assert.strictEqual(mir.entry._tag, 'UnavailableEntry')
    }),
)
