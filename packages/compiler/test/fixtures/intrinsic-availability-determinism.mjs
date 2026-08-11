import * as Effect from 'effect/Effect'
import * as Analysis from '../../dist/Analysis.js'
import * as Intrinsic from '../../dist/Intrinsic.js'
import * as IntrinsicAvailability from '../../dist/IntrinsicAvailability.js'

const source = `fn nativeWrapper() -> i32 { return Intrinsic.i32Add(20, 22) }
pub fn main() -> i32 { return nativeWrapper() }`
const self = await Effect.runPromise(
  Analysis.ofSourceRealized(
    'availability/determinism',
    new TextEncoder().encode(source),
    'wasm32-unknown-unknown',
  ),
)
const operation = Intrinsic.findOperation('Intrinsic', 'i32Add')
if (operation === undefined) throw new Error('expected Intrinsic.i32Add')
const catalog = Intrinsic.all()
  .flatMap((actor) => actor.operations)
  .map((candidate) =>
    Intrinsic.operationText(candidate.id) === Intrinsic.operationText(operation.id)
      ? Object.freeze({ ...candidate, targets: Object.freeze(['LLVM']) })
      : candidate,
  )
const selection = IntrinsicAvailability.select(self.instances.intrinsics, 'Wasm', catalog)
const wasm = await Effect.runPromise(Analysis.codegenWasm(self, { mode: 'release' }))
process.stdout.write(
  JSON.stringify({
    closure: IntrinsicAvailability.encode(
      IntrinsicAvailability.select(self.instances.intrinsics, 'Wasm').inventory,
    ),
    diagnostic: selection._tag === 'Unavailable' ? selection.diagnostics : [],
    hostImports: wasm.hostImports,
  }),
)
