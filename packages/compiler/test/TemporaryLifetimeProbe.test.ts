import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as MirVerification from '../src/MirVerification.js'
import * as Mir from '../src/Mir.js'
import * as MovePath from '../src/MovePath.js'
it.effect('inspects loop result cleanup', () => Effect.gen(function* () {
  const snapshot = yield* Analysis.ofSourceRealized('probe/loop-cleanup', new TextEncoder().encode(`import silk.string { String, InvalidUtf8 }
import silk.result { Result }
pub fn main() -> i32 {
  let data = b"a"
  let mut i = 0
  while i < 1 {
    let text = match move String.fromUtf8(data) {
      Result<string, InvalidUtf8>.Success { value } => value
      Result<string, InvalidUtf8>.Failure { error } => ""
    }
    if text == "a" { i = i + 1 } else { return 1 }
  }
  return 0
}`),'wasm32-unknown-unknown')
  assert.deepEqual(Analysis.diagnostics(snapshot),[])
  const program=Analysis.loweredMir(snapshot)
  const details=program.functions.flatMap(fn=>{
    const check=MirVerification.initializationOf(fn,program.layout)
    if(check.violations.length===0)return []
    return [{fn:fn.id,violations:check.violations,drops:MirVerification.operations(fn).flatMap(op=>op._tag==='Drop'?[{local:op.local,type:fn.localTypes.at(op.local.ordinal),span:op.provenance.span,initialization:op.initialization,before:[...(check.before.get(op)??new Map())].map(([id,state])=>[id,MovePath.encodeState(state)])}]:[])}]
  })
  assert.deepEqual(details,[])
}))
