import { createHash } from 'node:crypto'
import * as Effect from 'effect/Effect'
import * as Analysis from '../../dist/Analysis.js'
import * as Hir from '../../dist/Hir.js'
import * as Layout from '../../dist/Layout.js'
import * as Mir from '../../dist/Mir.js'
import * as Ownership from '../../dist/Ownership.js'
import * as Type from '../../dist/Type.js'

const source = `struct Box<T> { value: T }
fn identity<T>(value: T) -> T { return move value }
pub fn main() -> i32 {
  let flag = Box<bool> { value: identity(true) }
  let answer = Box<i32> { value: identity<i32>(42) }
  if flag.value { return answer.value }
  return 0
}`
const bytes = new TextEncoder().encode(source)
const snapshot = (target) =>
  Effect.runPromise(Analysis.ofSourceRealized('fixture/Generic', bytes, target))

const native = await snapshot('aarch64-apple-darwin')
const wasm = await snapshot('wasm32-unknown-unknown')
const nativeArtifact = await Effect.runPromise(Analysis.codegen(native, { mode: 'release' }))
const wasmArtifact = await Effect.runPromise(Analysis.codegenWasm(wasm, { mode: 'release' }))
const hash = (value) => createHash('sha256').update(value).digest('hex')
const layout = Analysis.layoutOf(native)

process.stdout.write(
  JSON.stringify({
    hir: Hir.encode(Analysis.hirOf(native, 'fixture/Generic')),
    ownership: Ownership.encode(Analysis.ownershipOf(native, 'fixture/Generic')),
    instances: Analysis.instancesOf(native).instances.map((instance) => ({
      declaration: instance.key.declaration,
      arguments: instance.key.typeArguments.map(Type.key),
      substitution: [...instance.substitution].map(([parameter, type]) => [
        parameter,
        Type.key(type),
      ]),
    })),
    layout: layout._tag === 'Available' ? Layout.encode(layout.value) : layout.error.message,
    mir: Mir.encode(Analysis.loweredMir(native)),
    trace: Analysis.traceOf(Analysis.evaluate(native)),
    nativeSymbols: nativeArtifact.symbols.map((entry) => entry.symbol),
    wasmSymbols: wasmArtifact.symbols.map((entry) => entry.symbol),
    native: hash(nativeArtifact.bitcode),
    wasm: hash(wasmArtifact.bytes),
  }),
)
