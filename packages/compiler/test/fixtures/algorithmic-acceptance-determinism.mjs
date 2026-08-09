import { createHash } from 'node:crypto'
import { readFileSync } from 'node:fs'
import * as Effect from 'effect/Effect'
import * as Analysis from '../../dist/Analysis.js'
import * as Hir from '../../dist/Hir.js'
import * as Layout from '../../dist/Layout.js'
import * as Mir from '../../dist/Mir.js'
import * as Ownership from '../../dist/Ownership.js'
import * as SourceFile from '../../dist/SourceFile.js'
import * as SourceResolver from '../../dist/SourceResolver.js'
import * as SyntaxFile from '../../dist/SyntaxFile.js'
import * as Type from '../../dist/Type.js'

const rootModule = 'app/Main'
const moduleNames = ['app/Main', 'compiler/Coverage', 'compiler/Member']
const modules = new Map(
  moduleNames.map((name) => [
    name,
    new Uint8Array(readFileSync(new URL(`./algorithmic-acceptance/${name}.silk`, import.meta.url))),
  ]),
)
const rootBytes = modules.get(rootModule)
if (rootBytes === undefined) throw new RangeError(`Fixture has no root module ${rootModule}`)
const imports = new Map([...modules].filter(([name]) => name !== rootModule))

const snapshot = (target) =>
  Effect.runPromise(
    Analysis.makeRealized({ root: SourceFile.make(rootModule, rootBytes), target }).pipe(
      Effect.provide(SourceResolver.memory(imports)),
    ),
  )

const native = await snapshot('aarch64-apple-darwin')
const wasm = await snapshot('wasm32-unknown-unknown')
const nativeArtifact = await Effect.runPromise(Analysis.codegen(native, { mode: 'release' }))
const wasmArtifact = await Effect.runPromise(Analysis.codegenWasm(wasm, { mode: 'release' }))
const hash = (value) => createHash('sha256').update(value).digest('hex')

const encodeSnapshot = (self) => ({
  closure: Analysis.modules(self).map((module) => module.name),
  syntax: Analysis.modules(self).map((module) =>
    SyntaxFile.encode(Analysis.syntaxOf(self, module.name)),
  ),
  semantic: Analysis.modules(self).flatMap((module) =>
    Analysis.matchesOf(self, module.name).map((match) => ({
      access: match.access,
      members: match.members.map(Type.encode),
      arms: match.arms.map((arm) => ({
        before: arm.before.map(Type.encode),
        after: arm.after.map(Type.encode),
        guarded: arm.guard !== undefined,
        reachable: arm.reachable,
      })),
    })),
  ),
  hir: Analysis.modules(self).map((module) => Hir.encode(Analysis.hirOf(self, module.name))),
  ownership: Analysis.modules(self).map((module) => {
    const value = Analysis.ownershipOf(self, module.name)
    return value === undefined ? '' : Ownership.encode(value)
  }),
  instances: Analysis.instancesOf(self).instances.map((instance) => instance.key.declaration),
  layout:
    Analysis.layoutOf(self)._tag === 'Available'
      ? Layout.encode(Analysis.layoutOf(self).value)
      : Analysis.layoutOf(self).error.message,
  mir: Mir.encode(Analysis.loweredMir(self)),
  trace: Analysis.traceOf(Analysis.evaluate(self)),
})

process.stdout.write(
  JSON.stringify({
    ...encodeSnapshot(native),
    wasmLayout:
      Analysis.layoutOf(wasm)._tag === 'Available'
        ? Layout.encode(Analysis.layoutOf(wasm).value)
        : Analysis.layoutOf(wasm).error.message,
    wasmMir: Mir.encode(Analysis.loweredMir(wasm)),
    nativeSymbols: nativeArtifact.symbols,
    wasmSymbols: wasmArtifact.symbols,
    nativeText: hash(nativeArtifact.ir),
    wasmText: hash(wasmArtifact.wat),
    nativeBytes: hash(nativeArtifact.bitcode),
    wasmBytes: hash(wasmArtifact.bytes),
  }),
)
