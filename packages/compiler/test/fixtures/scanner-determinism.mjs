import { createHash } from 'node:crypto'
import { readFileSync } from 'node:fs'
import * as Effect from 'effect/Effect'
import * as Analysis from '../../dist/Analysis.js'
import * as Hir from '../../dist/Hir.js'
import * as LayoutEncode from '../../dist/LayoutEncode.js'
import * as MirEncoding from '../../dist/MirEncoding.js'
import * as OwnershipEncoding from '../../dist/OwnershipEncoding.js'
import * as ToolchainIntegrity from '../../dist/ToolchainIntegrity.js'

const bytes = new Uint8Array(
  readFileSync(new URL('./scanner-acceptance/Main.silk', import.meta.url)),
)
const snapshot = (target) =>
  Effect.runPromise(Analysis.ofSourceRealized('scanner-acceptance/Main', bytes, target))
const hash = (value) => createHash('sha256').update(value).digest('hex')
const json = (value) =>
  JSON.stringify(value, (_key, candidate) =>
    typeof candidate === 'bigint' ? candidate.toString() : candidate,
  )

const native = await snapshot('aarch64-apple-darwin')
const wasm = await snapshot('wasm32-unknown-unknown')
const nativeArtifact = await Effect.runPromise(Analysis.codegen(native, { mode: 'release' }))

const encodeSnapshot = (self) => ({
  diagnostics: Analysis.diagnostics(self),
  modules: Analysis.modules(self).map((module) => module.name),
  hir: hash(
    Analysis.modules(self)
      .map((module) => Hir.encode(self.results.get(module.name)?.hir))
      .join('\n'),
  ),
  ownership: hash(
    Analysis.modules(self)
      .map((module) => {
        const value = Analysis.ownershipOf(self, module.name)
        return value === undefined ? '' : OwnershipEncoding.encode(value)
      })
      .join('\n'),
  ),
  instances: hash(json(Analysis.instancesOf(self).instances.map((instance) => instance.key))),
  layout: hash(LayoutEncode.encode(Analysis.layoutOf(self).value)),
  mir: hash(MirEncoding.encode(Analysis.loweredMir(self))),
})

process.stdout.write(
  json({
    toolchainIdentity: ToolchainIntegrity.installed().digest,
    native: encodeSnapshot(native),
    wasm: encodeSnapshot(wasm),
    nativeSymbols: nativeArtifact.symbols,
    nativeText: hash(nativeArtifact.ir),
    nativeBytes: hash(nativeArtifact.bitcode),
  }),
)
