import { modules } from './Stdlib.generated.js'

/** The reserved namespace prefix. User resolvers are never consulted for these identities. */
export const namespacePrefix = 'silk/'

/** Tests whether a module identity claims the reserved standard-library namespace. */
export const isReserved = (module: string): boolean =>
  module === 'silk' || module.startsWith(namespacePrefix)

/** One generated view of a canonical, compiler-shipped Silk source file. */
export interface Module {
  readonly module: string
  readonly path: string
  readonly sourceUrl: URL
  readonly bytes: Uint8Array
}

const encoder = new TextEncoder()

/** The deterministic standard-library manifest, ordered by canonical module identity. */
export const manifest: ReadonlyArray<Module> = Object.freeze(
  modules.map((entry) =>
    Object.freeze({
      module: entry.module,
      path: entry.path,
      sourceUrl: new URL(`../stdlib/${entry.path}`, import.meta.url),
      bytes: encoder.encode(entry.source),
    }),
  ),
)

const byModule = new Map(manifest.map((entry) => [entry.module, entry] as const))

/** Returns the generated manifest entry for one standard-library module identity. */
export const find = (module: string): Module | undefined => byModule.get(module)

/** Every standard-library module's exact source bytes by canonical identity. */
export const sources: ReadonlyMap<string, Uint8Array> = new Map(
  manifest.map((entry) => [entry.module, Uint8Array.from(entry.bytes)] as const),
)
