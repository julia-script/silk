/**
 * Workbench state lives in the URL, not just in localStorage.
 *
 * The point is addressability: a `/labs?s=…&l=…` link reproduces an exact view — this program,
 * these panes, this arrangement. That is what makes the workbench usable in a tight edit/refresh
 * loop and what makes a view shareable at all; a localStorage-only layout has to be rebuilt by
 * hand on every fresh browser, which is precisely the cost worth removing.
 *
 * The source is encoded alongside the layout on purpose. A layout-only link still forces whoever
 * opens it to retype the program, which leaves most of that cost in place.
 */

import type { SerializedDockview } from 'dockview'

export const sourceParam = 's'
export const layoutParam = 'l'

/**
 * Base64url keeps the payload safe in a query string without percent-encoding every other byte,
 * which matters because these URLs are meant to be pasted around by hand.
 */
const toBase64Url = (bytes: Uint8Array): string => {
  let binary = ''
  for (const byte of bytes) binary += String.fromCharCode(byte)
  return btoa(binary).replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '')
}

const fromBase64Url = (value: string): Uint8Array => {
  const padded = value.replace(/-/g, '+').replace(/_/g, '/')
  const binary = atob(padded.padEnd(Math.ceil(padded.length / 4) * 4, '='))
  return Uint8Array.from(binary, (character) => character.charCodeAt(0))
}

/**
 * Deflate shrinks a serialized dockview layout by roughly 5-10x. The layouts are verbose JSON
 * with deeply repeated keys, and an uncompressed one gets long enough to hit URL limits in some
 * tools once a few panes are open.
 */
const deflate = async (text: string): Promise<string> => {
  // `.slice()` re-backs the bytes with a plain `ArrayBuffer`; `Blob` will not accept an array
  // that is merely `ArrayBufferLike`, which is what `encode` is typed to return.
  const stream = new Blob([new TextEncoder().encode(text).slice()])
    .stream()
    .pipeThrough(new CompressionStream('deflate-raw'))
  const bytes = new Uint8Array(await new Response(stream).arrayBuffer())
  return toBase64Url(bytes)
}

const inflate = async (value: string): Promise<string> => {
  const stream = new Blob([fromBase64Url(value).slice()])
    .stream()
    .pipeThrough(new DecompressionStream('deflate-raw'))
  return new Response(stream).text()
}

/** The program a link carries: every module of the request, and which one it is rooted at. */
export interface SourceState {
  readonly root: string
  readonly modules: Readonly<Record<string, string>>
}

export const encodeSource = async (state: SourceState): Promise<string | undefined> => {
  try {
    return await deflate(JSON.stringify(state))
  } catch {
    return undefined
  }
}

export const decodeSource = async (value: string): Promise<SourceState | undefined> => {
  try {
    const parsed: unknown = await inflate(value).then(JSON.parse)
    if (typeof parsed !== 'object' || parsed === null) return undefined
    const { root, modules } = parsed as { root?: unknown; modules?: unknown }
    if (typeof root !== 'string') return undefined
    if (typeof modules !== 'object' || modules === null) return undefined
    const entries = Object.entries(modules).filter(
      (entry): entry is [string, string] => typeof entry[1] === 'string',
    )
    if (entries.length === 0) return undefined
    // A root naming a module that is not in the map would make the request unloadable.
    if (!entries.some(([name]) => name === root)) return undefined
    return { root, modules: Object.fromEntries(entries) }
  } catch {
    return undefined
  }
}

export const encodeLayout = async (layout: SerializedDockview): Promise<string | undefined> => {
  try {
    return await deflate(JSON.stringify(layout))
  } catch {
    return undefined
  }
}

export const decodeLayout = async (value: string): Promise<SerializedDockview | undefined> => {
  try {
    const parsed: unknown = JSON.parse(await inflate(value))
    // A layout from an older build can deserialize into something dockview rejects at
    // `fromJSON`, which would throw during render; the shape check keeps that to a fallback.
    if (typeof parsed !== 'object' || parsed === null || !('grid' in parsed)) return undefined
    return parsed as SerializedDockview
  } catch {
    return undefined
  }
}
