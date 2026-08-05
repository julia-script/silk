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

export const encodeSource = (source: string): string =>
  toBase64Url(new TextEncoder().encode(source))

export const decodeSource = (value: string): string | undefined => {
  try {
    return new TextDecoder().decode(fromBase64Url(value))
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
