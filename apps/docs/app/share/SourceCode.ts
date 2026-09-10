const chunkSize = 0x8000

/** Encodes UTF-8 Silk source as unpadded, URL-safe Base64. */
export const encode = (source: string): string => {
  const bytes = new TextEncoder().encode(source)
  let binary = ''
  for (let offset = 0; offset < bytes.length; offset += chunkSize) {
    binary += String.fromCharCode(...bytes.slice(offset, offset + chunkSize))
  }
  return btoa(binary).replaceAll('+', '-').replaceAll('/', '_').replace(/=+$/, '')
}

/** Decodes URL-safe Base64, rejecting malformed input and source above the supplied limit. */
export const decode = (encoded: string, maximumLength: number): string | undefined => {
  try {
    const base64 = encoded.replaceAll('-', '+').replaceAll('_', '/')
    const binary = atob(base64.padEnd(Math.ceil(base64.length / 4) * 4, '='))
    const bytes = Uint8Array.from(binary, (character) => character.charCodeAt(0))
    const source = new TextDecoder('utf-8', { fatal: true }).decode(bytes)
    return source.length <= maximumLength ? source : undefined
  } catch {
    return undefined
  }
}
