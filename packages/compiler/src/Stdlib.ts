/**
 * The Silk standard library: ordinary Silk source shipped with the compiler. Modules live in
 * the reserved `silk/` namespace, resolve through the module closure before any user resolver
 * is consulted, and compile through the same pipeline as user code with no privilege.
 */

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

/** The reserved namespace prefix. User resolvers are never consulted for these identities. */
export const namespacePrefix = 'silk/'

/** Tests whether a module identity claims the reserved standard-library namespace. */
export const isReserved = (module: string): boolean =>
  module === 'silk' || module.startsWith(namespacePrefix)

const vector = `// A growable owned sequence over the allocation substrate. Ordinary Silk: no compiler phase
// knows this type, and every capability it uses is available to user code.

pub struct Vector<T> {
  length: Usize
  capacity: Usize
}

pub fn length<T>(self: &Vector<T>) -> Usize {
  return self.length
}

pub fn capacity<T>(self: &Vector<T>) -> Usize {
  return self.capacity
}
`

/** Every standard-library module's source bytes by canonical identity. */
export const sources: ReadonlyMap<string, Uint8Array> = new Map([['silk/vector', ascii(vector)]])
