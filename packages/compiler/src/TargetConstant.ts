import * as Scalar from './Scalar.js'
import type * as Target from './Target.js'

/**
 * The closed vocabulary of pointer-width facts a constant initializer may name.
 *
 * A bound on `usize` or `isize` has no single literal spelling: `wasm32-unknown-unknown` is four
 * bytes wide and the three native triples are eight, so any one number is wrong for one of them.
 * Nothing here is evaluated — each name selects between literals the compiler already knows,
 * from the same `Scalar` table the checked intrinsics enforce.
 */
export type Selector = 'usizeMax' | 'isizeMax' | 'isizeMin' | 'pointerBits'

/** The identifier that roots a target-dependent constant initializer. */
export const root = 'Target' as const

const pointerWidthInteger = (spelling: 'usize' | 'isize'): Scalar.IntegerScalar => {
  const scalar = Scalar.find(spelling)
  // A wrong bound must never be quietly substituted for a missing one; both spellings belong to the
  // closed scalar catalog, so an absence here is a compiler defect rather than a source condition.
  if (scalar?.category !== 'Integer')
    throw new RangeError(`Target facts require the ${spelling} scalar`)
  return scalar
}

const usize = pointerWidthInteger('usize')
const isize = pointerWidthInteger('isize')

/** Every selector in deterministic spelling order, for diagnostics and documentation. */
export const all: ReadonlyArray<Selector> = Object.freeze([
  'isizeMax',
  'isizeMin',
  'pointerBits',
  'usizeMax',
])

/** Recognizes one member spelling below `Target`, or nothing when it names no known fact. */
export const find = (spelling: string): Selector | undefined =>
  all.find((candidate) => candidate === spelling)

/** The one primitive type a selector may be declared at. */
export const declaredType = (self: Selector): 'usize' | 'isize' | 'u32' =>
  self === 'pointerBits' ? 'u32' : self === 'usizeMax' ? 'usize' : 'isize'

/** The exact value the selector names for one target pointer width. */
export const value = (self: Selector, pointerBits: 32 | 64): bigint => {
  if (self === 'pointerBits') return BigInt(pointerBits)
  if (self === 'usizeMax') return Scalar.range(usize, pointerBits).maximum
  const range = Scalar.range(isize, pointerBits)
  return self === 'isizeMax' ? range.maximum : range.minimum
}

/** The pointer width one selected target words its addresses at. */
export const pointerBits = (self: Target.Target): 32 | 64 => (self.pointerSize === 4 ? 32 : 64)

/**
 * The value elaboration records before a target is selected. Elaboration is target-independent, so
 * it ranges every integer at the widest pointer width the compiler supports; `Lower` re-selects the
 * value against the target actually chosen, which is what every engine then observes.
 */
export const unselected = (self: Selector): bigint => value(self, 64)
