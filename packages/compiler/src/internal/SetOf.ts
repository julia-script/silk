/** One immutable set of primitives for the compiler's ownership layer. */

/** True when the two sets contain exactly the same elements. */
export const equal = <T>(left: ReadonlySet<T>, right: ReadonlySet<T>): boolean =>
  left.size === right.size && [...left].every((element) => right.has(element))

/** The union of two sets as a new frozen set. */
export const union = <T>(left: ReadonlySet<T>, right: ReadonlySet<T>): ReadonlySet<T> =>
  Object.freeze(new Set([...left, ...right]))

/** The intersection of two sets as a new frozen set. */
export const intersection = <T>(left: ReadonlySet<T>, right: ReadonlySet<T>): ReadonlySet<T> =>
  Object.freeze(new Set([...left].filter((element) => right.has(element))))
