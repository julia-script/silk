import * as Type from './Type.js'

/** One concrete declaration specialization independent of the phase that discovered it. */
export interface Specialization {
  readonly declaration: {
    readonly module: string
    readonly name: string
  }
  readonly typeArguments: ReadonlyArray<Type.GenericArgument>
}

/** Returns the canonical identity shared by discovery, proof dependencies, and lowering. */
export const key = (self: Specialization): string =>
  `${self.declaration.module}\u0000${self.declaration.name}\u0000${self.typeArguments
    .map(Type.genericArgumentKey)
    .join('\u0000')}`
