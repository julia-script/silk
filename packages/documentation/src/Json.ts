import type * as Project from './Project.js'

/** Encodes the canonical experimental documentation artifact deterministically. */
export const encode = (self: Project.Project): string => `${JSON.stringify(self, undefined, 2)}\n`
