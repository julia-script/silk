import type * as DeclarationFacts from './DeclarationFacts.js'
import type * as Instances from './Instances.js'
import type * as Type from './Type.js'

/** Compiler-private identity shared by every concrete local-shared payload cleanup helper. */
export const declaration: DeclarationFacts.CanonicalId = Object.freeze({
  _tag: 'CanonicalDeclarationId',
  module: '$silk.compiler.local-shared',
  name: 'payloadCleanup',
})

/** Selects the one generated helper specialization for a concrete payload type. */
export const instance = (element: Type.Type): Instances.InstanceKey =>
  Object.freeze({
    _tag: 'InstanceKey',
    declaration,
    typeArguments: Object.freeze([element]),
    contractRow: Object.freeze([]),
  })
