import { NodeServices } from '@effect/platform-node'
import * as HeapObservation from '@silk-effect/compiler/HeapObservation'
import * as Layer from 'effect/Layer'

/** Replaceable host services for compiler CLI tests. */
export const layer = Layer.merge(NodeServices.layer, HeapObservation.layerTest)
