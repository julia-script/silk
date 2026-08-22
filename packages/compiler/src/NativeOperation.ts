import * as CleanupPlan from './CleanupPlan.js'
import type * as Mir from './Mir.js'

/** Whether one MIR operation requires the native allocation ABI. */
export const needsAllocation = (operation: Mir.Operation): boolean =>
  operation._tag === 'Allocate' ||
  operation._tag === 'RawBufferFrom' ||
  operation._tag === 'RawBufferCount' ||
  operation._tag === 'RawBufferSlot' ||
  operation._tag === 'RawBufferRead' ||
  operation._tag === 'RawBufferView' ||
  operation._tag === 'RawBufferCopy' ||
  operation._tag === 'RawBufferFill' ||
  operation._tag === 'SlotWrite' ||
  operation._tag === 'SlotTake' ||
  operation._tag === 'SlotCopy' ||
  operation._tag === 'SlotDrop' ||
  (operation._tag === 'CloseEffectEntry' &&
    operation.failures.some((failure) => CleanupPlan.reclaims(failure.cleanup))) ||
  (operation._tag === 'Drop' && CleanupPlan.reclaims(operation.cleanup))
