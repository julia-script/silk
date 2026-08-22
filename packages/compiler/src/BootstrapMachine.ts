import type { BlockedReason } from './BootstrapTrace.js'
import type { Value } from './BootstrapValue.js'
import type * as Mir from './Mir.js'
import type * as SourceSpan from './SourceSpan.js'

export interface LocalState {
  readonly value: Value
  readonly fromCall: boolean
}

export type Step =
  | { readonly _tag: 'Value'; readonly value: Value }
  | { readonly _tag: 'Blocked'; readonly reason: BlockedReason }
  | TransferStep

export interface TransferStep {
  readonly _tag: 'Transfer'
  readonly origin: Mir.SuspendEffectRegion
  readonly child: CallRequest
}

export interface CallRequest {
  readonly _tag: 'CallRequest'
  readonly target: Mir.MirFunction
  readonly arguments: ReadonlyArray<Value>
  readonly span: SourceSpan.SourceSpan
}

export interface OriginTransferRequest {
  readonly _tag: 'OriginTransferRequest'
  readonly origin: Mir.SuspendEffectRegion
  readonly child: CallRequest
}

export interface RelayTransferRequest {
  readonly _tag: 'RelayTransferRequest'
  readonly transfer: TransferStep
  readonly state?: Mir.CoroutineFrameState
}

export type MachineRequest = CallRequest | OriginTransferRequest | RelayTransferRequest
