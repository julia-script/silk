import type * as DocumentVersion from './DocumentVersion.js'

/** One editor or filesystem fact accepted by the authoritative source ledger. */
export type SourceEvent = Open | Change | Close | Invalidate

export interface Open {
  readonly _tag: 'Open'
  readonly uri: string
  readonly version: DocumentVersion.DocumentVersion
  readonly bytes: Uint8Array
}

export interface Change {
  readonly _tag: 'Change'
  readonly uri: string
  readonly version: DocumentVersion.DocumentVersion
  readonly bytes: Uint8Array
}

export interface Close {
  readonly _tag: 'Close'
  readonly uri: string
}

export interface Invalidate {
  readonly _tag: 'Invalidate'
  readonly paths: ReadonlyArray<string>
  readonly rediscover: boolean
}

export const open = (
  uri: string,
  version: DocumentVersion.DocumentVersion,
  bytes: Uint8Array,
): Open => Object.freeze({ _tag: 'Open', uri, version, bytes })

export const change = (
  uri: string,
  version: DocumentVersion.DocumentVersion,
  bytes: Uint8Array,
): Change => Object.freeze({ _tag: 'Change', uri, version, bytes })

export const close = (uri: string): Close => Object.freeze({ _tag: 'Close', uri })

export const invalidate = (paths: ReadonlyArray<string>, rediscover = false): Invalidate =>
  Object.freeze({ _tag: 'Invalidate', paths: Object.freeze([...paths]), rediscover })
