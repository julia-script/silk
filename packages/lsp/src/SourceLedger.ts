import * as Data from 'effect/Data'
import * as Result from 'effect/Result'
import * as DocumentVersion from './DocumentVersion.js'
import type * as SourceEvent from './SourceEvent.js'

const TypeId: unique symbol = Symbol.for('@silk-lang/lsp/SourceLedger')

/** Latest accepted bytes for one open editor document. */
export interface Entry {
  readonly uri: string
  readonly version: DocumentVersion.DocumentVersion
  readonly bytes: Uint8Array
}

export interface SourceLedger {
  readonly [TypeId]: Map<string, Entry>
}

export type Acceptance =
  | { readonly _tag: 'Accepted'; readonly event: SourceEvent.SourceEvent; readonly entry?: Entry }
  | { readonly _tag: 'Ignored'; readonly event: SourceEvent.SourceEvent }

export class ProtocolViolation extends Data.TaggedError('ProtocolViolation')<{
  readonly uri: string
  readonly message: string
}> {}

export const make = (): SourceLedger => ({ [TypeId]: new Map() })

export const get = (self: SourceLedger, uri: string): Entry | undefined => self[TypeId].get(uri)

export const entries = (self: SourceLedger): ReadonlyArray<Entry> =>
  Object.freeze([...self[TypeId].values()])

/** Performs the authoritative non-yielding source transition before background work is signaled. */
export const accept = (
  self: SourceLedger,
  event: SourceEvent.SourceEvent,
): Result.Result<Acceptance, ProtocolViolation> => {
  switch (event._tag) {
    case 'Open':
    case 'Change': {
      const previous = self[TypeId].get(event.uri)
      if (previous !== undefined && !DocumentVersion.isAfter(event.version, previous.version))
        return Result.fail(
          new ProtocolViolation({
            uri: event.uri,
            message: `Document version ${event.version.value} is not newer than ${previous.version.value}`,
          }),
        )
      const entry = Object.freeze({
        uri: event.uri,
        version: event.version,
        bytes: Uint8Array.from(event.bytes),
      })
      self[TypeId].set(event.uri, entry)
      return Result.succeed(Object.freeze({ _tag: 'Accepted', event, entry }))
    }
    case 'Close': {
      const deleted = self[TypeId].delete(event.uri)
      return Result.succeed(
        deleted
          ? Object.freeze({ _tag: 'Accepted', event })
          : Object.freeze({ _tag: 'Ignored', event }),
      )
    }
    case 'Invalidate':
      return Result.succeed(Object.freeze({ _tag: 'Accepted', event }))
  }
}

export const clear = (self: SourceLedger): void => {
  self[TypeId].clear()
}
