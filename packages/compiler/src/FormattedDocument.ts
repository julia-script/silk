/** The immutable result of canonical source formatting. */
export interface FormattedDocument {
  readonly _tag: 'FormattedDocument'
  readonly bytes: ReadonlyArray<number>
  readonly changed: boolean
}

/** Copies formatted bytes into one immutable, reusable result. */
export const make = (bytes: Uint8Array, changed: boolean): FormattedDocument =>
  Object.freeze({
    _tag: 'FormattedDocument',
    bytes: Object.freeze(Array.from(bytes)),
    changed,
  })

/** Returns a defensive mutable copy for filesystem or protocol adapters. */
export const toUint8Array = (self: FormattedDocument): Uint8Array => Uint8Array.from(self.bytes)
