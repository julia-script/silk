import type * as Backend from './Backend.js'
import type * as CAbi from './CAbi.js'
import type * as Target from './Target.js'

export interface FunctionEntry {
  readonly kind: 'function'
  readonly symbol: string
  readonly abi: 'C'
  readonly direction: 'export' | 'import'
  readonly parameters: ReadonlyArray<CAbi.TypeText>
  readonly result: CAbi.TypeText
}

export interface DataEntry {
  readonly kind: 'data'
  readonly symbol: string
  readonly abi: 'C'
  readonly direction: 'export' | 'import'
  readonly type: CAbi.TypeText
}

export type Entry = FunctionEntry | DataEntry

/** The stable V1 machine-readable ABI surface of one native library. */
export interface AbiManifest {
  readonly silkForeignAbi: 1
  readonly target: Target.Id
  readonly exports: ReadonlyArray<Entry>
  readonly imports: ReadonlyArray<Entry>
}

const compareText = (left: string, right: string): number => {
  if (left < right) return -1
  if (left > right) return 1
  return 0
}

const compareEntries = (left: Entry, right: Entry): number =>
  compareText(left.symbol, right.symbol) || compareText(left.kind, right.kind)

const functionEntry = (
  direction: 'export' | 'import',
  fn: Backend.ForeignExport | Backend.ForeignImport,
): FunctionEntry =>
  Object.freeze({
    kind: 'function',
    symbol: fn.symbol,
    abi: 'C',
    direction,
    parameters: Object.freeze([...fn.parameters]),
    result: fn.result,
  })

const dataEntry = (data: Backend.ForeignStatic): DataEntry =>
  Object.freeze({
    kind: 'data',
    symbol: data.symbol,
    abi: 'C',
    direction: data.direction === 'Export' ? 'export' : 'import',
    type: data.type,
  })

/** Creates a versioned manifest from one target-qualified verified ABI inventory. */
export const make = (
  target: Target.Target,
  functionImports: ReadonlyArray<Backend.ForeignImport>,
  functionExports: ReadonlyArray<Backend.ForeignExport>,
  data: ReadonlyArray<Backend.ForeignStatic>,
): AbiManifest => {
  const exports: Array<Entry> = [
    ...functionExports.map((fn) => functionEntry('export', fn)),
    ...data.filter((entry) => entry.direction === 'Export').map(dataEntry),
  ]
  const imports: Array<Entry> = [
    ...functionImports.map((fn) => functionEntry('import', fn)),
    ...data.filter((entry) => entry.direction === 'Import').map(dataEntry),
  ]
  exports.sort(compareEntries)
  imports.sort(compareEntries)
  return Object.freeze({
    silkForeignAbi: 1,
    target: target.id,
    exports: Object.freeze(exports),
    imports: Object.freeze(imports),
  })
}

/** Renders stable two-space JSON with one trailing newline. */
export const render = (self: AbiManifest): string => `${JSON.stringify(self, null, 2)}\n`

/** Encodes one canonical manifest as UTF-8 bytes. */
export const encode = (self: AbiManifest): Uint8Array => new TextEncoder().encode(render(self))
