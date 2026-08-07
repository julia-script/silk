/**
 * Projects `Instr` trees into the labs row grammar.
 *
 * The interpreter identifies "where execution is" and "where breakpoints are" by instruction
 * value identity, so these rows carry the actual `Instr` values: clicking a row toggles a
 * breakpoint on that exact value, and the row whose value the paused session reports is the
 * one that lights up. `else`/`end` rows are visual structure only and toggle nothing.
 */

import type * as Instr from '@silk-effect/wasm/Instr'
import type * as Interp from '@silk-effect/wasm/Interp'
import type { RowModel } from '../row/row'

/** Display names for opaque handles (functions, globals, memories) inside instructions. */
export type HandleNames = ReadonlyMap<object, string>

const handleName = (names: HandleNames, handle: object, fallback: string): string =>
  `$${names.get(handle) ?? fallback}`

export const instrText = (instr: Instr.Instr, names: HandleNames): string => {
  switch (instr._tag) {
    case 'Op':
      return instr.mnemonic
    case 'I32Const':
      return `i32.const ${instr.value}`
    case 'I64Const':
      return `i64.const ${instr.value}`
    case 'F32Const':
      return `f32.const ${instr.value}`
    case 'F64Const':
      return `f64.const ${instr.value}`
    case 'LocalGet':
      return `local.get ${instr.local}`
    case 'LocalSet':
      return `local.set ${instr.local}`
    case 'LocalTee':
      return `local.tee ${instr.local}`
    case 'GlobalGet':
      return `global.get ${handleName(names, instr.global, 'global')}`
    case 'GlobalSet':
      return `global.set ${handleName(names, instr.global, 'global')}`
    case 'Call':
      return `call ${handleName(names, instr.func, 'func')}`
    case 'CallIndirect':
      return 'call_indirect'
    case 'ReturnCall':
      return `return_call ${handleName(names, instr.func, 'func')}`
    case 'Block':
      return 'block'
    case 'Loop':
      return 'loop'
    case 'If':
      return 'if'
    case 'Br':
      return `br ${instr.depth}`
    case 'BrIf':
      return `br_if ${instr.depth}`
    case 'BrTable':
      return `br_table ${instr.depths.join(' ')} ${instr.defaultDepth}`
    case 'MemoryAccess':
      return `${instr.mnemonic} ${handleName(names, instr.memory, 'mem')}${
        Number(instr.offset) === 0 ? '' : ` offset=${instr.offset}`
      }`
    case 'MemorySize':
      return 'memory.size'
    case 'MemoryGrow':
      return 'memory.grow'
    case 'RefNull':
      return 'ref.null'
    case 'RefFunc':
      return `ref.func ${handleName(names, instr.func, 'func')}`
    default:
      return instr._tag.replace(/([a-z])([A-Z])/g, '$1_$2').toLowerCase()
  }
}

export interface ListingOptions {
  /** The instruction the paused session is about to execute. */
  readonly current: Instr.Instr | undefined
  readonly breakpoints: ReadonlySet<object>
  readonly onToggle: (instr: Instr.Instr) => void
}

const structural = (key: string, depth: number, label: string): RowModel => ({
  key,
  depth,
  label,
})

/** One function body as rows, nested blocks indented, `end` rows closing them. */
export const listingRows = (
  fn: { readonly name: string; readonly signature: string; readonly body: ReadonlyArray<Instr.Instr> },
  names: HandleNames,
  options: ListingOptions,
): ReadonlyArray<RowModel> => {
  const rows: Array<RowModel> = [
    {
      key: `${fn.name}-head`,
      label: `func $${fn.name}`,
      detail: fn.signature,
      head: true,
      dot: 'node',
    },
  ]
  let line = 0
  const walk = (body: ReadonlyArray<Instr.Instr>, depth: number): void => {
    for (const instr of body) {
      line += 1
      const key = `${fn.name}-${line}`
      const isCurrent = instr === options.current
      rows.push({
        key,
        lead: line,
        depth,
        caret: isCurrent ? '▸' : undefined,
        dot: options.breakpoints.has(instr) ? 'error' : undefined,
        label: instrText(instr, names),
        tone: isCurrent ? 'symbol' : undefined,
        onActivate: () => options.onToggle(instr),
      })
      switch (instr._tag) {
        case 'Block':
        case 'Loop':
        case 'TryTable':
          walk(instr.body, depth + 1)
          rows.push(structural(`${key}-end`, depth, 'end'))
          break
        case 'If':
          walk(instr.thenBody, depth + 1)
          if (instr.elseBody.length > 0) {
            rows.push(structural(`${key}-else`, depth, 'else'))
            walk(instr.elseBody, depth + 1)
          }
          rows.push(structural(`${key}-end`, depth, 'end'))
          break
        default:
          break
      }
    }
  }
  walk(fn.body, 1)
  return rows
}

const formatValue = (value: Interp.Value): string => {
  if (typeof value === 'bigint') return `${value} : i64`
  if (typeof value === 'number') return `${value}`
  switch (value._tag) {
    case 'NullRef':
      return 'ref.null'
    case 'FuncRef':
      return `funcref ${value.funcIndex}`
    case 'HostRef':
      return 'hostref'
  }
}

export const frameRows = (frames: ReadonlyArray<Interp.FrameInfo>): ReadonlyArray<RowModel> =>
  frames
    .map(
      (frame, index): RowModel => ({
        key: `frame-${index}`,
        lead: index,
        label: `${frame.funcName ?? `func ${frame.funcIndex}`}()`,
        dot: index === frames.length - 1 ? 'ok' : 'node',
        detail: index === frames.length - 1 ? 'current' : '',
      }),
    )
    .reverse()

export const localRows = (locals: ReadonlyArray<Interp.LocalInfo>): ReadonlyArray<RowModel> =>
  locals.map(
    (local): RowModel => ({
      key: `local-${local.index}`,
      lead: local.index,
      label: local.name === undefined ? `local ${local.index}` : `$${local.name}`,
      detail: formatValue(local.value),
    }),
  )

export const stackRows = (values: ReadonlyArray<Interp.Value>): ReadonlyArray<RowModel> =>
  values
    .map(
      (value, index): RowModel => ({
        key: `stack-${index}`,
        lead: index,
        label: formatValue(value),
        detail: index === values.length - 1 ? 'top' : '',
      }),
    )
    .reverse()

export const globalRows = (globals: ReadonlyArray<Interp.GlobalInfo>): ReadonlyArray<RowModel> =>
  globals.map(
    (global, index): RowModel => ({
      key: `global-${index}`,
      label: `$${global.name ?? index}`,
      detail: `${formatValue(global.value)}${global.mutable ? '' : ' · const'}`,
    }),
  )

export const memoryRows = (bytes: Uint8Array): ReadonlyArray<RowModel> => {
  const rows: Array<RowModel> = []
  for (let offset = 0; offset < bytes.length; offset += 16) {
    const slice = Array.from(bytes.subarray(offset, offset + 16))
    rows.push({
      key: `mem-${offset}`,
      lead: `${offset.toString(16).padStart(4, '0')}`,
      label: slice.map((byte) => byte.toString(16).padStart(2, '0')).join(' '),
      detail: slice.some((byte) => byte !== 0) ? '' : 'zero',
    })
  }
  return rows
}
