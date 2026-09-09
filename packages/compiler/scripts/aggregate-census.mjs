// External, read-only diagnostic instrumentation; never used for timing comparisons.
import { registerHooks } from 'node:module'
import { readFileSync, writeFileSync } from 'node:fs'
import { fileURLToPath } from 'node:url'
import { createHash } from 'node:crypto'
import * as Config from 'effect/Config'
import * as Effect from 'effect/Effect'

// Enable with node --import ./packages/compiler/scripts/aggregate-census.mjs <entry>.
// A fresh output file is mandatory; this harness must never be enabled in timed samples.
const outputPath = Effect.runSync(Config.nonEmptyString('SILK_AGGREGATE_OUTPUT'))

const entries = new Map()
const functions = new WeakMap()
const typeCache = new Map()
let rootSnapshot
const snapshots = []
const decode = (x) => Buffer.from(x.bytes).toString('utf8')
export function enter(entry, roots, lanes, semantic, encode) {
  const types = entry.fn.localTypes.map((type) => {
    let result = typeCache.get(type)
    if (result) return result
    const spelling = encode(semantic(type))
    result = {
      kind: typeof type === 'string' ? type : type._tag,
      type: spelling.slice(0, 1000),
      typeHash: createHash('sha256').update(spelling).digest('hex'),
      lanes: lanes(type).length,
    }
    typeCache.set(type, result)
    return result
  })
  const operations = Object.create(null)
  for (const block of entry.linear)
    for (const op of block.operations) operations[op._tag] = (operations[op._tag] ?? 0) + 1
  const row = {
    symbol: entry.symbol,
    operations,
    linearBlocks: entry.linear.length,
    locals: types.map((type, root) => ({
      root,
      ...type,
      mutable: roots.mutable.has(root),
      address: roots.address.has(root),
      events: Object.create(null),
    })),
  }
  entries.set(entry.symbol, row)
  functions.set(entry.fn, row)
}
export function event(context, root, kind, width) {
  const local = functions.get(context.fn)?.locals[root]
  if (!local) throw new Error('Census lost root attribution')
  const count = (local.events[kind] ??= { invocations: 0, lanes: 0 })
  count.invocations++
  count.lanes += width
}
export function record(state) {
  if (!state.functions.some((fn) => entries.has(decode(state.globals[fn.global].name)))) return
  const output = []
  for (const fn of state.functions) {
    const body = fn.body
    if (!body) continue
    const symbol = decode(state.globals[fn.global].name)
    const row = entries.get(symbol) ?? { symbol, locals: [], operations: {} }
    const uses = new Uint32Array(body.values.length)
    const visit = (value) => {
      if (value === null || typeof value !== 'object') return
      if (value._tag === 'Local') {
        uses[value.value]++
        return
      }
      if (Array.isArray(value)) {
        for (const x of value) visit(x)
        return
      }
      for (const key of Object.keys(value))
        if (!['name', 'fastMath', 'access'].includes(key)) visit(value[key])
    }
    for (const inst of body.instructions) visit(inst)
    row.counts = Object.create(null)
    row.loads = Object.create(null)
    row.instructions = body.instructions.length
    row.unusedLoads = 0
    for (const inst of body.instructions) {
      row.counts[inst._tag] = (row.counts[inst._tag] ?? 0) + 1
      if (inst._tag !== 'Load') continue
      const name = decode(inst.name)
      const unused = uses[inst.result] === 0
      const prefix = name.replace(/[0-9].*$/, '').replace(/_.*/, '')
      const group = (row.loads[prefix] ??= { total: 0, unused: 0 })
      group.total++
      if (unused) {
        group.unused++
        row.unusedLoads++
      }
      const root = /^(?:mut|reload|place)(\d+)_/.exec(name)
      if (root) {
        const local = row.locals[Number(root[1])]
        if (!local) throw new Error(`Missing local ${symbol} ${name}`)
        local.loads = (local.loads ?? 0) + 1
        local.unusedLoads = (local.unusedLoads ?? 0) + Number(unused)
      }
    }
    output.push(row)
  }
  const totals = Object.create(null)
  for (const row of output)
    for (const [tag, count] of Object.entries(row.counts)) totals[tag] = (totals[tag] ?? 0) + count
  const snapshotInstructions = state.functions.reduce(
    (total, fn) => total + (fn.body?.instructions.length ?? 0),
    0,
  )
  if (Object.values(totals).reduce((total, count) => total + count, 0) !== snapshotInstructions)
    throw new Error('Census opcode totals do not reconcile with the immutable snapshot')
  const first = rootSnapshot === undefined
  // The requested module is emitted first; later emissions compile helper sources.
  rootSnapshot ??= { snapshotInstructions, totals, functions: output }
  snapshots.push({
    ordinal: snapshots.length,
    functions: output.length,
    snapshotInstructions,
    totals,
  })
  writeFileSync(outputPath, JSON.stringify({ ...rootSnapshot, snapshots }), {
    flag: first ? 'wx' : 'w',
  })
}
const replace = (source, marker, inserted) => {
  if (source.split(marker).length !== 2) throw new Error(`Census marker not unique: ${marker}`)
  return source.replace(marker, inserted)
}
registerHooks({
  load(url, context, nextLoad) {
    if (!url.startsWith('file:')) return nextLoad(url, context)
    let source
    if (url.endsWith('/packages/compiler/dist/NativeFunction.js')) {
      source = readFileSync(fileURLToPath(url), 'utf8')
      const marker = 'const roots = discoverRoots(entry.fn, entry.linear);'
      source =
        `import { enter } from ${JSON.stringify(import.meta.url)};\nimport * as CensusType from './Type.js';\n` +
        replace(
          source,
          marker,
          marker + '\nenter(entry, roots, valueLanesFor, Mir.semanticType, CensusType.encode);',
        )
    } else if (url.endsWith('/packages/compiler/dist/NativeStorage.js')) {
      source = readFileSync(fileURLToPath(url), 'utf8')
      source = replace(
        source,
        'const loaded = [];',
        "event(context, root, 'joinReload', storage.length);\nconst loaded = [];",
      )
      source = replace(
        source,
        'const reloadId = context.sequences.reload++;',
        "event(context, root, 'aliasReload', NativeType.valueLanesFor(context.types, type).length);\nconst reloadId = context.sequences.reload++;",
      )
      source = replace(
        source,
        'for (const [lane, pointer] of storage.entries()) {\n        const stored',
        "event(context, root.ordinal, 'mutableStore', storage.length);\nfor (const [lane, pointer] of storage.entries()) {\n        const stored",
      )
      source = replace(
        source,
        'export const storeAddressValues = Effect.fnUntraced(function* (context, root, values, name) {',
        "export const storeAddressValues = Effect.fnUntraced(function* (context, root, values, name) {\nevent(context, root, 'addressStore', values.length);",
      )
      source = `import { event } from ${JSON.stringify(import.meta.url)};\n` + source
      for (const [marker, eventText] of [
        [
          'export const materialize = Effect.fnUntraced(function* (context, local) {',
          "event(context, local.ordinal, 'boundaryMaterialize', NativeType.valueLanesFor(context.types, context.fn.localTypes[local.ordinal]).length);",
        ],
        [
          'export const copyLocal = Effect.fnUntraced(function* (context, destination, source) {',
          "event(context, destination.ordinal, 'valueCopy', 0);",
        ],
        [
          'export const writeLocal = Effect.fnUntraced(function* (context, root, values) {',
          "event(context, root, 'destinationStore', values.length);",
        ],
      ])
        source = replace(source, marker, marker + '\n' + eventText)
    } else if (url.endsWith('/packages/llvm/dist/Bitcode.js')) {
      source = readFileSync(fileURLToPath(url), 'utf8')
      const marker = "const state = yield* BuilderState.snapshot(self, 'Bitcode.encode');"
      source =
        `import { record } from ${JSON.stringify(import.meta.url)};\n` +
        replace(source, marker, marker + '\nrecord(state);')
    } else return nextLoad(url, context)
    return { format: 'module', shortCircuit: true, source }
  },
})
