import { Analysis } from '@silk-effect/compiler'
import { readFileSync } from 'node:fs'
import * as Snapshot from './snapshot'
import { describe, expect, it } from 'vitest'
import { presets } from './presets'
import { viewById } from './registry'
import type { ViewContext } from './registry'

const encoder = new TextEncoder()

const snapshotOf = (preset: (typeof presets)[number], target?: string): Analysis.Snapshot =>
  Snapshot.make({
    rootModule: preset.root,
    sources: new Map(
      Object.entries(preset.modules).map(([name, text]) => [name, encoder.encode(text)]),
    ),
    ...(target === undefined ? {} : { target }),
  })

const acceptancePreset = presets.find((preset) => preset.label === 'ok · Algorithmic coverage fold')
const exclusiveSlicePreset = presets.find(
  (preset) => preset.label === 'ok · Exclusive runtime slice',
)
const typedEffectPreset = presets.find((preset) => preset.label === 'ok · Typed Effect recovery')
const residualEffectPreset = presets.find(
  (preset) => preset.label === 'fail · Unhandled Effect residual',
)
const eagerEffectPreset = presets.find(
  (preset) => preset.label === 'ok · Eager setup, lazy Effect body',
)
const captureEffectPreset = presets.find(
  (preset) => preset.label === 'ok · Reusable exclusive capture',
)
const retryEffectPreset = presets.find(
  (preset) => preset.label === 'ok · Retry with persistent capture',
)
const providerEffectPreset = presets.find(
  (preset) => preset.label === 'ok · Existing provider capture',
)
const suspendedEffectLabels = [
  'ok · Suspended state resumes',
  'ok · Suspended map and flatMap',
  'ok · Stack-safe suspended recursion',
] as const
const suspendedEffectPresets = suspendedEffectLabels.map((label) =>
  presets.find((preset) => preset.label === label),
)
const loggingPreset = presets.find(
  (preset) => preset.label === 'ok · Portable Logger provider',
)
const layoutPreset = presets.find((preset) => preset.label === 'ok · Validated target Layout')
const allocationPreset = presets.find(
  (preset) => preset.label === 'ok · Self-contained Allocation contract',
)
const callablePreset = presets.find(
  (preset) => preset.label === 'ok · Named function and stored section',
)
const ungroupedRunPreset = presets.find(
  (preset) => preset.label === 'ok · Ungrouped run composes first',
)
const groupedRunPreset = presets.find(
  (preset) => preset.label === 'ok · Grouped run transforms result',
)
const pipedEffectLabels = [
  'ok · Piped success composition',
  'ok · Piped failure recovery',
  'ok · Piped acquired provider',
] as const
const pipedEffectPresets = pipedEffectLabels.map((label) =>
  presets.find((preset) => preset.label === label),
)
const ownedSequenceLabels = [
  'ok · Vector growing append',
  'ok · Vector failed growth preserves contents',
  'ok · Vector destruction order',
  'ok · Vector early drop',
  'ok · Scanner returns owned tokens',
] as const
const ownedSequencePresets = ownedSequenceLabels.map((label) =>
  presets.find((preset) => preset.label === label),
)
const scannerPreset = ownedSequencePresets.at(-1)

const acceptanceContext = (
  preset: (typeof presets)[number],
  snapshot: Analysis.Snapshot,
): ViewContext => ({
  snapshot,
  modules: preset.modules,
  root: preset.root,
  mode: 'release',
  profile: 'release',
  cursor: undefined,
  onSelectSpan: () => undefined,
  evaluation: Analysis.evaluate(snapshot),
  onEvaluate: () => undefined,
  filter: '',
  showTrivia: false,
})

describe('preset catalog', () => {
  it('shows both completed recursion and a configured call-depth stop', () => {
    const completed = presets.find(
      (preset) => preset.label === 'ok · Completed runtime recursion',
    )
    const limited = presets.find(
      (preset) => preset.label === 'trap · Call-depth evaluation limit',
    )
    expect(completed).toBeDefined()
    expect(limited).toBeDefined()
    if (completed === undefined || limited === undefined) return

    expect(Analysis.evaluate(snapshotOf(completed))._tag).toBe('Completed')
    const outcome = Analysis.evaluate(snapshotOf(limited), limited.evaluation)
    expect(outcome._tag).toBe('Blocked')
    if (outcome._tag !== 'Blocked') return
    expect(outcome.reason._tag).toBe('EvaluationLimit')
    if (outcome.reason._tag !== 'EvaluationLimit') return
    expect(outcome.reason.kind).toBe('CallDepth')
    expect(outcome.reason.activeFrames).toHaveLength(4)
  })

  it('has a unique label per preset, so the picker can key on it', () => {
    const labels = presets.map((preset) => preset.label)
    expect(new Set(labels).size).toBe(labels.length)
  })

  it('gives every projected MIR row a unique React key', () => {
    const mirView = viewById('mir')
    expect(mirView).toBeDefined()
    if (mirView === undefined) return

    const collisions: Array<string> = []
    for (const preset of presets) {
      const snapshot = snapshotOf(preset, 'wasm32-unknown-unknown')
      const keys = mirView
        .project({
          snapshot,
          modules: preset.modules,
          root: preset.root,
          mode: 'release',
          profile: 'release',
          cursor: undefined,
          onSelectSpan: () => undefined,
          evaluation: undefined,
          onEvaluate: () => undefined,
          filter: '',
          showTrivia: false,
        })
        .rows.map((row) => row.key)
      const seen = new Set<string>()
      for (const key of keys) {
        if (seen.has(key)) collisions.push(`${preset.label}: ${key}`)
        seen.add(key)
      }
    }

    expect(collisions).toEqual([])
  }, 60_000)

  it('prefixes every label with ok, fail, or trap so intent is visible in the picker', () => {
    for (const preset of presets) {
      expect(preset.label, preset.label).toMatch(/^(ok|fail|trap) · /)
    }
    expect(presets.some((preset) => preset.label.startsWith('ok · '))).toBe(true)
    expect(presets.some((preset) => preset.label.startsWith('fail · '))).toBe(true)
    expect(presets.some((preset) => preset.label.startsWith('trap · '))).toBe(true)
  })

  it(
    'keeps fail-prefixed presets as the ones that surface diagnostics',
    () => {
      for (const preset of presets) {
        const snapshot = snapshotOf(preset)
        const hasDiagnostics = Analysis.diagnostics(snapshot).length > 0
        if (preset.label.startsWith('fail · ')) {
          expect(hasDiagnostics, preset.label).toBe(true)
        } else if (preset.label.startsWith('ok · ')) {
          expect(hasDiagnostics, preset.label).toBe(false)
        }
      }
    },
    60_000,
  )

  it('roots every preset at a module it actually defines', () => {
    for (const preset of presets) {
      expect(Object.keys(preset.modules), preset.label).toContain(preset.root)
    }
  })

  // Presets exist to put the compiler in a specific state, including deliberately broken states.
  // What must never happen is a preset that crashes the driver: a mistranscribed program would
  // take down whichever pane rendered it.
  it(
    'builds a snapshot for every preset, damaged programs included',
    () => {
      for (const preset of presets) {
        expect(() => snapshotOf(preset), preset.label).not.toThrow()
      }
    },
    60_000,
  )

  // Lowering is target-aware, so a preset that builds for one target is not evidence it builds
  // for the rest — and the workbench lets any target be selected against any preset.
  it(
    'builds a snapshot for every preset against every selectable target',
    () => {
      for (const target of [
        'aarch64-apple-darwin',
        'x86_64-unknown-linux-gnu',
        'aarch64-unknown-linux-gnu',
        'wasm32-unknown-unknown',
      ]) {
        for (const preset of presets) {
          expect(() => snapshotOf(preset, target), `${preset.label} · ${target}`).not.toThrow()
        }
      }
    },
    180_000,
  )

  it('keeps the phases the labs shipped presets for', () => {
    const groups = new Set(presets.map((preset) => preset.group))
    expect(groups).toEqual(
      new Set([
        'syntax',
        'evaluation',
        'generics',
        'acceptance',
        'modules',
        'headers',
        'structs',
        'names',
        'operators',
        'arrays',
        'ownership',
        'matching',
        'control',
        'discovery',
        'backend',
        'effects',
        'allocation',
        'callables',
        'interfaces',
      ]),
    )
  })

  it('still carries the multi-module programs, which single-source presets cannot express', () => {
    const multi = presets.filter((preset) => Object.keys(preset.modules).length > 1)
    expect(multi.length).toBeGreaterThanOrEqual(5)
  })

  it('keeps the algorithmic preset byte-identical to the three-engine fixture', () => {
    expect(acceptancePreset).toBeDefined()
    if (acceptancePreset === undefined) return

    expect(acceptancePreset.root).toBe('app/Main')
    expect(Object.keys(acceptancePreset.modules)).toEqual([
      'app/Main',
      'compiler/Member',
      'compiler/Coverage',
    ])
    for (const name of Object.keys(acceptancePreset.modules)) {
      const fixture = readFileSync(
        new URL(
          `../../../../packages/compiler/test/fixtures/algorithmic-acceptance/${name}.silk`,
          import.meta.url,
        ),
        'utf8',
      )
      expect(acceptancePreset.modules[name], name).toBe(fixture)
    }
  })

  it('keeps the exclusive slice preset byte-identical and visible through coordinated panes', () => {
    expect(exclusiveSlicePreset).toBeDefined()
    if (exclusiveSlicePreset === undefined) return
    const fixture = readFileSync(
      new URL(
        '../../../../packages/compiler/test/fixtures/runtime-slice-exclusive.silk',
        import.meta.url,
      ),
      'utf8',
    )
    expect(exclusiveSlicePreset.modules.main).toBe(fixture)
    const native = snapshotOf(exclusiveSlicePreset, 'aarch64-apple-darwin')
    const wasm = snapshotOf(exclusiveSlicePreset, 'wasm32-unknown-unknown')
    expect(Analysis.diagnostics(native)).toEqual([])
    expect(Analysis.evaluate(native)._tag).toBe('Completed')
    expect(Analysis.layoutOf(native)._tag).toBe('Available')
    expect(Analysis.mirOf(wasm)._tag).toBe('Available')
    expect(viewById('hir')).toBeDefined()
    expect(viewById('ownership')).toBeDefined()
    expect(viewById('layout')).toBeDefined()
    expect(viewById('evaluation')).toBeDefined()
    expect(viewById('backend')).toBeDefined()
  })

  it('exposes typed Effect recovery through the unified inspector', () => {
    expect(typedEffectPreset).toBeDefined()
    if (typedEffectPreset === undefined) return
    const native = snapshotOf(typedEffectPreset, 'aarch64-apple-darwin')
    const wasm = snapshotOf(typedEffectPreset, 'wasm32-unknown-unknown')
    expect(Analysis.diagnostics(native)).toEqual([])
    expect(Analysis.hirOf(native, typedEffectPreset.root)).toBeDefined()
    expect(Analysis.layoutOf(native)._tag).toBe('Available')
    expect(Analysis.mirOf(wasm)._tag).toBe('Available')
    const evaluation = Analysis.evaluate(native)
    expect(evaluation._tag).toBe('Completed')
    if (evaluation._tag === 'Completed') {
      expect(evaluation.result.value).toBe(42)
      expect(evaluation.trace.some((event) => event._tag === 'EffectFailure')).toBe(true)
      expect(evaluation.trace.some((event) => event._tag === 'EffectSuccess')).toBe(true)
    }
    for (const id of ['hir', 'ownership', 'layout', 'mir', 'evaluation', 'backend']) {
      expect(viewById(id)?.id, id).toBe(id)
    }
  })

  it('exposes suspension, continuation state, composition, and recursion in Labs', () => {
    for (const [index, preset] of suspendedEffectPresets.entries()) {
      expect(preset, suspendedEffectLabels[index]).toBeDefined()
      if (preset === undefined) continue

      const native = snapshotOf(preset, 'aarch64-apple-darwin')
      const wasm = snapshotOf(preset, 'wasm32-unknown-unknown')
      expect(Analysis.diagnostics(native), preset.label).toEqual([])
      expect(Analysis.mirOf(native)._tag, preset.label).toBe('Available')
      expect(Analysis.mirOf(wasm)._tag, preset.label).toBe('Available')

      const evaluation = Analysis.evaluate(native)
      expect(evaluation._tag, preset.label).toBe('Completed')
      if (evaluation._tag !== 'Completed') continue
      expect(evaluation.result.value, preset.label).toBe(42)
      expect(
        evaluation.trace.some((event) => event._tag === 'SuspensionOrigin'),
        preset.label,
      ).toBe(true)
      const requests = evaluation.trace.filter(
        (event) => event._tag === 'ContinuationRequest',
      ).length
      const releases = evaluation.trace.filter(
        (event) => event._tag === 'ContinuationRelease',
      ).length
      expect(requests, preset.label).toBeGreaterThan(0)
      expect(releases, preset.label).toBe(requests)
    }
  })

  it('exposes portable logging through the coordinated facade panes', () => {
    expect(loggingPreset).toBeDefined()
    if (loggingPreset === undefined) return
    const native = snapshotOf(loggingPreset, 'aarch64-apple-darwin')
    const wasm = snapshotOf(loggingPreset, 'wasm32-unknown-unknown')
    expect(Analysis.diagnostics(native)).toEqual([])
    expect(Analysis.modules(native).map((module) => module.name)).toContain('silk/logging')
    expect(Analysis.hirOf(native, 'silk/effects')).toBeDefined()
    expect(Analysis.mirOf(wasm)._tag).toBe('Available')
    const evaluation = Analysis.evaluate(native)
    expect(evaluation._tag).toBe('Completed')
    if (evaluation._tag === 'Completed') expect(evaluation.result.value).toBe(42)
    const context = acceptanceContext(loggingPreset, native)
    for (const id of ['closure', 'index', 'hir', 'ownership', 'instances', 'mir', 'evaluation', 'backend']) {
      const view = viewById(id)
      expect(view, id).toBeDefined()
      expect(() => view?.project(context), id).not.toThrow()
    }
  })

  it('keeps construction, reusable capture, and retry examples executable', () => {
    for (const [preset, expected] of [
      [eagerEffectPreset, 42],
      [captureEffectPreset, 12],
      [retryEffectPreset, 3],
      [providerEffectPreset, 42],
    ] as const) {
      expect(preset).toBeDefined()
      if (preset === undefined) continue
      const snapshot = snapshotOf(preset, 'aarch64-apple-darwin')
      expect(Analysis.diagnostics(snapshot), preset.label).toEqual([])
      const evaluation = Analysis.evaluate(snapshot)
      expect(evaluation._tag, preset.label).toBe('Completed')
      if (evaluation._tag === 'Completed') expect(evaluation.result.value).toBe(expected)
    }
  })

  it('keeps the heavy Effect pipeline examples executable', () => {
    for (const [index, label] of pipedEffectLabels.entries()) {
      const preset = pipedEffectPresets.at(index)
      expect(preset, label).toBeDefined()
      if (preset === undefined) continue
      const snapshot = snapshotOf(preset, 'aarch64-apple-darwin')
      expect(Analysis.diagnostics(snapshot), label).toEqual([])
      const evaluation = Analysis.evaluate(snapshot)
      expect(evaluation._tag, label).toBe('Completed')
      if (evaluation._tag === 'Completed') expect(evaluation.result.value, label).toBe(42)
    }
  })

  it('exposes callable environments and both run groupings through the unified inspector', () => {
    for (const preset of [callablePreset, ungroupedRunPreset, groupedRunPreset]) {
      expect(preset).toBeDefined()
      if (preset === undefined) continue
      const snapshot = snapshotOf(preset, 'aarch64-apple-darwin')
      expect(Analysis.diagnostics(snapshot), preset.label).toEqual([])
      const evaluation = Analysis.evaluate(snapshot)
      expect(evaluation._tag, preset.label).toBe('Completed')
      if (evaluation._tag === 'Completed') expect(evaluation.result.value).toBe(42)
      for (const id of ['hir', 'ownership', 'instances', 'layout', 'mir', 'evaluation', 'backend']) {
        expect(viewById(id)?.id, `${preset.label} · ${id}`).toBe(id)
      }
    }
  })

  it('shows validated Layout and affine Allocation facts in the unified inspector', () => {
    expect(layoutPreset).toBeDefined()
    expect(allocationPreset).toBeDefined()
    if (layoutPreset === undefined || allocationPreset === undefined) return

    const layout = snapshotOf(layoutPreset, 'aarch64-apple-darwin')
    expect(Analysis.diagnostics(layout)).toEqual([])
    const evaluation = Analysis.evaluate(layout)
    expect(evaluation._tag).toBe('Completed')
    if (evaluation._tag === 'Completed') expect(evaluation.result.value).toBe(42)

    const allocation = snapshotOf(allocationPreset, 'wasm32-unknown-unknown')
    expect(Analysis.diagnostics(allocation)).toEqual([])
    expect(Analysis.hirOf(allocation, allocationPreset.root)).toBeDefined()
    expect(Analysis.ownershipOf(allocation, allocationPreset.root)).toBeDefined()
    expect(Analysis.layoutOf(allocation)._tag).toBe('Available')
  })

  // The allocation presets are the workbench's only view of the raw storage substrate, so each
  // one has to keep demonstrating the case its label claims — on both an address width where
  // usize is 64 bits and one where it is 32.
  it('runs every accepted allocation preset to completion on both target widths', () => {
    for (const label of [
      'ok · Self-contained Allocation contract',
      'ok · Early drop releases the buffer once',
      'ok · Zero-sized elements keep distinct owners',
      'ok · Drop hook runs before field cleanup',
    ]) {
      const preset = presets.find((candidate) => candidate.label === label)
      expect(preset, label).toBeDefined()
      if (preset === undefined) continue
      for (const target of ['aarch64-apple-darwin', 'wasm32-unknown-unknown']) {
        const snapshot = snapshotOf(preset, target)
        expect(Analysis.diagnostics(snapshot), `${label} · ${target}`).toEqual([])
        const evaluation = Analysis.evaluate(snapshot)
        expect(evaluation._tag, `${label} · ${target}`).toBe('Completed')
        if (evaluation._tag === 'Completed')
          expect(evaluation.result.value, `${label} · ${target}`).toBe(42)
      }
    }
  })

  it('keeps the scanner preset byte-identical to the acceptance fixture', () => {
    expect(scannerPreset).toBeDefined()
    if (scannerPreset === undefined) return
    const fixture = readFileSync(
      new URL(
        '../../../../packages/compiler/test/fixtures/scanner-acceptance/Main.silk',
        import.meta.url,
      ),
      'utf8',
    )
    expect(scannerPreset.modules.main).toBe(fixture)
  })

  it('projects every owned-sequence preset through the coordinated analysis panes', () => {
    for (const [index, label] of ownedSequenceLabels.entries()) {
      const preset = ownedSequencePresets.at(index)
      expect(preset, label).toBeDefined()
      if (preset === undefined) continue
      for (const target of ['aarch64-apple-darwin', 'wasm32-unknown-unknown']) {
        const snapshot = snapshotOf(preset, target)
        expect(Analysis.diagnostics(snapshot), `${label} · ${target}`).toEqual([])
        const context = acceptanceContext(preset, snapshot)
        for (const viewId of ['ownership', 'mir', 'evaluation']) {
          const view = viewById(viewId)
          expect(view, `${label} · ${viewId}`).toBeDefined()
          expect(view?.project(context).rows.length, `${label} · ${viewId}`).toBeGreaterThan(0)
        }
        const evaluated = Analysis.evaluate(snapshot)
        expect(evaluated._tag, `${label} · ${target}`).toBe('Completed')
        if (evaluated._tag === 'Completed')
          expect(evaluated.result.value, `${label} · ${target}`).toBe(42)
      }
    }
  })

  it('projects the parametric Vector Drop conformance and its hook', () => {
    const preset = ownedSequencePresets.at(0)
    expect(preset).toBeDefined()
    if (preset === undefined) return
    const snapshot = snapshotOf(preset, 'wasm32-unknown-unknown')
    const view = viewById('index')
    expect(view).toBeDefined()
    const rows = view?.project(acceptanceContext(preset, snapshot)).rows ?? []
    expect(
      rows.some((row) => row.label.startsWith('impl<T>') && row.label.includes('Vector<T>')),
    ).toBe(true)
    expect(rows.some((row) => row.detail?.includes('drop hook'))).toBe(true)
  })

  it('shows growth, rollback, destruction, early drop, and scanner tokens in evaluation', () => {
    const outcomes = new Map(
      ownedSequencePresets.flatMap((preset) => {
        if (preset === undefined) return []
        const evaluated = Analysis.evaluate(snapshotOf(preset, 'wasm32-unknown-unknown'))
        return evaluated._tag === 'Completed' ? [[preset.label, evaluated.trace] as const] : []
      }),
    )
    const tags = (label: (typeof ownedSequenceLabels)[number]) =>
      (outcomes.get(label) ?? []).map((event) => event._tag)
    // Growth migrates its elements with one bulk copy, so the trace shows RawBufferCopy where it
    // used to show one SlotTake per migrated element; the appended element still writes a slot.
    expect(tags('ok · Vector growing append')).toEqual(
      expect.arrayContaining([
        'AllocationAcquire',
        'RawBufferCopy',
        'SlotWrite',
        'AllocationRelease',
      ]),
    )
    expect(tags('ok · Vector failed growth preserves contents')).toEqual(
      expect.arrayContaining(['EffectFailure', 'AllocationAcquire', 'AllocationRelease']),
    )
    expect(tags('ok · Vector destruction order')).toEqual(
      expect.arrayContaining(['SlotDrop', 'AllocationRelease']),
    )
    expect(tags('ok · Vector early drop')).toEqual(
      expect.arrayContaining(['SlotDrop', 'AllocationRelease']),
    )
    const scannerTrace = outcomes.get('ok · Scanner returns owned tokens') ?? []
    expect(
      scannerTrace.flatMap((event) =>
        event._tag === 'Binding' &&
        event.target.name === 'observe' &&
        event.value._tag === 'I32Value'
          ? [event.value.value]
          : [],
      ),
    ).toEqual([1, 2, 3, 1, 2, 3, 1, 2, 3, 1])
  })

  // The boundary is the point of the substrate: raw storage outside `unsafe` has to stay a
  // frontend rejection rather than something the evaluator quietly runs.
  it('keeps raw storage outside an unsafe block rejected before evaluation', () => {
    const preset = presets.find(
      (candidate) => candidate.label === 'fail · Raw storage outside unsafe is rejected',
    )
    expect(preset).toBeDefined()
    if (preset === undefined) return
    const snapshot = snapshotOf(preset, 'aarch64-apple-darwin')
    expect(Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code)).toEqual(['SEM0082'])
    expect(Analysis.evaluate(snapshot)._tag).toBe('Blocked')
  })

  it('keeps an unhandled Effect residual visibly stopped in the unified inspector', () => {
    expect(residualEffectPreset).toBeDefined()
    if (residualEffectPreset === undefined) return
    const snapshot = snapshotOf(residualEffectPreset, 'aarch64-apple-darwin')

    expect(Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code)).toContain(
      'SEM0066',
    )
    expect(Analysis.evaluate(snapshot)._tag).toBe('Blocked')
  })

  it('coordinates every acceptance phase through existing panes', () => {
    expect(acceptancePreset).toBeDefined()
    if (acceptancePreset === undefined) return

    const native = snapshotOf(acceptancePreset, 'aarch64-apple-darwin')
    const wasm = snapshotOf(acceptancePreset, 'wasm32-unknown-unknown')
    expect(Analysis.diagnostics(native)).toEqual([])
    // `usize` renders and reads decimal text, so naming it reaches the formatting stack and the
    // owned text it produces. The closure is an analysis fact, not an artifact cost.
    expect(Analysis.modules(native).map((module) => module.name)).toEqual([
      'app/Main',
      'compiler/Coverage',
      'compiler/Member',
      'silk/bytes',
      'silk/core',
      'silk/format',
      'silk/i32',
      'silk/i64',
      'silk/layout',
      'silk/option',
      'silk/order',
      'silk/raw-buffer',
      'silk/result',
      'silk/slot',
      'silk/string',
      'silk/u32',
      'silk/u64',
      'silk/u8',
      'silk/usize',
      'silk/vector',
    ])
    for (const name of Object.keys(acceptancePreset.modules)) {
      expect(Analysis.hirOf(native, name), name).toBeDefined()
      expect(Analysis.ownershipOf(native, name), name).toBeDefined()
    }
    expect(Analysis.instancesOf(native).instances.length).toBeGreaterThan(0)
    expect(Analysis.layoutOf(native)._tag).toBe('Available')
    expect(Analysis.mirOf(native)._tag).toBe('Available')
    const evaluation = Analysis.evaluate(native)
    expect(evaluation._tag).toBe('Completed')
    if (evaluation._tag === 'Completed') expect(evaluation.result.value).toBe(42)

    for (const id of [
      'source',
      'closure',
      'resolution',
      'hir',
      'ownership',
      'instances',
      'layout',
      'mir',
      'evaluation',
      'backend',
    ]) {
      expect(viewById(id)?.id, id).toBe(id)
    }
    expect(viewById('acceptance')).toBeUndefined()

    const backend = viewById('backend')
    expect(backend).toBeDefined()
    if (backend === undefined) return
    const nativeView = backend.project(acceptanceContext(acceptancePreset, native))
    const wasmView = backend.project(acceptanceContext(acceptancePreset, wasm))
    expect(nativeView.unavailable).toBeUndefined()
    expect(nativeView.rows.length).toBeGreaterThan(0)
    expect(wasmView.unavailable).toBeUndefined()
    expect(wasmView.facts?.map((fact) => fact.text)).toContain('runs · agrees')
  })
})
