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
const layoutPreset = presets.find((preset) => preset.label === 'ok · Validated target Layout')
const allocationPreset = presets.find(
  (preset) => preset.label === 'ok · Self-contained Allocation contract',
)

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
  it('has a unique label per preset, so the picker can key on it', () => {
    const labels = presets.map((preset) => preset.label)
    expect(new Set(labels).size).toBe(labels.length)
  })

  it('prefixes every label with ok, fail, or trap so intent is visible in the picker', () => {
    for (const preset of presets) {
      expect(preset.label, preset.label).toMatch(/^(ok|fail|trap) · /)
    }
    expect(presets.some((preset) => preset.label.startsWith('ok · '))).toBe(true)
    expect(presets.some((preset) => preset.label.startsWith('fail · '))).toBe(true)
    expect(presets.some((preset) => preset.label.startsWith('trap · '))).toBe(true)
  })

  it('keeps fail-prefixed presets as the ones that surface diagnostics', () => {
    for (const preset of presets) {
      const snapshot = snapshotOf(preset)
      const hasDiagnostics = Analysis.diagnostics(snapshot).length > 0
      if (preset.label.startsWith('fail · ')) {
        expect(hasDiagnostics, preset.label).toBe(true)
      } else if (preset.label.startsWith('ok · ')) {
        expect(hasDiagnostics, preset.label).toBe(false)
      }
    }
  })

  it('roots every preset at a module it actually defines', () => {
    for (const preset of presets) {
      expect(Object.keys(preset.modules), preset.label).toContain(preset.root)
    }
  })

  // Presets exist to put the compiler in a specific state, including deliberately broken states.
  // What must never happen is a preset that crashes the driver: a mistranscribed program would
  // take down whichever pane rendered it.
  it('builds a snapshot for every preset, damaged programs included', () => {
    for (const preset of presets) {
      expect(() => snapshotOf(preset), preset.label).not.toThrow()
    }
  })

  // Lowering is target-aware, so a preset that builds for one target is not evidence it builds
  // for the rest — and the workbench lets any target be selected against any preset.
  it('builds a snapshot for every preset against every selectable target', () => {
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
  })

  it('keeps the phases the labs shipped presets for', () => {
    const groups = new Set(presets.map((preset) => preset.group))
    expect(groups).toEqual(
      new Set([
        'syntax',
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
    expect(Analysis.modules(native).map((module) => module.name)).toEqual([
      'app/Main',
      'compiler/Coverage',
      'compiler/Member',
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
