import { Analysis } from '@silk-effect/compiler'
import { describe, expect, it } from 'vitest'
import { presets } from './presets'

const encoder = new TextEncoder()

const snapshotOf = (preset: (typeof presets)[number]): Analysis.Snapshot =>
  Analysis.make({
    rootModule: preset.root,
    sources: new Map(
      Object.entries(preset.modules).map(([name, text]) => [name, encoder.encode(text)]),
    ),
  })

describe('preset catalog', () => {
  it('has a unique label per preset, so the picker can key on it', () => {
    const labels = presets.map((preset) => preset.label)
    expect(new Set(labels).size).toBe(labels.length)
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

  it('keeps the phases the labs shipped presets for', () => {
    const groups = new Set(presets.map((preset) => preset.group))
    expect(groups).toEqual(
      new Set(['syntax', 'modules', 'headers', 'ownership', 'discovery', 'backend']),
    )
  })

  it('still carries the multi-module programs, which single-source presets cannot express', () => {
    const multi = presets.filter((preset) => Object.keys(preset.modules).length > 1)
    expect(multi.length).toBeGreaterThanOrEqual(5)
  })
})
