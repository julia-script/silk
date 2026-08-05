import { Analysis } from '@silk-effect/compiler'
import type { ModuleClosure } from '@silk-effect/compiler'
import { renderToStaticMarkup } from 'react-dom/server'
import { describe, expect, it } from 'vitest'
import { ClosureView, ModuleClosureLab } from './module-closure'

const encoder = new TextEncoder()
const mainFn = 'pub fn main() -> I32 { return 42 }'

const load = (
  rootModule: string,
  entries: ReadonlyArray<readonly [string, string]>,
): ModuleClosure.Closure =>
  Analysis.make({
    rootModule,
    sources: new Map(entries.map(([name, text]) => [name, encoder.encode(text)])),
  }).closure

describe('ClosureView', () => {
  it('lists a diamond closure in canonical order without cycles', () => {
    const closure = load('root', [
      ['root', `import left\nimport right\n${mainFn}`],
      ['left', `import shared\n${mainFn}`],
      ['right', `import shared\n${mainFn}`],
      ['shared', mainFn],
    ])
    const markup = renderToStaticMarkup(<ClosureView closure={closure} />)

    expect(markup.indexOf('id="module-left"')).toBeLessThan(markup.indexOf('id="module-right"'))
    expect(markup.indexOf('id="module-right"')).toBeLessThan(markup.indexOf('id="module-root"'))
    expect(markup.indexOf('id="module-root"')).toBeLessThan(markup.indexOf('id="module-shared"'))
    expect(markup).toContain('shared · resolved')
    expect(markup).toContain('No cycles')
    expect(markup).not.toContain('· cycle')
  })

  it('marks cycle members and names the cycle canonically', () => {
    const closure = load('root', [
      ['root', `import beta\n${mainFn}`],
      ['beta', `import gamma\n${mainFn}`],
      ['gamma', `import beta\n${mainFn}`],
    ])
    const markup = renderToStaticMarkup(<ClosureView closure={closure} />)

    expect(markup).toContain('beta → gamma')
    expect(markup).toContain('module · cycle')
    expect(markup).not.toContain('No cycles')
  })
})

describe('ModuleClosureLab', () => {
  it('renders the default diamond preset with editable module sources', () => {
    const markup = renderToStaticMarkup(<ModuleClosureLab />)

    expect(markup).toContain('aria-label="Closure presets"')
    expect(markup).toContain('id="module-source-root"')
    expect(markup).toContain('id="module-source-shared"')
    expect(markup).toContain('No cycles')
    expect(markup).toContain('No diagnostics')
  })

  it('surfaces unknown-import and self-import diagnostics through the unified panel', () => {
    const unknown = load('root', [['root', `import missing\n${mainFn}`]])
    const self = load('root', [['root', `import root\n${mainFn}`]])

    expect(
      unknown.diagnostics.map((diagnostic) => `${diagnostic.phase}:${diagnostic.code}`),
    ).toEqual(['module:MOD0001'])
    expect(self.diagnostics.map((diagnostic) => `${diagnostic.phase}:${diagnostic.code}`)).toEqual([
      'module:MOD0002',
    ])
  })
})
