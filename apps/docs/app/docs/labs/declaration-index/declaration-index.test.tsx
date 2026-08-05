import { DeclarationIndex, ModuleClosure } from '@silk-effect/compiler'
import { renderToStaticMarkup } from 'react-dom/server'
import { describe, expect, it } from 'vitest'
import { DeclarationIndexLab, IndexView } from './declaration-index'

const encoder = new TextEncoder()

const collect = (
  rootModule: string,
  entries: ReadonlyArray<readonly [string, string]>,
): DeclarationIndex.Index =>
  DeclarationIndex.collect(
    ModuleClosure.load({
      rootModule,
      sources: new Map(entries.map(([name, text]) => [name, encoder.encode(text)])),
    }),
  )

describe('IndexView', () => {
  it('lists headers across modules with canonical identities and signatures', () => {
    const index = collect('root', [
      ['root', 'import lib\npub fn main() -> I32 { return 42 }'],
      ['lib', 'pub fn choose(left: I32, right: I32) -> I32 { return left }'],
    ])
    const markup = renderToStaticMarkup(<IndexView index={index} />)

    expect(markup.indexOf('id="index-lib"')).toBeLessThan(markup.indexOf('id="index-root"'))
    expect(markup).toContain('lib.choose')
    expect(markup).toContain('root.main')
    expect(markup).toContain('(left: I32, right: I32) -&gt; I32')
  })

  it('marks duplicate and unidentified headers explicitly', () => {
    const index = collect('root', [
      ['root', 'pub fn same() -> I32 { return 1 }\npub fn same() -> I32 { return 2 }\npub fn () -> I32 { return 0 }'],
    ])
    const markup = renderToStaticMarkup(<IndexView index={index} />)

    expect(markup).toContain('duplicate of root.same')
    expect(markup).toContain('unidentified')
  })
})

describe('DeclarationIndexLab', () => {
  it('renders the default preset with editable sources and no diagnostics', () => {
    const markup = renderToStaticMarkup(<DeclarationIndexLab />)

    expect(markup).toContain('aria-label="Index presets"')
    expect(markup).toContain('id="index-source-root"')
    expect(markup).toContain('No diagnostics')
  })

  it('surfaces header diagnostics for unknown types', () => {
    const index = collect('root', [['root', 'pub fn puzzle(value: Mystery) -> Enigma { return 0 }']])

    expect(index.diagnostics.map((diagnostic) => diagnostic.code)).toEqual([
      'SEM0001',
      'SEM0001',
    ])
  })
})
