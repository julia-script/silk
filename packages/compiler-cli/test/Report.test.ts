import { assert, it } from '@effect/vitest'
import type * as Diagnostic from '@silk-effect/compiler/Diagnostic'
import type * as Driver from '@silk-effect/compiler/Driver'
import * as Report from '../src/Report.js'

const ascii = (text: string): Uint8Array =>
  Uint8Array.from(text.split('').map((character) => character.charCodeAt(0)))

const source = (text: string) => ({ path: 'main.silk', bytes: ascii(text) })

// Spans are nominal, so tests build them structurally rather than through the private constructor.
const span = (start: number, end: number): Diagnostic.Diagnostic['span'] =>
  ({ sourceId: 'main', start, end }) as unknown as Diagnostic.Diagnostic['span']

const diagnostic = (
  overrides: Partial<Diagnostic.Diagnostic> & { readonly span: Diagnostic.Diagnostic['span'] },
): Diagnostic.Diagnostic =>
  ({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: 'SEM0001',
    severity: 'error',
    message: 'Unknown type Nope',
    reason: { _tag: 'UnknownType' },
    ...overrides,
  }) as Diagnostic.Diagnostic

it('reports the first byte as line 1, column 1', () => {
  assert.deepStrictEqual(Report.positionAt(ascii('abc'), 0), { line: 1, column: 1 })
})

it('counts lines and restarts columns after each newline', () => {
  const bytes = ascii('one\ntwo\nthree')
  assert.deepStrictEqual(Report.positionAt(bytes, 4), { line: 2, column: 1 })
  assert.deepStrictEqual(Report.positionAt(bytes, 6), { line: 2, column: 3 })
  assert.deepStrictEqual(Report.positionAt(bytes, 8), { line: 3, column: 1 })
})

it('clamps an offset past the end to the final position', () => {
  const bytes = ascii('ab')
  assert.deepStrictEqual(Report.positionAt(bytes, 99), { line: 1, column: 3 })
})

it('renders a diagnostic with a clickable path, position, code, and message', () => {
  const rendered = Report.diagnostic(
    diagnostic({ span: span(17, 21) }),
    source('pub fn main() -> Nope { return 42 }'),
  )
  assert.strictEqual(rendered, 'main.silk:1:18: error[SEM0001] Unknown type Nope')
})

it('indents diagnostic notes beneath their diagnostic', () => {
  const rendered = Report.diagnostic(
    diagnostic({ span: span(0, 1), notes: ['try I32'] }),
    source('pub fn main() -> Nope { return 42 }'),
  )
  assert.strictEqual(rendered, 'main.silk:1:1: error[SEM0001] Unknown type Nope\n  note: try I32')
})

it('explains a missing entry in terms of the declaration the user must add', () => {
  const outcome: Driver.Outcome = {
    _tag: 'NoEntry',
    reason: 'MissingEntry',
    diagnostics: [],
    report: [],
  }
  assert.strictEqual(
    Report.outcome(outcome, source('pub fn other() -> I32 { return 1 }')),
    'No entry point: the root module declares no `main`',
  )
})

it('names the executable, target, and symbol count on success', () => {
  const outcome: Driver.Outcome = {
    _tag: 'Compiled',
    executable: '/tmp/a.out',
    target: { id: 'aarch64-apple-darwin' } as Driver.Compiled['target'],
    symbols: [
      {
        declaration: { _tag: 'CanonicalDeclarationId', module: 'main', name: 'main' },
        symbol: 'silk_main',
      },
    ],
    diagnostics: [],
    report: [],
  }
  assert.strictEqual(
    Report.outcome(outcome, source('pub fn main() -> I32 { return 42 }')),
    'Compiled main.silk -> /tmp/a.out (aarch64-apple-darwin, 1 symbols)',
  )
})

it('keeps the failing command so a toolchain failure is reproducible by hand', () => {
  const outcome: Driver.Outcome = {
    _tag: 'Failed',
    stage: 'link',
    failure: {
      _tag: 'ToolchainFailure',
      planned: {
        _tag: 'PlannedCommand',
        target: { id: 'aarch64-apple-darwin' } as Driver.Compiled['target'],
        command: 'clang',
        arguments: ['-o', 'a.out'],
      },
      reason: { _tag: 'ProcessFailure' },
      status: 1,
      output: 'ld: symbol not found',
    } as Driver.Failed['failure'],
    report: [],
  }
  const rendered = Report.outcome(outcome, source(''))
  assert.strictEqual(
    rendered,
    [
      'link stage failed: process exited with status 1',
      '  command: clang -o a.out',
      '  | ld: symbol not found',
    ].join('\n'),
  )
})

it('treats only a Compiled outcome as success', () => {
  assert.strictEqual(
    Report.succeeded({ _tag: 'NoEntry', reason: 'MissingEntry', diagnostics: [], report: [] }),
    false,
  )
})
