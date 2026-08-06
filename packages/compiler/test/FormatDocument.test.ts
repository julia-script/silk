import { assert, it } from '@effect/vitest'
import * as FormatDocument from '../src/internal/FormatDocument.js'

const decode = (bytes: Uint8Array): string => new TextDecoder().decode(bytes)

const delimited = (items: ReadonlyArray<string>): FormatDocument.Document => {
  const breakLine = FormatDocument.ifBreak(FormatDocument.hardLine)
  return FormatDocument.group(
    FormatDocument.concat(
      FormatDocument.text('call('),
      FormatDocument.indent(
        FormatDocument.concat(
          breakLine,
          FormatDocument.join(
            FormatDocument.concat(FormatDocument.text(','), FormatDocument.softLine),
            items.map(FormatDocument.text),
          ),
          FormatDocument.ifBreak(FormatDocument.text(',')),
        ),
      ),
      breakLine,
      FormatDocument.text(')'),
    ),
  )
}

it('keeps a group flat when it exactly fits the remaining width', () => {
  assert.strictEqual(decode(FormatDocument.render(delimited(['a', 'b']), 10)), 'call(a, b)\n')
})

it('breaks a group deterministically when its flat form exceeds the width', () => {
  const document = delimited(['alpha', 'beta'])
  const expected = 'call(\n  alpha,\n  beta,\n)\n'

  assert.strictEqual(decode(FormatDocument.render(document, 12)), expected)
  assert.strictEqual(decode(FormatDocument.render(document, 12)), expected)
})

it('nests indentation and removes horizontal whitespace before every line break', () => {
  const document = FormatDocument.concat(
    FormatDocument.text('outer {   '),
    FormatDocument.indent(
      FormatDocument.concat(
        FormatDocument.hardLine,
        FormatDocument.text('inner {\t'),
        FormatDocument.indent(
          FormatDocument.concat(FormatDocument.hardLine, FormatDocument.text('value')),
        ),
      ),
    ),
  )

  assert.strictEqual(decode(FormatDocument.render(document)), 'outer {\n  inner {\n    value\n')
})

it('normalizes the document envelope to exactly one final LF', () => {
  const document = FormatDocument.concat(
    FormatDocument.text('value'),
    FormatDocument.hardLine,
    FormatDocument.hardLine,
  )

  assert.strictEqual(decode(FormatDocument.render(document)), 'value\n')
})
