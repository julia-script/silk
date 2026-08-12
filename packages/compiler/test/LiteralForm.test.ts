import { assert, it } from '@effect/vitest'
import * as LiteralForm from '../src/LiteralForm.js'

const bytes = (value: string): Uint8Array => new TextEncoder().encode(value)

it('recognizes every committed form longest-first', () => {
  assert.deepEqual(
    LiteralForm.forms.map(({ category, modifier, delimiterWidth, escapePolicy, tokenKind }) => ({
      category,
      modifier,
      delimiterWidth,
      escapePolicy,
      tokenKind,
    })),
    [
      {
        category: 'Bytes',
        modifier: 'b',
        delimiterWidth: 3,
        escapePolicy: 'Escaped',
        tokenKind: 'ByteStringLiteral',
      },
      {
        category: 'Text',
        modifier: 'r',
        delimiterWidth: 3,
        escapePolicy: 'Raw',
        tokenKind: 'TextLiteral',
      },
      {
        category: 'Text',
        modifier: '',
        delimiterWidth: 3,
        escapePolicy: 'Escaped',
        tokenKind: 'TextLiteral',
      },
      {
        category: 'Bytes',
        modifier: 'b',
        delimiterWidth: 1,
        escapePolicy: 'Escaped',
        tokenKind: 'ByteStringLiteral',
      },
      {
        category: 'Text',
        modifier: 'r',
        delimiterWidth: 1,
        escapePolicy: 'Raw',
        tokenKind: 'TextLiteral',
      },
      {
        category: 'Text',
        modifier: '',
        delimiterWidth: 1,
        escapePolicy: 'Escaped',
        tokenKind: 'TextLiteral',
      },
    ],
  )
  assert.strictEqual(LiteralForm.recognize(bytes('"""body"""'))?.delimiterWidth, 3)
  assert.strictEqual(LiteralForm.recognize(bytes('b"""body"""'))?.delimiterWidth, 3)
  assert.strictEqual(LiteralForm.recognize(bytes('r"""body"""'))?.delimiterWidth, 3)
})

it('scans a raw boundary without consulting a backslash', () => {
  // `r"path\"` closes at its own quote; the escaped form would swallow it and run on.
  assert.deepEqual(LiteralForm.scanBoundary(bytes('r"path\\" tail'), 2, 1, 'Raw'), {
    end: 8,
    terminated: true,
  })
  assert.deepEqual(LiteralForm.scanBoundary(bytes('"path\\" tail'), 1, 1, 'Escaped'), {
    end: 12,
    terminated: false,
  })
})

it('reserves identifier-like unknown modifiers without accepting them as forms', () => {
  assert.deepEqual(LiteralForm.recognizeUnknown(bytes('future"value"')), {
    modifier: 'future',
    modifierWidth: 6,
    delimiterWidth: 1,
  })
  assert.deepEqual(LiteralForm.recognizeUnknown(bytes('br"""value"""')), {
    modifier: 'br',
    modifierWidth: 2,
    delimiterWidth: 3,
  })
  assert.isUndefined(LiteralForm.recognizeUnknown(bytes('b"value"')))
  assert.isUndefined(LiteralForm.recognizeUnknown(bytes('ordinary')))
})
