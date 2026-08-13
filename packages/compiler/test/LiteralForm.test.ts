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
      {
        category: 'Character',
        modifier: '',
        delimiterWidth: 1,
        escapePolicy: 'Escaped',
        tokenKind: 'CharLiteral',
      },
    ],
  )
  assert.strictEqual(LiteralForm.recognize(bytes('"""body"""'))?.delimiterWidth, 3)
  assert.strictEqual(LiteralForm.recognize(bytes('b"""body"""'))?.delimiterWidth, 3)
  assert.strictEqual(LiteralForm.recognize(bytes('r"""body"""'))?.delimiterWidth, 3)
})

it('recognizes the character form on its own delimiter alone', () => {
  const form = LiteralForm.recognize(bytes("'a'"))
  assert.strictEqual(form?.category, 'Character')
  assert.strictEqual(form?.delimiter, 0x27)
  assert.strictEqual(form?.tokenKind, 'CharLiteral')
  // The apostrophe is not a modifier position, so no quote form and no reserved modifier claims it.
  assert.strictEqual(LiteralForm.recognize(bytes('"a"'))?.category, 'Text')
  assert.isUndefined(LiteralForm.recognizeUnknown(bytes("b'a'")))
})

it('scans a character boundary to its own delimiter and stops at a line ending', () => {
  assert.deepEqual(LiteralForm.scanBoundary(bytes("'a' tail"), 1, 1, 'Escaped', 0x27), {
    end: 3,
    terminated: true,
  })
  // `'\''` escapes the delimiter, so the boundary is the fourth byte and not the third.
  assert.deepEqual(LiteralForm.scanBoundary(bytes("'\\''"), 1, 1, 'Escaped', 0x27), {
    end: 4,
    terminated: true,
  })
  assert.deepEqual(LiteralForm.scanBoundary(bytes("'a\nnext"), 1, 1, 'Escaped', 0x27), {
    end: 2,
    terminated: false,
  })
})

it('counts scalars rather than bytes in a character body', () => {
  const count = (value: string): number =>
    LiteralForm.scalarCount(bytes(value), 0, bytes(value).length)
  assert.strictEqual(count(''), 0)
  assert.strictEqual(count('a'), 1)
  assert.strictEqual(count('ab'), 2)
  // Two UTF-8 bytes, one scalar; four bytes, one scalar.
  assert.strictEqual(count('é'), 1)
  assert.strictEqual(count('😀'), 1)
  assert.strictEqual(count('\\n'), 1)
  assert.strictEqual(count('\\x41'), 1)
  assert.strictEqual(count('\\u{2603}'), 1)
  assert.strictEqual(count('\\u{2603}a'), 2)
  assert.strictEqual(count("\\'"), 1)
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
