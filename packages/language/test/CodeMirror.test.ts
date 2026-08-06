import { EditorState } from '@codemirror/state'
import { assert, it } from 'vitest'
import * as CodeMirror from '../src/CodeMirror.js'

const rangesFor = (doc: string, category: string) =>
  CodeMirror.highlightRanges(doc).filter((range) => range.category === category)

const spellings = (doc: string, category: string): ReadonlyArray<string> =>
  rangesFor(doc, category).map((range) => doc.slice(range.from, range.to))

it('classifies keywords, identifiers, numbers, and operators', () => {
  const doc = 'pub fn main() -> I32 { return 42 }'
  assert.deepStrictEqual(spellings(doc, 'keyword'), ['pub', 'fn', 'return'])
  assert.deepStrictEqual(spellings(doc, 'identifier'), ['main', 'I32'])
  assert.deepStrictEqual(spellings(doc, 'number'), ['42'])
  assert.include(spellings(doc, 'operator'), '->')
  assert.include(spellings(doc, 'punctuation'), '{')
})

it('distinguishes doc comments from line comments', () => {
  const doc = '// plain\n/// documented'
  assert.deepStrictEqual(spellings(doc, 'line-comment'), ['// plain'])
  assert.deepStrictEqual(spellings(doc, 'doc-comment'), ['/// documented'])
})

it('marks invalid bytes', () => {
  assert.deepStrictEqual(spellings('let @', 'invalid'), ['@'])
})

it('booleans are their own category', () => {
  assert.deepStrictEqual(spellings('let ok = true', 'boolean'), ['true'])
})

it('places highlights correctly after multi-byte characters', () => {
  const doc = '// café \u{1f600}\nfn x() {}'
  assert.deepStrictEqual(spellings(doc, 'keyword'), ['fn'])
  const [fn] = rangesFor(doc, 'keyword')
  assert.strictEqual(doc.slice(fn?.from ?? 0, fn?.to ?? 0), 'fn')
})

it('maps editor offsets back to byte offsets', () => {
  const doc = '// é\nfn'
  assert.strictEqual(CodeMirror.charOffsetToByteOffset(doc, 0), 0)
  assert.strictEqual(CodeMirror.charOffsetToByteOffset(doc, 4), 5)
  assert.strictEqual(CodeMirror.charOffsetToByteOffset(doc, doc.length), 8)
})

it('maps byte offsets to editor offsets, inverting the selection mapping', () => {
  const doc = '// é\nfn'
  assert.strictEqual(CodeMirror.byteOffsetToCharOffset(doc, 0), 0)
  assert.strictEqual(CodeMirror.byteOffsetToCharOffset(doc, 5), 4)
  assert.strictEqual(CodeMirror.byteOffsetToCharOffset(doc, 8), doc.length)
  const ascii = 'pub fn main'
  assert.strictEqual(CodeMirror.byteOffsetToCharOffset(ascii, 4), 4)
  assert.strictEqual(CodeMirror.byteOffsetToCharOffset(ascii, 99), ascii.length)
})

const stateClasses = (state: EditorState): ReadonlyArray<string> => {
  const classes: Array<string> = []
  const iterator = state.field(CodeMirror.field).iter()
  while (iterator.value !== null) {
    classes.push(String(iterator.value.spec.class))
    iterator.next()
  }
  return classes
}

it('the state field re-lexes on edit', () => {
  const state = EditorState.create({ doc: 'le', extensions: [CodeMirror.extension()] })
  assert.notInclude(stateClasses(state), 'cm-silk-keyword')
  const edited = state.update({ changes: { from: 2, insert: 't' } }).state
  assert.isTrue(stateClasses(edited).some((cls) => cls.includes('cm-silk-keyword')))
})

it('invalid tokens carry a distinct stable class', () => {
  const state = EditorState.create({ doc: 'let @', extensions: [CodeMirror.extension()] })
  assert.isTrue(stateClasses(state).some((cls) => cls.includes('cm-silk-invalid')))
})
