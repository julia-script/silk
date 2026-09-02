import { EditorState } from '@codemirror/state'
import { assert, it } from 'vitest'
import * as CodeMirror from '../src/CodeMirror.js'

const rangesFor = (doc: string, category: string) =>
  CodeMirror.highlightRanges(doc).filter((range) => range.category === category)

const spellings = (doc: string, category: string): ReadonlyArray<string> =>
  rangesFor(doc, category).map((range) => doc.slice(range.from, range.to))

it('classifies keywords, identifiers, numbers, and operators', () => {
  const doc = 'pub role Primary pub fn main() -> i32 { return 42 }'
  assert.deepStrictEqual(spellings(doc, 'keyword'), ['pub', 'role', 'pub', 'fn', 'return'])
  assert.deepStrictEqual(spellings(doc, 'identifier'), ['Primary', 'main'])
  assert.deepStrictEqual(spellings(doc, 'type'), ['i32'])
  assert.deepStrictEqual(spellings(doc, 'number'), ['42'])
  assert.include(spellings(doc, 'operator'), '->')
  assert.include(spellings(doc, 'punctuation'), '{')
})

it('classifies valid durations as numbers and malformed candidates as invalid', () => {
  const doc = 'let delay = 01h05m00s let broken = 1h60m'
  assert.deepStrictEqual(spellings(doc, 'number'), ['01h05m00s'])
  assert.deepStrictEqual(spellings(doc, 'invalid'), ['1h60m'])
})

it('distinguishes doc comments from line comments', () => {
  const doc = '// plain\n/// documented\n//! module docs'
  assert.deepStrictEqual(spellings(doc, 'line-comment'), ['// plain'])
  assert.deepStrictEqual(spellings(doc, 'doc-comment-marker'), ['///'])
  assert.deepStrictEqual(spellings(doc, 'doc-comment'), [' documented'])
  assert.deepStrictEqual(spellings(doc, 'module-doc-comment-marker'), ['//!'])
  assert.deepStrictEqual(spellings(doc, 'module-doc-comment'), [' module docs'])
})

it('highlights Markdown links and nested Silk examples inside documentation', () => {
  const doc = `/// # Recovery
/// Uses [\`Problem\`] with **care**.
/// \`\`\`silk
/// effect fn recover(problem: Problem) -> i32
/// \`\`\``
  assert.deepStrictEqual(spellings(doc, 'doc-heading-marker'), ['#'])
  assert.deepStrictEqual(spellings(doc, 'doc-link-target'), ['Problem'])
  assert.deepStrictEqual(spellings(doc, 'doc-strong-marker'), ['**', '**'])
  assert.deepStrictEqual(spellings(doc, 'doc-code-fence'), ['```', '```'])
  assert.deepStrictEqual(spellings(doc, 'doc-code-language'), ['silk'])
  assert.deepStrictEqual(spellings(doc, 'doc-code-keyword'), ['effect', 'fn'])
  assert.deepStrictEqual(spellings(doc, 'doc-code-type'), ['Problem', 'i32'])
})

it('marks invalid bytes', () => {
  assert.deepStrictEqual(spellings('let $', 'invalid'), ['$'])
})

it('classifies the bitwise operator spellings as operators', () => {
  assert.deepStrictEqual(spellings('a & b ^ c | ~d', 'operator'), ['&', '^', '|', '~'])
})

it('highlights every single-line and multiline literal from compiler token ranges', () => {
  const doc = '"one" b"two" """line é\n// still text\n""" b"""bytes\n""" tail'
  assert.deepStrictEqual(spellings(doc, 'string'), [
    '"one"',
    'b"two"',
    '"""line é\n// still text\n"""',
    'b"""bytes\n"""',
  ])
  assert.include(spellings(doc, 'identifier'), 'tail')
})

it('marks reserved and unterminated literal introductions as invalid ranges', () => {
  assert.deepStrictEqual(spellings('future"value"', 'invalid'), ['future"value"'])
  assert.deepStrictEqual(spellings('"""unterminated\nbody', 'invalid'), ['"""unterminated\nbody'])
})

it('booleans are their own category', () => {
  assert.deepStrictEqual(spellings('let ok = true', 'boolean'), ['true'])
})

it('highlights mutable loop keywords through the compiler lexer', () => {
  assert.deepStrictEqual(spellings('let mut value = 0 while true { break continue }', 'keyword'), [
    'let',
    'mut',
    'while',
    'break',
    'continue',
  ])
})

it('highlights unsafe and conformance syntax through compiler tokens', () => {
  assert.deepStrictEqual(spellings('impl Allocator for Mine { unsafe { return 1 } }', 'keyword'), [
    'impl',
    'for',
    'unsafe',
    'return',
  ])
})

it('highlights parametric conformance syntax through compiler tokens', () => {
  assert.deepStrictEqual(spellings('impl<T> Drop for Vector<T> { }', 'keyword'), ['impl', 'for'])
})

it('highlights inherent impl syntax through compiler tokens', () => {
  assert.deepStrictEqual(spellings('impl<T> Option<T> { pub fn none() -> Self { } }', 'keyword'), [
    'impl',
    'pub',
    'fn',
  ])
})

it('highlights callable mode keywords without reserving dual', () => {
  assert.deepStrictEqual(
    spellings('fn(fn(i32) -> i32, mut fn(i32) -> i32, once fn(i32) -> i32) dual', 'keyword'),
    ['fn', 'fn', 'mut', 'fn', 'once', 'fn'],
  )
})

it('highlights type unions separately from pipelines', () => {
  assert.deepStrictEqual(spellings('Token | End |> inspect', 'operator'), ['|', '|>'])
})

it('highlights the complete match surface through compiler tokens', () => {
  const doc = 'match &mut event { Token { kind, .. } if true => kind _ => 0 }'
  assert.deepStrictEqual(spellings(doc, 'keyword'), ['match', 'mut', 'if'])
  assert.deepStrictEqual(spellings(doc, 'operator'), ['&', '=>', '=>'])
  assert.include(spellings(doc, 'punctuation'), '..')
  assert.deepStrictEqual(spellings(doc, 'boolean'), ['true'])
  assert.include(spellings(doc, 'identifier'), 'Token')
  assert.include(spellings(doc, 'identifier'), '_')
})

it('highlights raw pointer qualifiers as keywords behind the star operator', () => {
  const doc = 'fn fill(buffer: *mut u8) -> *const u8 { return buffer }'
  assert.deepStrictEqual(spellings(doc, 'keyword'), ['fn', 'mut', 'const', 'return'])
  assert.deepStrictEqual(spellings(doc, 'operator'), ['*', '->', '*'])
})

it('highlights bracketed fixed-array punctuation', () => {
  assert.deepStrictEqual(spellings('[i32; 4]', 'punctuation'), ['[', ';', ']'])
})

it('distinguishes generic angles from comparison operators', () => {
  const doc =
    'fn keep<T>(value: Box<T>, a: i32, b: i32) -> Box<T> { if a < b { return keep<i32>(value) } return value }'
  assert.deepStrictEqual(spellings(doc, 'type-punctuation'), [
    '<',
    '>',
    '<',
    '>',
    '<',
    '>',
    '<',
    '>',
  ])
  assert.deepStrictEqual(
    spellings(doc, 'operator').filter((spelling) => spelling === '<'),
    ['<'],
  )
})

it('classifies never and executable scalar names as builtin types', () => {
  assert.deepStrictEqual(spellings('never | i32 | bool', 'type'), ['never', 'i32', 'bool'])
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
  const state = EditorState.create({ doc: 'let $', extensions: [CodeMirror.extension()] })
  assert.isTrue(stateClasses(state).some((cls) => cls.includes('cm-silk-invalid')))
})
