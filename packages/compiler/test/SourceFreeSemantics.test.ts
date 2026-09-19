import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as AuthoredIdentity from '../src/AuthoredIdentity.js'
import * as AuthoredLowering from '../src/AuthoredLowering.js'
import * as Diagnostic from '../src/Diagnostic.js'
import * as Elaboration from '../src/Elaboration.js'
import * as Lexer from '../src/Lexer.js'
import * as ModuleClosure from '../src/ModuleClosure.js'
import * as NameResolution from '../src/NameResolution.js'
import * as Ownership from '../src/Ownership.js'
import * as Parser from '../src/Parser.js'
import * as SemanticContext from '../src/SemanticContext.js'
import * as SourceFile from '../src/SourceFile.js'
import type * as SyntaxFile from '../src/SyntaxFile.js'
import * as Tir from '../src/Tir.js'

const name = 'source-free/main'

/** Stands where syntax or source used to be: any read is a semantic phase reaching back for it. */
const released = <A extends object>(what: string): A =>
  new Proxy({} as A, {
    get: (_target, key) => {
      throw new Error(`semantic analysis read ${what}.${String(key)} after it was released`)
    },
  })

const analyze = (text: string) =>
  Effect.gen(function* () {
    const source = SourceFile.make(name, new TextEncoder().encode(text))
    const authored = yield* AuthoredLowering.lower(
      Parser.parse(Lexer.lex(source)),
      AuthoredIdentity.module('memory', name),
    )
    // From here on only the authored module and its presentation exist.
    const closure: ModuleClosure.Closure = Object.freeze({
      _tag: 'ModuleClosure',
      rootModule: name,
      modules: Object.freeze([
        Object.freeze({
          _tag: 'Module' as const,
          name,
          syntax: released<SyntaxFile.SyntaxFile>('syntax'),
          authored,
          declarations: ModuleClosure.selectedDeclarations(authored.module, new Map()),
          imports: Object.freeze([]),
        }),
      ]),
      cycles: Object.freeze([]),
      diagnostics: Object.freeze([]),
      sources: new Map([[name, released<SourceFile.SourceFile>('source')]]),
      missingRoots: Object.freeze([]),
      resolutionFailures: Object.freeze([]),
    })
    const analyzed = NameResolution.analyze(closure)
    const headers = analyzed.index.modules.at(0)
    const scope = NameResolution.scopeOf(analyzed.resolution, name)
    if (headers === undefined || scope === undefined)
      throw new RangeError('fixture lost its module')
    const result = Elaboration.elaborateModule({ authored, headers, scope, index: analyzed.index })
    const ownership = Ownership.checkModule(
      result,
      analyzed.index,
      Ownership.localSharedAccessBoundaryPlan(new Map([[name, result]])),
    )
    const registry = SemanticContext.registryOf(SemanticContext.make(authored))
    return { result, ownership, published: Diagnostic.publishAll(result.diagnostics, registry) }
  })

const program = (body: string) => `struct Counter { value: i32 }
static fn limit() -> i32 { return 40 }
fn bump(counter: &mut Counter, by: i32) -> i32 {
  let add = fn(extra: i32) -> i32 { return counter.value + extra }
  return add(by)
}
pub fn main() -> i32 {
  let mut counter = Counter { value: limit() }
  ${body}
}`

it.effect(
  'elaborates, evaluates statics and checks ownership with syntax and source released',
  () =>
    Effect.gen(function* () {
      const { result, ownership } = yield* analyze(program('return bump(&mut counter, 2)'))
      assert.deepEqual(result.diagnostics, [])
      assert.deepEqual(ownership.diagnostics, [])
      // The hidden anonymous body is published beside the named ones.
      assert.isAbove(result.hiddenFunctions.length, 0)
      assert.include(Tir.encode(result.tir), 'fn source-free/main.main')
    }),
)

it.effect('names diagnostics through the current presentation when a declaration moves', () =>
  Effect.gen(function* () {
    const text = program('return missing(&mut counter)')
    const moved = `// a comment that moves every declaration\n\n${text}`
    const first = yield* analyze(text)
    const second = yield* analyze(moved)
    // What elaboration produced names no revision, so moving the declaration changes none of it.
    assert.deepEqual(second.result.diagnostics, first.result.diagnostics)
    const before = first.published
    const after = second.published
    assert.deepEqual(
      before.map((diagnostic) => diagnostic.code),
      ['SEM0004'],
    )
    const shift = moved.length - text.length
    assert.deepEqual(
      after.map((diagnostic) => [diagnostic.span.start, diagnostic.span.end]),
      before.map((diagnostic) => [diagnostic.span.start + shift, diagnostic.span.end + shift]),
    )
    assert.strictEqual(
      moved.slice(after[0]?.span.start, after[0]?.span.end),
      text.slice(before[0]?.span.start, before[0]?.span.end),
    )
  }),
)
