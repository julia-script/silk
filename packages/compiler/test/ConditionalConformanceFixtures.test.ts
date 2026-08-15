import { NodeServices } from '@effect/platform-node'
import { assert, layer } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Option from 'effect/Option'
import * as Path from 'effect/Path'
import * as Analysis from '../src/Analysis.js'
import * as DeclarationIndex from '../src/DeclarationIndex.js'
import * as FormattedDocument from '../src/FormattedDocument.js'
import * as Formatter from '../src/Formatter.js'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SyntaxCorrespondence from '../src/SyntaxCorrespondence.js'
import * as SyntaxTree from '../src/SyntaxTree.js'
import * as Type from '../src/Type.js'

const decoder = new TextDecoder()
const fixture = Effect.fnUntraced(function* (name: string) {
  const fileSystem = yield* FileSystem.FileSystem
  const path = yield* Path.Path
  const fixturePath = yield* path.fromFileUrl(
    new URL(`./fixtures/conditional-conformance/${name}.silk`, import.meta.url),
  )
  return yield* fileSystem.readFile(fixturePath)
})
const parse = Effect.fnUntraced(function* (name: string) {
  const source = yield* fixture(name)
  return Parser.parse(Lexer.lex(SourceFile.make(`conditional-conformance/${name}`, source)))
})
const parseAs = Effect.fnUntraced(function* (id: string, name: string) {
  const source = yield* fixture(name)
  return Parser.parse(Lexer.lex(SourceFile.make(id, source)))
})
const analyze = Effect.fnUntraced(function* (name: string) {
  const source = yield* fixture(name)
  return yield* Analysis.ofSourceRealized(
    `conditional-conformance/${name}`,
    source,
    'wasm32-unknown-unknown',
  )
})

layer(NodeServices.layer)('conditional conformance fixtures', (it) => {
  it.effect('formats a bounded impl canonically and idempotently', () =>
    Effect.gen(function* () {
      const original = yield* parse('syntax-original')
      assert.deepEqual(original.parserDiagnostics, [])
      const first = yield* Formatter.format(original)
      const text = decoder.decode(FormattedDocument.toUint8Array(first))
      assert.strictEqual(
        text,
        `interface Decoder<T> {
  fn decode(value: &T) -> i32
}

struct Wrapper<S> {
  source: S
}

fn wrapperDecode<S: Decoder>(value: &Wrapper<S>) -> i32 {
  return Decoder.decode(&value.source)
}

impl<S: Decoder<S>> Decoder<Wrapper<S>> for Wrapper<S> {
  decode: Wrapper.wrapperDecode
}
`,
      )
      const second = yield* Formatter.format(
        Parser.parse(
          Lexer.lex(
            SourceFile.make('conditional-conformance/formatted', Uint8Array.from(first.bytes)),
          ),
        ),
      )
      assert.deepEqual(second.bytes, first.bytes)
      assert.strictEqual(second.changed, false)
    }),
  )

  it.effect(
    'keeps an unchanged bounded impl corresponding after an earlier declaration is inserted',
    () =>
      Effect.gen(function* () {
        const previous = yield* parseAs('conditional-conformance/syntax', 'syntax-original')
        const current = yield* parseAs('conditional-conformance/syntax', 'syntax-shifted')
        const previousImpl = SyntaxTree.directNode(previous.root, 'ImplDeclaration')
        const currentImpl = SyntaxTree.directNode(current.root, 'ImplDeclaration')
        assert.isDefined(previousImpl)
        assert.isDefined(currentImpl)
        const correspondence = Option.getOrThrow(SyntaxCorrespondence.between(previous, current))
        assert.strictEqual(
          Option.getOrThrow(
            SyntaxCorrespondence.currentOf(
              correspondence,
              previousImpl ?? assert.fail('expected previous impl'),
            ),
          ),
          currentImpl,
        )
      }),
  )

  it.effect('accepts mapped and recursively nested optional schema fixtures', () =>
    Effect.gen(function* () {
      for (const name of ['accepted-mapped', 'accepted-optional']) {
        const snapshot = yield* analyze(name)
        assert.deepEqual(
          Analysis.diagnostics(snapshot).map(
            (diagnostic) => `${diagnostic.code}: ${diagnostic.message}`,
          ),
          [],
          name,
        )
      }
    }),
  )

  it.effect('rejects equal and growing requirement providers at declaration time', () =>
    Effect.gen(function* () {
      for (const name of ['rejected-equal-provider', 'rejected-growing-provider']) {
        const snapshot = yield* analyze(name)
        const diagnostics = Analysis.diagnostics(snapshot).filter(
          (diagnostic) => diagnostic.code === 'SEM0120',
        )
        assert.strictEqual(diagnostics.length, 1, name)
        assert.include(diagnostics.at(0)?.message ?? '', 'does not descend')
      }
    }),
  )

  it.effect('rejects an alpha-renamed duplicate conditional head', () =>
    Effect.gen(function* () {
      const snapshot = yield* analyze('duplicate')
      assert.strictEqual(
        Analysis.diagnostics(snapshot).filter((diagnostic) => diagnostic.code === 'SEM0119').length,
        1,
      )
    }),
  )

  it.effect('retains a damaged requirement as unavailable and never admits it as a proof', () =>
    Effect.gen(function* () {
      const syntax = yield* parse('damaged')
      assert.isAbove(syntax.parserDiagnostics.length, 0)
      const snapshot = yield* analyze('damaged')
      const conformance = Analysis.declarationIndex(snapshot).modules.at(0)?.conformances.at(0)
      assert.strictEqual(conformance?.termination._tag, 'UnavailableTermination')
      const provider = Type.nominal('conditional-conformance/damaged', 'Wrapper', [
        Type.nominal('conditional-conformance/damaged', 'Schema'),
      ])
      const proof = DeclarationIndex.prove(
        Analysis.declarationIndex(snapshot),
        provider,
        Type.nominal('conditional-conformance/damaged', 'Decoder', [provider]),
      )
      assert.strictEqual(proof._tag, 'Unproved')
    }),
  )

  it.effect('rejects mismatched and omitted interface providers', () =>
    Effect.gen(function* () {
      const mismatched = yield* analyze('wrong-provider')
      const invalid = Analysis.diagnostics(mismatched).filter(
        (diagnostic) =>
          diagnostic.code === 'SEM0083' &&
          diagnostic.reason._tag === 'InvalidConformance' &&
          diagnostic.reason.detail.includes('must be applied to its own provider'),
      )
      assert.strictEqual(invalid.length, 1)

      const omitted = yield* analyze('missing-provider')
      assert.include(
        Analysis.diagnostics(omitted).map((diagnostic) => diagnostic.code),
        'SEM0051',
      )
    }),
  )
})
