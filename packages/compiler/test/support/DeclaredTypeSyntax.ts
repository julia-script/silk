import * as Effect from 'effect/Effect'
import type * as DeclarationIndex from '../../src/DeclarationIndex.js'
import * as FormattedDocument from '../../src/FormattedDocument.js'
import * as Formatter from '../../src/Formatter.js'
import * as Lexer from '../../src/Lexer.js'
import * as ModuleClosure from '../../src/ModuleClosure.js'
import * as NameResolution from '../../src/NameResolution.js'
import * as Parser from '../../src/Parser.js'
import * as SourceFile from '../../src/SourceFile.js'
import * as SourceResolver from '../../src/SourceResolver.js'
import * as SyntaxTree from '../../src/SyntaxTree.js'
import { raise } from './raise.js'

const encoder = new TextEncoder()
const decoder = new TextDecoder()

/** Parses one declared-type syntax fixture. */
export const parse = (id: string, source: string) =>
  Parser.parse(Lexer.lex(SourceFile.make(id, encoder.encode(source))))

/** Returns every descendant element in source preorder. */
export const descendants = (node: SyntaxTree.Node): ReadonlyArray<SyntaxTree.Element> =>
  node.children.flatMap(
    (child): ReadonlyArray<SyntaxTree.Element> =>
      SyntaxTree.isNode(child) ? [child, ...descendants(child)] : [child],
  )

/** Completes the declaration index for one in-memory module. */
export const index = Effect.fnUntraced(function* (id: string, source: string) {
  const closure = yield* ModuleClosure.load({
    root: SourceFile.make(id, encoder.encode(source)),
  }).pipe(Effect.provide(SourceResolver.memory(new Map())))
  return NameResolution.analyze(closure).index
})

/** Completes the declaration index for one root and its in-memory imports. */
export const indexWithImports = Effect.fnUntraced(function* (
  root: string,
  sources: Readonly<Record<string, string>>,
) {
  const rootSource = sources[root] ?? raise(`missing root source ${root}`)
  const imports = new Map(
    Object.entries(sources).flatMap(([module, source]) =>
      module === root ? [] : [[module, encoder.encode(source)] as const],
    ),
  )
  const closure = yield* ModuleClosure.load({
    root: SourceFile.make(root, encoder.encode(rootSource)),
  }).pipe(Effect.provide(SourceResolver.memory(imports)))
  return NameResolution.analyze(closure).index
})

/** Finds one named ordinary or Effect declaration in a completed index. */
export const declaration = (self: DeclarationIndex.Index, module: string, name: string) =>
  self.modules
    .find((candidate) => candidate.module === module)
    ?.declarations.find(
      (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === name,
    )

/** Returns stable diagnostic codes in emission order. */
export const codes = (self: DeclarationIndex.Index): ReadonlyArray<string> =>
  self.diagnostics.map((diagnostic) => diagnostic.code)

/** Formats one declared-type fixture to canonical UTF-8 text. */
export const format = Effect.fnUntraced(function* (id: string, source: string) {
  const document = yield* Formatter.format(parse(id, source))
  return decoder.decode(FormattedDocument.toUint8Array(document))
})
