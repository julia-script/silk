import { NodeRuntime, NodeServices } from '@effect/platform-node'
import * as DocBlock from '@silklang/compiler/DocBlock'
import * as FormattedDocument from '@silklang/compiler/FormattedDocument'
import * as Lexer from '@silklang/compiler/Lexer'
import * as Parser from '@silklang/compiler/Parser'
import * as SourceFile from '@silklang/compiler/SourceFile'
import * as CodeFence from '@silklang/docgen/CodeFence'
import * as Formatter from '@silklang/formatter/Formatter'
import * as Console from 'effect/Console'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Path from 'effect/Path'
import * as Result from 'effect/Result'

const check = process.argv.includes('--check')

const sourceFiles = Effect.fnUntraced(function* (/** @type {string} */ directory) {
  const fileSystem = yield* FileSystem.FileSystem
  const path = yield* Path.Path
  return (yield* fileSystem.readDirectory(directory, { recursive: true }))
    .filter((entry) => entry.endsWith('.silk'))
    .map((entry) => path.join(directory, entry))
    .sort()
})

const inventory = Effect.fnUntraced(
  function* (/** @type {import('@silklang/compiler/SyntaxFile').SyntaxFile} */ syntax) {
    const result = []
    for (const block of DocBlock.all(syntax)) {
      const fences = yield* CodeFence.all(syntax.source, block)
      result.push({ block, fences })
    }
    return result
  },
)

const join = (parts) => {
  const size = parts.reduce((total, part) => total + part.length, 0)
  const output = new Uint8Array(size)
  let offset = 0
  for (const part of parts) {
    output.set(part, offset)
    offset += part.length
  }
  return output
}

const applyEdits = (bytes, edits) => {
  let output = Uint8Array.from(bytes)
  for (const edit of edits.sort((left, right) => right.start - left.start))
    output = join([output.slice(0, edit.start), edit.bytes, output.slice(edit.end)])
  return output
}

const formatEmbeddedDocumentation = Effect.fnUntraced(function* (/** @type {string} */ file) {
  const fileSystem = yield* FileSystem.FileSystem
  const bytes = yield* fileSystem.readFile(file)
  const syntax = Parser.parse(Lexer.lex(SourceFile.make(file, bytes)))
  const attempted = yield* Effect.result(Formatter.format(syntax))
  if (Result.isFailure(attempted))
    return {
      _tag: 'Failure',
      message: `${file}: ${attempted.failure.message}\n${JSON.stringify(attempted.failure.reason, null, 2)}`,
    }

  const canonicalSource = SourceFile.make(
    file,
    FormattedDocument.toUint8Array(attempted.success),
    syntax.source.origin,
  )
  const canonicalSyntax = Parser.parse(Lexer.lex(canonicalSource))
  // The shipped stdlib owns its outer layout separately. Project only the public
  // formatter's embedded-documentation edits back onto the authored source.
  const originalBlocks = yield* inventory(syntax)
  const canonicalBlocks = yield* inventory(canonicalSyntax)
  const edits = []
  for (const [blockOrdinal, original] of originalBlocks.entries()) {
    const canonical = canonicalBlocks.at(blockOrdinal)
    if (canonical === undefined) continue
    const replacements = original.fences.flatMap((fence) => {
      const destination = canonical.fences.at(fence.ordinal)
      return CodeFence.isActive(fence) &&
        destination !== undefined &&
        fence.body !== destination.body
        ? [{ fence, body: destination.body }]
        : []
    })
    if (replacements.length === 0) continue
    edits.push({
      start: original.block.span.start,
      end: original.block.span.end,
      bytes: yield* CodeFence.rewrite(syntax.source, original.block, replacements),
    })
  }
  return edits.length === 0
    ? { _tag: 'Unchanged' }
    : { _tag: 'Changed', bytes: applyEdits(syntax.source.bytes, edits) }
})

const main = Effect.fn('FormatStdlibDocumentation.main')(function* () {
  const fileSystem = yield* FileSystem.FileSystem
  const path = yield* Path.Path
  const sourceRoot = path.resolve(import.meta.dirname, '../../compiler/stdlib/silk')
  const drift = []
  const failures = []

  for (const file of yield* sourceFiles(sourceRoot)) {
    const outcome = yield* formatEmbeddedDocumentation(file)
    if (outcome._tag === 'Failure') failures.push(outcome.message)
    else if (outcome._tag === 'Changed') {
      drift.push(file)
      if (!check) yield* fileSystem.writeFile(file, outcome.bytes)
    }
  }

  if (failures.length > 0) return yield* Effect.fail(failures.join('\n'))
  if (check && drift.length > 0)
    return yield* Effect.fail(
      drift.map((file) => `${file}: embedded docs need formatting`).join('\n'),
    )
  if (!check && drift.length > 0)
    yield* Console.log(`Formatted embedded documentation in ${drift.length} shipped Silk files.`)
})

main().pipe(
  Effect.tapError((error) => Console.error(String(error))),
  Effect.provide(NodeServices.layer),
  NodeRuntime.runMain({ disableErrorReporting: true }),
)
