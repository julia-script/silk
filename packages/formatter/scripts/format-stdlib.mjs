import { readdirSync, readFileSync, writeFileSync } from 'node:fs'
import { resolve } from 'node:path'
import * as DocBlock from '@silk-effect/compiler/DocBlock'
import * as FormattedDocument from '@silk-effect/compiler/FormattedDocument'
import * as Lexer from '@silk-effect/compiler/Lexer'
import * as Parser from '@silk-effect/compiler/Parser'
import * as SourceFile from '@silk-effect/compiler/SourceFile'
import * as CodeFence from '@silk-effect/documentation/CodeFence'
import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import * as Formatter from '../dist/Formatter.js'

const sourceRoot = resolve(import.meta.dirname, '../../compiler/stdlib/silk')
const check = process.argv.includes('--check')

const sources = (directory) =>
  readdirSync(directory, { withFileTypes: true })
    .flatMap((entry) => {
      const path = resolve(directory, entry.name)
      return entry.isDirectory() ? sources(path) : path.endsWith('.silk') ? [path] : []
    })
    .sort()

const drift = []
const failures = []

const inventory = (syntax) =>
  DocBlock.all(syntax).map((block) => ({
    block,
    fences: Effect.runSync(CodeFence.all(syntax.source, block)),
  }))

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

for (const path of sources(sourceRoot)) {
  const bytes = readFileSync(path)
  const syntax = Parser.parse(Lexer.lex(SourceFile.make(path, bytes)))
  const attempted = Effect.runSync(Effect.result(Formatter.format(syntax)))
  if (Result.isFailure(attempted)) {
    failures.push(
      `${path}: ${attempted.failure.message}\n${JSON.stringify(attempted.failure.reason, null, 2)}`,
    )
    continue
  }
  const canonicalSource = SourceFile.make(
    path,
    FormattedDocument.toUint8Array(attempted.success),
    syntax.source.origin,
  )
  const canonicalSyntax = Parser.parse(Lexer.lex(canonicalSource))
  const originalBlocks = inventory(syntax)
  const canonicalBlocks = inventory(canonicalSyntax)
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
      bytes: Effect.runSync(CodeFence.rewrite(syntax.source, original.block, replacements)),
    })
  }
  if (edits.length === 0) continue
  const embeddedCanonical = applyEdits(syntax.source.bytes, edits)
  drift.push(path)
  if (!check) writeFileSync(path, embeddedCanonical)
}

for (const failure of failures) console.error(failure)
if (check) for (const path of drift) console.error(`${path}: needs formatting`)
if (failures.length > 0 || (check && drift.length > 0)) process.exitCode = 1
else if (!check && drift.length > 0)
  console.log(`Formatted ${drift.length} shipped Silk source files.`)
