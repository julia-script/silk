import { assert, it } from '@effect/vitest'
import * as Lexer from '@silklang/compiler/Lexer'
import * as Parser from '@silklang/compiler/Parser'
import * as SourceFile from '@silklang/compiler/SourceFile'
import * as Formatter from '@silklang/formatter/Formatter'
import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import { presets } from '../app/labs/presets'

const encoder = new TextEncoder()

it.effect('keeps every formatter-compatible Labs preset canonical', () =>
  Effect.gen(function* () {
    const drift: Array<string> = []
    const refused: Array<string> = []

    for (const preset of presets) {
      for (const [module, source] of Object.entries(preset.modules)) {
        const syntax = Parser.parse(
          Lexer.lex(SourceFile.make(`labs://${preset.label}/${module}.silk`, encoder.encode(source))),
        )
        const attempted = yield* Effect.result(Formatter.format(syntax))
        if (Result.isFailure(attempted)) {
          refused.push(`${preset.label} · ${module}`)
          continue
        }
        if (attempted.success.changed) drift.push(`${preset.label} · ${module}`)
      }
    }

    assert.deepStrictEqual(drift, [])
    assert.deepStrictEqual(
      refused,
      refused.filter((entry) => entry.startsWith('fail · ')),
    )
  }),
)
