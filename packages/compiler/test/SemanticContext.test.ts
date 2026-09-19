import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as AuthoredIdentity from '../src/AuthoredIdentity.js'
import * as AuthoredLowering from '../src/AuthoredLowering.js'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as SemanticContext from '../src/SemanticContext.js'
import * as SourceFile from '../src/SourceFile.js'
import { unreachable } from './support/raise.js'

const encoder = new TextEncoder()

const source = `fn answer(seed: i32) -> i32 {
  let value = seed + 1
  return value
}
`

it.effect('resolves spans, text and document order through the current presentation', () =>
  Effect.gen(function* () {
    const lowered = yield* AuthoredLowering.lower(
      Parser.parse(Lexer.lex(SourceFile.make('app/Main', encoder.encode(source)))),
      AuthoredIdentity.module('fixture', 'app/Main'),
    )
    const context = SemanticContext.make(lowered)
    assert.strictEqual(SemanticContext.make(lowered), context)
    const declaration = lowered.module.declarations[0] ?? unreachable('expected a declaration')
    const header = context.spanOf(declaration.header.anchor)
    assert.strictEqual(header.sourceId, 'app/Main')
    assert.strictEqual(source.slice(header.start, header.end).startsWith('fn answer'), true)
    if (declaration.header._tag !== 'FunctionHeader')
      return yield* Effect.die('expected a function')
    assert.strictEqual(SemanticContext.nameText(context, declaration.header.name), 'answer')
    // Body nodes are ordered after the header, and missing anchors fall back to their nearest
    // presented ancestor instead of failing.
    if (declaration.body._tag !== 'CallableBody' || declaration.body.block === undefined)
      return yield* Effect.die('expected a callable body')
    const block = declaration.body.block
    assert.isAbove(context.orderOf(block.anchor), context.orderOf(declaration.header.anchor))
    const phantom: AuthoredIdentity.Anchor = {
      ...block.anchor,
      path: [...block.anchor.path, { _tag: 'LocalSegment', role: 'nowhere', occurrence: 3 }],
    }
    assert.deepEqual(context.spanOf(phantom), context.spanOf(block.anchor))
    assert.strictEqual(context.orderOf(phantom), context.orderOf(block.anchor))
    const foreignOwner: AuthoredIdentity.Anchor = {
      _tag: 'AuthoredAnchor',
      owner: {
        ...declaration.owner,
        path: [
          ...declaration.owner.path,
          { _tag: 'OwnerSegment', kind: 'callable', role: 'anonymous', occurrence: 7 },
        ],
      },
      path: [],
    }
    assert.deepEqual(context.spanOf(foreignOwner), header)
    assert.strictEqual(
      context.spanOf({ ...phantom, owner: AuthoredIdentity.module('x', 'y') }).end,
      0,
    )
  }),
)
