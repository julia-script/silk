import { assert, it } from '@effect/vitest'
import * as Analysis from '@silklang/compiler/Analysis'
import type * as Completion from '@silklang/compiler/Completion'
import * as Intrinsic from '@silklang/compiler/Intrinsic'
import * as Presentation from '@silklang/compiler/Presentation'
import * as SemanticOccurrence from '@silklang/compiler/SemanticOccurrence'
import * as TypeHint from '@silklang/compiler/TypeHint'

const occurrenceQuery: (
  self: Analysis.FrontendSnapshot,
  module: string,
  offset: number,
) => SemanticOccurrence.SemanticOccurrence | undefined = Analysis.semanticOccurrenceAt

const completionQuery: (
  self: Analysis.FrontendSnapshot,
  module: string,
  offset: number,
) => Completion.Result | undefined = Analysis.completionAt

const hintQuery: (
  self: Analysis.FrontendSnapshot,
  module: string,
  start: number,
  end: number,
) => ReadonlyArray<TypeHint.TypeHint> = Analysis.typeHints

it('exports the immutable compiler editor API through public subpaths', () => {
  assert.strictEqual(typeof occurrenceQuery, 'function')
  assert.strictEqual(typeof completionQuery, 'function')
  assert.strictEqual(typeof hintQuery, 'function')
  assert.strictEqual(typeof Presentation.type, 'function')
  assert.strictEqual(typeof SemanticOccurrence.at, 'function')
  assert.strictEqual(typeof TypeHint.make, 'function')
  assert.isTrue(Object.isFrozen(Intrinsic.all()))
  assert.isTrue(Intrinsic.all().every((actor) => Object.isFrozen(actor.operations)))
})
