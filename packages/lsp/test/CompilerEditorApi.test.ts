import { assert, it } from '@effect/vitest'
import * as Analysis from '@silk-effect/compiler/Analysis'
import type * as Completion from '@silk-effect/compiler/Completion'
import * as Intrinsic from '@silk-effect/compiler/Intrinsic'
import * as Presentation from '@silk-effect/compiler/Presentation'
import * as SemanticOccurrence from '@silk-effect/compiler/SemanticOccurrence'
import * as TypeHint from '@silk-effect/compiler/TypeHint'

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
