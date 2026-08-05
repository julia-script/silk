import { assert, it } from '@effect/vitest'
import * as Handle from '../src/internal/Handle.js'
import type * as ModuleState from '../src/internal/ModuleState.js'
import * as OwnedHandle from '../src/internal/OwnedHandle.js'
import * as Subtype from '../src/internal/Subtype.js'
import type * as Type from '../src/Type.js'
import * as ValType from '../src/ValType.js'

/**
 * A hand-built type table:
 *   0: struct base (open)
 *   1: struct extended, supertype 0
 *   2: func [] -> [i32]
 *   3: array (mut i8)
 */
const owner = OwnedHandle.makeOwner()
const handles: Array<Type.Type> = Array.from({ length: 4 }, (_, index) =>
  Handle.make<'Type'>('Type', owner, index),
)
const entry = (composite: ModuleState.CompositeType, supertype?: number): ModuleState.SubType => ({
  composite,
  supertype,
  final: supertype !== undefined,
  name: undefined,
})
const table = {
  types: [
    {
      ...entry({ _tag: 'Struct', fields: [{ storage: ValType.i32, mutable: false }] }),
      final: false,
    },
    entry(
      {
        _tag: 'Struct',
        fields: [
          { storage: ValType.i32, mutable: false },
          { storage: ValType.f64, mutable: false },
        ],
      },
      0,
    ),
    entry({ _tag: 'Func', params: [], results: [ValType.i32] }),
    entry({ _tag: 'Array', field: { storage: { _tag: 'Packed', width: 'i8' }, mutable: true } }),
  ],
  typeHandles: handles,
}
const base = handles[0] as Type.Type
const extended = handles[1] as Type.Type
const funcType = handles[2] as Type.Type
const arrayType = handles[3] as Type.Type

const isSub = (sub: ValType.ValType, sup: ValType.ValType) => Subtype.matches(table, sub, sup)

it('relates the abstract any-hierarchy edges', () => {
  assert.isTrue(isSub(ValType.eqref, ValType.anyref))
  assert.isTrue(isSub(ValType.i31ref, ValType.eqref))
  assert.isTrue(isSub(ValType.structref, ValType.eqref))
  assert.isTrue(isSub(ValType.arrayref, ValType.eqref))
  assert.isTrue(isSub(ValType.i31ref, ValType.anyref))
  assert.isFalse(isSub(ValType.anyref, ValType.eqref))
  assert.isFalse(isSub(ValType.i31ref, ValType.structref))
})

it('keeps the four hierarchies apart', () => {
  assert.isFalse(isSub(ValType.funcref, ValType.anyref))
  assert.isFalse(isSub(ValType.externref, ValType.anyref))
  assert.isFalse(isSub(ValType.exnref, ValType.anyref))
  assert.isFalse(isSub(ValType.anyref, ValType.funcref))
})

it('places bottom types below their own hierarchy only', () => {
  assert.isTrue(isSub(ValType.refNull('none'), ValType.i31ref))
  assert.isTrue(isSub(ValType.refNull('none'), ValType.refNull(base)))
  assert.isTrue(isSub(ValType.refNull('nofunc'), ValType.funcref))
  assert.isTrue(isSub(ValType.refNull('nofunc'), ValType.refNull(funcType)))
  assert.isFalse(isSub(ValType.refNull('none'), ValType.funcref))
  assert.isFalse(isSub(ValType.refNull('noextern'), ValType.anyref))
})

it('implies nullability from sub to super, never the reverse', () => {
  assert.isTrue(isSub(ValType.ref('i31'), ValType.i31ref))
  assert.isFalse(isSub(ValType.i31ref, ValType.ref('i31')))
  assert.isTrue(isSub(ValType.ref(extended), ValType.refNull(extended)))
})

it('classifies concrete types into their abstract ancestors', () => {
  assert.isTrue(isSub(ValType.refNull(base), ValType.structref))
  assert.isTrue(isSub(ValType.refNull(base), ValType.eqref))
  assert.isTrue(isSub(ValType.refNull(arrayType), ValType.arrayref))
  assert.isTrue(isSub(ValType.refNull(funcType), ValType.funcref))
  assert.isFalse(isSub(ValType.refNull(funcType), ValType.anyref))
  assert.isFalse(isSub(ValType.structref, ValType.refNull(base)))
})

it('walks declared supertype chains between concrete types', () => {
  assert.isTrue(isSub(ValType.refNull(extended), ValType.refNull(base)))
  assert.isFalse(isSub(ValType.refNull(base), ValType.refNull(extended)))
  assert.isFalse(isSub(ValType.refNull(funcType), ValType.refNull(base)))
})

it('keeps numeric and vector types nominal', () => {
  assert.isTrue(isSub(ValType.i32, ValType.i32))
  assert.isFalse(isSub(ValType.i32, ValType.i64))
  assert.isFalse(isSub(ValType.v128, ValType.i32))
  assert.isFalse(isSub(ValType.i32, ValType.anyref))
})
