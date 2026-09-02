import { assert, it } from '@effect/vitest'
import * as CAbi from '../src/CAbi.js'
import * as ForeignSymbol from '../src/ForeignSymbol.js'
import * as Target from '../src/Target.js'
import * as Type from '../src/Type.js'

const admitted: ReadonlyArray<readonly [Type.Builtin, string]> = [
  ['i8', 'i8'],
  ['u8', 'u8'],
  ['i16', 'i16'],
  ['u16', 'u16'],
  ['i32', 'i32'],
  ['u32', 'u32'],
  ['i64', 'i64'],
  ['u64', 'u64'],
  ['isize', 'i64'],
  ['usize', 'u64'],
  ['f32', 'f32'],
  ['f64', 'f64'],
]

const rejected: ReadonlyArray<readonly [string, Type.Type]> = [
  ['bool', 'bool'],
  ['char', 'char'],
  ['string', Type.string],
  ['reference', Type.reference('Shared', 'i32')],
  ['slice', Type.slice('Shared', 'u8')],
  ['struct', Type.nominal('app/main', 'Point')],
  ['type parameter', Type.parameter({ module: 'app/main', name: 'f' }, 0, 'T')],
]

it('admits every V1 scalar as parameter and result and classifies it on a 64-bit target', () => {
  for (const [spelling, text] of admitted) {
    assert.strictEqual(CAbi.admit(spelling, 'Parameter')._tag, 'Admitted', spelling)
    assert.strictEqual(CAbi.admit(spelling, 'Result')._tag, 'Admitted', spelling)
    assert.strictEqual(
      CAbi.typeText(CAbi.classify(spelling, Target.aarch64AppleDarwin, 'Parameter')),
      text,
    )
  }
})

it('rejects every non-scalar spelling with the offending type', () => {
  for (const [label, type] of rejected) {
    assert.deepEqual(
      CAbi.admit(type, 'Parameter'),
      { _tag: 'NotAdmitted', type, position: 'Parameter' },
      label,
    )
    assert.strictEqual(CAbi.admit(type, 'Result')._tag, 'NotAdmitted', label)
    assert.throws(() => CAbi.classify(type, Target.aarch64AppleDarwin, 'Parameter'), RangeError)
  }
})

it('admits unit only as a result', () => {
  assert.strictEqual(CAbi.admit(Type.unit, 'Result')._tag, 'Admitted')
  assert.strictEqual(CAbi.admit(Type.unit, 'Parameter')._tag, 'NotAdmitted')
  assert.deepEqual(CAbi.classify(Type.unit, Target.aarch64AppleDarwin, 'Result'), { _tag: 'Void' })
  assert.throws(() => CAbi.classify(Type.unit, Target.aarch64AppleDarwin, 'Parameter'), RangeError)
})

it('classifies pointer-width integers by the selected target', () => {
  assert.deepEqual(CAbi.classify('usize', Target.wasm32UnknownUnknown, 'Parameter'), {
    _tag: 'Integer',
    bits: 32,
    signed: false,
  })
  assert.deepEqual(CAbi.classify('isize', Target.aarch64AppleDarwin, 'Parameter'), {
    _tag: 'Integer',
    bits: 64,
    signed: true,
  })
})

it('produces a stable canonical signature key', () => {
  const native = CAbi.signature(['i32', 'usize'], 'f64', Target.aarch64AppleDarwin)
  assert.strictEqual(CAbi.signatureKey(native), '(i32,u64)->f64')
  assert.strictEqual(
    CAbi.signatureKey(CAbi.signature(['i32', 'usize'], 'f64', Target.wasm32UnknownUnknown)),
    '(i32,u32)->f64',
  )
  assert.strictEqual(
    CAbi.signatureKey(CAbi.signature([], Type.unit, Target.x8664UnknownLinuxGnu)),
    '()->void',
  )
  assert.strictEqual(
    CAbi.signatureKey(CAbi.signature(['i32', 'u64'], 'f64', Target.x8664UnknownLinuxGnu)),
    CAbi.signatureKey(native),
  )
})

it('validates native symbol spelling', () => {
  for (const symbol of ['abs', '_start', 'silk_test_add', 'A1']) {
    assert.isTrue(ForeignSymbol.isValidSpelling(symbol), symbol)
  }
  for (const symbol of ['', '1abc', 'not a symbol', 'a-b', 'a\0b', '_abs\n', 'ünï']) {
    assert.isFalse(ForeignSymbol.isValidSpelling(symbol), JSON.stringify(symbol))
  }
})

it('reserves the entry point, runtime symbols, and generated symbol shapes', () => {
  for (const symbol of [
    'main',
    'silk_main',
    'silk_os_file_open_v1',
    'silk_coroutine_frame_push_v1',
    'silk_standard_stream_write_v1',
    'silk_host_argc_v1',
    'silk_host_argv_v1',
    'silk_suspend_child_0',
    'silk_app_main_run__1',
  ]) {
    assert.isTrue(ForeignSymbol.isReserved(symbol), symbol)
  }
  for (const symbol of ['abs', 'malloc', 'silk_test_add', 'silky_main', 'main2', 'suspend_x']) {
    assert.isFalse(ForeignSymbol.isReserved(symbol), symbol)
  }
  assert.isTrue(ForeignSymbol.reservedSymbols.every(ForeignSymbol.isReserved))
})
