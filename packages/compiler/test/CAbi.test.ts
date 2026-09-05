import { assert, it } from '@effect/vitest'
import * as AbiManifest from '../src/AbiManifest.js'
import type * as Backend from '../src/Backend.js'
import * as CAbi from '../src/CAbi.js'
import * as CHeader from '../src/CHeader.js'
import * as CLayout from '../src/CLayout.js'
import * as ForeignSymbol from '../src/ForeignSymbol.js'
import * as Lifetime from '../src/Lifetime.js'
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
  ['string', Type.string(Lifetime.staticLifetime)],
  ['reference', Type.reference('Shared', 'i32', Lifetime.staticLifetime)],
  ['slice', Type.slice('Shared', 'u8', Lifetime.staticLifetime)],
  ['struct', Type.nominal('app/main', 'Point')],
  ['type parameter', Type.parameter({ module: 'app/main', name: 'f' }, 0, 'T')],
]

it('admits exactly the non-nominal C-layout field vocabulary', () => {
  const resolveNothing: CLayout.ResolveStruct = () => undefined
  const acceptedFields: ReadonlyArray<Type.Type> = [
    ...admitted.map(([type]) => type),
    Type.pointer(false, Type.string(Lifetime.staticLifetime)),
    Type.pointer(true, Type.nominal('app/main', 'Opaque')),
    Type.fixedArray('u16', 3),
    Type.fixedArray(Type.fixedArray('f64', 2), 4),
  ]
  const rejectedFields: ReadonlyArray<readonly [Type.Type, CLayout.RejectionReason]> = [
    ['bool', 'UnsupportedType'],
    ['char', 'UnsupportedType'],
    [Type.string(Lifetime.staticLifetime), 'UnsupportedType'],
    [Type.unit, 'UnknownRecord'],
    [Type.reference('Shared', 'i32', Lifetime.staticLifetime), 'UnsupportedType'],
    [Type.slice('Shared', 'u8', Lifetime.staticLifetime), 'UnsupportedType'],
    [Type.fixedArray('u8', 0), 'ZeroLengthArray'],
    [Type.fixedArray('bool', 2), 'UnsupportedType'],
  ]

  for (const type of acceptedFields) {
    assert.strictEqual(CLayout.admit(type, resolveNothing)._tag, 'Admitted', Type.encode(type))
  }
  for (const [type, reason] of rejectedFields) {
    const admission = CLayout.admit(type, resolveNothing)
    assert.strictEqual(admission._tag, 'NotAdmitted', Type.encode(type))
    if (admission._tag === 'NotAdmitted') assert.strictEqual(admission.reason, reason)
  }
})

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

const pointers: ReadonlyArray<readonly [string, Type.Pointer]> = [
  ['*const u8', Type.pointer(false, 'u8')],
  ['*mut u8', Type.pointer(true, 'u8')],
  ['*mut Opaque', Type.pointer(true, Type.nominal('app/main', 'Opaque'))],
  ['*const Vector<i32>', Type.pointer(false, Type.nominal('silk/vector', 'Vector', ['i32']))],
]

it('admits a pointer of any pointee in both positions without examining the pointee', () => {
  for (const [spelling, type] of pointers) {
    assert.strictEqual(CAbi.admit(type, 'Parameter')._tag, 'Admitted', spelling)
    assert.strictEqual(CAbi.admit(type, 'Result')._tag, 'Admitted', spelling)
    assert.deepEqual(
      CAbi.classify(type, Target.aarch64AppleDarwin, 'Result'),
      { _tag: 'Pointer', mutable: type.mutable, pointee: type.pointee },
      spelling,
    )
    assert.strictEqual(
      CAbi.typeText(CAbi.classify(type, Target.wasm32UnknownUnknown, 'Parameter')),
      type.mutable ? '*mut' : '*const',
      spelling,
    )
  }
  // The pointee stays unexamined: a pointer to a rejected pointee is still admitted.
  for (const [label, type] of rejected) {
    assert.strictEqual(CAbi.admit(Type.pointer(false, type), 'Parameter')._tag, 'Admitted', label)
  }
})

it('keys pointer mutability so `*const u8` and `*mut u8` name different signatures', () => {
  const constKey = CAbi.signatureKey(
    CAbi.signature([Type.pointer(false, 'u8'), 'usize'], 'i32', Target.aarch64AppleDarwin),
  )
  const mutKey = CAbi.signatureKey(
    CAbi.signature([Type.pointer(true, 'u8'), 'usize'], 'i32', Target.aarch64AppleDarwin),
  )
  assert.strictEqual(constKey, '(*const,u64)->i32')
  assert.strictEqual(mutKey, '(*mut,u64)->i32')
  assert.notStrictEqual(constKey, mutKey)
  assert.strictEqual(
    CAbi.signatureKey(
      CAbi.signature(['usize'], Type.pointer(true, 'u8'), Target.x8664UnknownLinuxGnu),
    ),
    '(u64)->*mut',
  )
  // Pointee spelling does not reach the key: two opaque pointees classify identically.
  assert.strictEqual(
    CAbi.signatureKey(
      CAbi.signature(
        [Type.pointer(true, Type.nominal('app/main', 'Opaque'))],
        Type.unit,
        Target.aarch64AppleDarwin,
      ),
    ),
    '(*mut)->void',
  )
})

it('admits recursively C-compatible function pointers and keys their full signature', () => {
  const compare = Type.foreignFunction(
    [Type.pointer(false, 'i32'), Type.pointer(false, 'i32')],
    'i32',
  )
  assert.strictEqual(CAbi.admit(compare, 'Parameter')._tag, 'Admitted')
  assert.strictEqual(
    CAbi.typeText(CAbi.classify(compare, Target.aarch64AppleDarwin, 'Parameter')),
    'extern "C" fn(*const,*const)->i32',
  )
  assert.strictEqual(
    CAbi.signatureKey(CAbi.signature([compare, 'usize'], Type.unit, Target.aarch64AppleDarwin)),
    '(extern "C" fn(*const,*const)->i32,u64)->void',
  )

  const invalidParameter = Type.foreignFunction([Type.unit], 'i32')
  const invalidResult = Type.foreignFunction(['i32'], 'bool')
  assert.strictEqual(CAbi.admit(invalidParameter, 'Parameter')._tag, 'NotAdmitted')
  assert.strictEqual(CAbi.admit(invalidResult, 'Parameter')._tag, 'NotAdmitted')
})

it('renders one exact canonical header for scalars, pointers, nested callbacks, and empty arity', () => {
  const functions: ReadonlyArray<Backend.ForeignExport> = [
    {
      symbol: 'visit',
      parameters: ['*const', '*mut', 'extern "C" fn(i32,*const)->u8'],
      result: 'extern "C" fn(u64)->i32',
    },
    { symbol: 'answer', parameters: [], result: 'i32' },
  ]
  const data: ReadonlyArray<Backend.ForeignStatic> = [
    { symbol: 'callback_slot', type: 'extern "C" fn(i16)->u16', direction: 'Export' },
    { symbol: 'mutable_state', type: '*mut', direction: 'Export' },
    { symbol: 'silk_abi_version', type: 'u32', direction: 'Export' },
    { symbol: 'host_state', type: '*mut', direction: 'Import' },
  ]
  const header = CHeader.make('answer-kit', functions, data)
  const expected = [
    '#ifndef SILK_ANSWER_KIT_H',
    '#define SILK_ANSWER_KIT_H',
    '',
    '#include <stdint.h>',
    '',
    '#ifdef __cplusplus',
    'extern "C" {',
    '#endif',
    '',
    'int32_t answer(void);',
    'extern uint16_t (*const callback_slot)(int16_t arg0);',
    'extern void *const mutable_state;',
    'extern const uint32_t silk_abi_version;',
    'int32_t (*visit(const void *arg0, void *arg1, uint8_t (*arg2)(int32_t arg0, const void *arg1)))(uint64_t arg0);',
    '',
    '#ifdef __cplusplus',
    '}',
    '#endif',
    '',
    '#endif /* SILK_ANSWER_KIT_H */',
    '',
  ].join('\n')
  assert.strictEqual(CHeader.render(header), expected)
  assert.deepStrictEqual(CHeader.encode(header), new TextEncoder().encode(expected))
})

it('encodes exact target-qualified ABI manifests for Darwin and Linux', () => {
  const exports: ReadonlyArray<Backend.ForeignExport> = [
    { symbol: 'answer', parameters: [], result: 'i32' },
  ]
  const data: ReadonlyArray<Backend.ForeignStatic> = [
    { symbol: 'silk_abi_version', type: 'u32', direction: 'Export' },
    { symbol: 'host_state', type: '*mut', direction: 'Import' },
  ]
  for (const target of [Target.aarch64AppleDarwin, Target.x8664UnknownLinuxGnu]) {
    const pointerWidth = CAbi.typeText(CAbi.classify('usize', target, 'Parameter'))
    const imports: ReadonlyArray<Backend.ForeignImport> = [
      { symbol: 'host_log', parameters: ['*const', pointerWidth], result: 'void' },
    ]
    const manifest = AbiManifest.make(target, imports, exports, data)
    const expected = `${JSON.stringify(
      {
        silkForeignAbi: 1,
        target: target.id,
        exports: [
          {
            kind: 'function',
            symbol: 'answer',
            abi: 'C',
            direction: 'export',
            parameters: [],
            result: 'i32',
          },
          {
            kind: 'data',
            symbol: 'silk_abi_version',
            abi: 'C',
            direction: 'export',
            type: 'u32',
          },
        ],
        imports: [
          {
            kind: 'function',
            symbol: 'host_log',
            abi: 'C',
            direction: 'import',
            parameters: ['*const', pointerWidth],
            result: 'void',
          },
          {
            kind: 'data',
            symbol: 'host_state',
            abi: 'C',
            direction: 'import',
            type: '*mut',
          },
        ],
      },
      null,
      2,
    )}\n`
    assert.strictEqual(AbiManifest.render(manifest), expected)
    assert.deepStrictEqual(AbiManifest.encode(manifest), new TextEncoder().encode(expected))
  }
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
