import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import type * as Alignment from './Alignment.js'
import * as Attribute from './Attribute.js'
import type * as Builder from './Builder.js'
import * as Constant from './Constant.js'
import * as FunctionActor from './Function.js'
import type * as FunctionBody from './FunctionBody.js'
import * as FunctionBodyActor from './FunctionBody.js'
import * as BuilderState from './internal/BuilderState.js'
import * as Handle from './internal/Handle.js'
import type * as TypeDescription from './internal/TypeDescription.js'
import { invalidInput, invalidState, type LlvmError, wrappedFailure } from './LlvmError.js'
import * as Type from './Type.js'
import type * as Value from './Value.js'

/**
 * The complete, deterministic intrinsic identifier inventory from the pinned Zig LLVM builder.
 *
 * @category intrinsics
 * @since 0.0.0
 */
export const inventory = Object.freeze([
  'va_start',
  'va_end',
  'va_copy',
  'returnaddress',
  'addressofreturnaddress',
  'sponentry',
  'frameaddress',
  'prefetch',
  'thread.pointer',
  'abs',
  'smax',
  'smin',
  'umax',
  'umin',
  'scmp',
  'ucmp',
  'memcpy',
  'memcpy.inline',
  'memmove',
  'memset',
  'memset.inline',
  'sqrt',
  'powi',
  'sin',
  'cos',
  'tan',
  'asin',
  'acos',
  'atan',
  'atan2',
  'sinh',
  'cosh',
  'tanh',
  'sincos',
  'sincospi',
  'modf',
  'pow',
  'exp',
  'exp2',
  'exp10',
  'ldexp',
  'frexp',
  'log',
  'log10',
  'log2',
  'fma',
  'fabs',
  'minnum',
  'maxnum',
  'minimum',
  'maximum',
  'minimumnum',
  'maximumnum',
  'copysign',
  'floor',
  'ceil',
  'trunc',
  'rint',
  'nearbyint',
  'round',
  'roundeven',
  'lround',
  'llround',
  'lrint',
  'llrint',
  'bitreverse',
  'bswap',
  'ctpop',
  'ctlz',
  'cttz',
  'fshl',
  'fshr',
  'clmul',
  'sadd.with.overflow',
  'uadd.with.overflow',
  'ssub.with.overflow',
  'usub.with.overflow',
  'smul.with.overflow',
  'umul.with.overflow',
  'sadd.sat',
  'uadd.sat',
  'ssub.sat',
  'usub.sat',
  'sshl.sat',
  'ushl.sat',
  'smul.fix',
  'umul.fix',
  'smul.fix.sat',
  'umul.fix.sat',
  'sdiv.fix',
  'udiv.fix',
  'sdiv.fix.sat',
  'udiv.fix.sat',
  'canonicalize',
  'fmuladd',
  'vector.reduce.add',
  'vector.reduce.fadd',
  'vector.reduce.mul',
  'vector.reduce.fmul',
  'vector.reduce.and',
  'vector.reduce.or',
  'vector.reduce.xor',
  'vector.reduce.smax',
  'vector.reduce.smin',
  'vector.reduce.umax',
  'vector.reduce.umin',
  'vector.reduce.fmax',
  'vector.reduce.fmin',
  'vector.reduce.fmaximum',
  'vector.reduce.fminimum',
  'vector.insert',
  'vector.extract',
  'is.fpclass',
  'var.annotation',
  'ptr.annotation',
  'annotation',
  'codeview.annotation',
  'trap',
  'debugtrap',
  'ubsantrap',
  'stackprotector',
  'stackguard',
  'objectsize',
  'expect',
  'expect.with.probability',
  'assume',
  'ssa.copy',
  'type.test',
  'type.checked.load',
  'type.checked.load.relative',
  'arithmetic.fence',
  'donothing',
  'load.relative',
  'sideeffect',
  'is.constant',
  'ptrmask',
  'threadlocal.address',
  'vscale',
  'dbg.declare',
  'dbg.value',
  'amdgcn.workitem.id.x',
  'amdgcn.workitem.id.y',
  'amdgcn.workitem.id.z',
  'amdgcn.workgroup.id.x',
  'amdgcn.workgroup.id.y',
  'amdgcn.workgroup.id.z',
  'amdgcn.dispatch.ptr',
  'nvvm.read.ptx.sreg.tid.x',
  'nvvm.read.ptx.sreg.tid.y',
  'nvvm.read.ptx.sreg.tid.z',
  'nvvm.read.ptx.sreg.ntid.x',
  'nvvm.read.ptx.sreg.ntid.y',
  'nvvm.read.ptx.sreg.ntid.z',
  'nvvm.read.ptx.sreg.ctaid.x',
  'nvvm.read.ptx.sreg.ctaid.y',
  'nvvm.read.ptx.sreg.ctaid.z',
  'wasm.memory.size',
  'wasm.memory.grow',
] as const)

/**
 * A compile-time-safe intrinsic identifier drawn from {@link inventory}.
 *
 * @category intrinsics
 * @since 0.0.0
 */
export type Id = (typeof inventory)[number]

/**
 * Describes how one intrinsic obtains its signature, overload suffixes, and attributes.
 *
 * @category intrinsics
 * @since 0.0.0
 */
export interface CatalogEntry {
  readonly id: Id
  readonly llvmName: `llvm.${Id}`
  readonly signature: 'built-in' | 'explicit'
  readonly overloads: 'none' | 'recipe' | 'caller'
  readonly attributes: 'built-in' | 'explicit'
}

const builtInIds: ReadonlySet<Id> = new Set([
  'va_start',
  'va_end',
  'va_copy',
  'memcpy',
  'memcpy.inline',
  'memmove',
  'memset',
  'memset.inline',
  'assume',
  'trap',
  'debugtrap',
  'donothing',
  'sideeffect',
])

const builtInAttributeIds: ReadonlySet<Id> = new Set([
  'memcpy',
  'memcpy.inline',
  'memmove',
  'memset',
  'memset.inline',
  'assume',
])

const recipeOverloadIds: ReadonlySet<Id> = new Set([
  'va_start',
  'va_end',
  'va_copy',
  'memcpy',
  'memcpy.inline',
  'memmove',
  'memset',
  'memset.inline',
])

/**
 * Searchable metadata for every pinned intrinsic, in {@link inventory} order.
 *
 * @category intrinsics
 * @since 0.0.0
 */
export const catalog: ReadonlyArray<CatalogEntry> = Object.freeze(
  inventory.map((id) =>
    Object.freeze({
      id,
      llvmName: `llvm.${id}`,
      signature: builtInIds.has(id) ? 'built-in' : 'explicit',
      overloads: recipeOverloadIds.has(id) ? 'recipe' : builtInIds.has(id) ? 'none' : 'caller',
      attributes: builtInAttributeIds.has(id) ? 'built-in' : 'explicit',
    }),
  ),
)

/**
 * Caller-supplied signature for an intrinsic without a built-in recipe.
 *
 * @category intrinsics
 * @since 0.0.0
 */
export interface ExplicitSignature {
  readonly returnType: Type.Type
  readonly parameters: ReadonlyArray<Type.Type>
  readonly variadic?: boolean
}

/**
 * Optional signature and canonical attribute overrides for {@link resolve}.
 *
 * @category intrinsics
 * @since 0.0.0
 */
export interface ResolveOptions {
  /**
   * Required for catalog entries whose signature cannot be inferred solely from overload types.
   * Common and memory intrinsics have built-in recipes and need no explicit signature.
   */
  readonly signature?: ExplicitSignature
  /** Canonical declaration attributes for an explicit signature recipe. */
  readonly attributes?: Attribute.FunctionSet
}

/**
 * Volatility, inlining, and parameter alignment controls for {@link memcpy} and {@link memmove}.
 *
 * @category intrinsics
 * @since 0.0.0
 */
export interface MemoryCopyOptions {
  readonly volatile?: boolean
  readonly inline?: boolean
  readonly destinationAlignment?: Alignment.Alignment
  readonly sourceAlignment?: Alignment.Alignment
}

/**
 * Volatility, inlining, and destination alignment controls for {@link memset}.
 *
 * @category intrinsics
 * @since 0.0.0
 */
export interface MemorySetOptions {
  readonly volatile?: boolean
  readonly inline?: boolean
  readonly destinationAlignment?: Alignment.Alignment
}

type OverloadedRecipe = (
  builder: Builder.Builder,
  overloads: ReadonlyArray<Type.Type>,
) => Effect.Effect<ExplicitSignature, LlvmError>

type SimpleRecipe = (builder: Builder.Builder) => Effect.Effect<ExplicitSignature, LlvmError>

/** @internal */
const memoryCopySignature = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  overloads: ReadonlyArray<Type.Type>,
): Effect.fn.Return<ExplicitSignature, LlvmError> {
  const destination = overloads[0]
  const source = overloads[1]
  const length = overloads[2]
  if (destination === undefined || source === undefined || length === undefined) {
    return yield* Effect.fail(
      invalidInput({
        operation: 'Intrinsic.resolve',
        message: 'Memory copy intrinsics require destination, source, and length overload types',
        input: overloads,
      }),
    )
  }
  return {
    returnType: yield* Type.voidType(builder),
    parameters: [destination, source, length, yield* Type.integer(builder, 1)],
  }
})

/** @internal */
const memorySetSignature = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  overloads: ReadonlyArray<Type.Type>,
): Effect.fn.Return<ExplicitSignature, LlvmError> {
  const destination = overloads[0]
  const length = overloads[1]
  if (destination === undefined || length === undefined) {
    return yield* Effect.fail(
      invalidInput({
        operation: 'Intrinsic.resolve',
        message: 'Memory set intrinsics require destination and length overload types',
        input: overloads,
      }),
    )
  }
  return {
    returnType: yield* Type.voidType(builder),
    parameters: [
      destination,
      yield* Type.integer(builder, 8),
      length,
      yield* Type.integer(builder, 1),
    ],
  }
})

const overloadedRecipes: Readonly<Partial<Record<Id, OverloadedRecipe>>> = Object.freeze({
  va_start: Effect.fnUntraced(function* (builder, overloads) {
    const parameter = overloads[0]
    if (parameter === undefined) {
      return yield* Effect.fail(
        invalidInput({
          operation: 'Intrinsic.resolve',
          message: 'llvm.va_start requires one pointer overload type',
          input: overloads,
        }),
      )
    }
    return { returnType: yield* Type.voidType(builder), parameters: [parameter] }
  }),
  va_end: Effect.fnUntraced(function* (builder, overloads) {
    const parameter = overloads[0]
    if (parameter === undefined) {
      return yield* Effect.fail(
        invalidInput({
          operation: 'Intrinsic.resolve',
          message: 'llvm.va_end requires one pointer overload type',
          input: overloads,
        }),
      )
    }
    return { returnType: yield* Type.voidType(builder), parameters: [parameter] }
  }),
  va_copy: Effect.fnUntraced(function* (builder, overloads) {
    const parameter = overloads[0]
    if (parameter === undefined) {
      return yield* Effect.fail(
        invalidInput({
          operation: 'Intrinsic.resolve',
          message: 'llvm.va_copy requires one pointer overload type',
          input: overloads,
        }),
      )
    }
    return { returnType: yield* Type.voidType(builder), parameters: [parameter, parameter] }
  }),
  memcpy: memoryCopySignature,
  'memcpy.inline': memoryCopySignature,
  memmove: memoryCopySignature,
  memset: memorySetSignature,
  'memset.inline': memorySetSignature,
})

const simpleRecipes: Readonly<Partial<Record<Id, SimpleRecipe>>> = Object.freeze({
  assume: Effect.fnUntraced(function* (builder: Builder.Builder) {
    return {
      returnType: yield* Type.voidType(builder),
      parameters: [yield* Type.integer(builder, 1)],
    }
  }),
  trap: Effect.fnUntraced(function* (builder: Builder.Builder) {
    return { returnType: yield* Type.voidType(builder), parameters: [] }
  }),
  debugtrap: Effect.fnUntraced(function* (builder: Builder.Builder) {
    return { returnType: yield* Type.voidType(builder), parameters: [] }
  }),
  donothing: Effect.fnUntraced(function* (builder: Builder.Builder) {
    return { returnType: yield* Type.voidType(builder), parameters: [] }
  }),
  sideeffect: Effect.fnUntraced(function* (builder: Builder.Builder) {
    return { returnType: yield* Type.voidType(builder), parameters: [] }
  }),
})

/** @internal */
const mangleDescription = (
  state: BuilderState.MutableState,
  description: TypeDescription.Description,
): string => {
  switch (description._tag) {
    case 'Integer':
      return `i${description.bitWidth}`
    case 'Pointer':
      return `p${description.addressSpace.value}`
    case 'Vector': {
      const child = state.types[description.child]
      if (child === undefined) throw new Error('missing intrinsic overload vector child')
      return `${description.scalable ? 'nxv' : 'v'}${description.length}${mangleDescription(state, child)}`
    }
    case 'Array': {
      const child = state.types[description.child]
      if (child === undefined) throw new Error('missing intrinsic overload array child')
      return `a${description.length}${mangleDescription(state, child)}`
    }
    case 'Simple':
      switch (description.tag) {
        case 'Half':
          return 'f16'
        case 'BFloat':
          return 'bf16'
        case 'Float':
          return 'f32'
        case 'Double':
          return 'f64'
        case 'X86Fp80':
          return 'f80'
        case 'Fp128':
          return 'f128'
        case 'PpcFp128':
          return 'ppcf128'
        default:
          return description.tag.toLowerCase()
      }
    case 'Structure':
    case 'NamedStructure':
    case 'Function':
    case 'TargetExtension':
      throw new Error('intrinsic overload type has no canonical LLVM mangling')
  }
}

/** @internal */
const intrinsicName = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  id: Id,
  overloads: ReadonlyArray<Type.Type>,
): Effect.fn.Return<string, LlvmError> {
  return yield* BuilderState.mutate(builder, 'Intrinsic.name', (state, owner) =>
    Result.gen(function* () {
      const suffix: Array<string> = []
      for (const type of overloads) {
        const index = yield* Handle.resolve(builder, owner, type, 'Type', 'Intrinsic.name')
        const description = state.types[index]
        if (description === undefined) {
          return yield* Result.fail(
            invalidState({
              operation: 'Intrinsic.name',
              message: 'Intrinsic overload type is missing',
              state: type,
            }),
          )
        }
        suffix.push(
          yield* Result.try({
            try: () => mangleDescription(state, description),
            catch: (cause) =>
              wrappedFailure({
                operation: 'Intrinsic.name',
                message: 'Intrinsic overload type cannot be mangled',
                cause: cause,
              }),
          }),
        )
      }
      return `llvm.${id}${suffix.length === 0 ? '' : `.${suffix.join('.')}`}`
    }),
  )
})

/** @internal */
const flagSet = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  names: ReadonlyArray<string>,
): Effect.fn.Return<Attribute.Set, LlvmError> {
  const values: Array<Attribute.Attribute> = []
  for (const name of names) values.push(yield* Attribute.flag(builder, name))
  return yield* Attribute.set(builder, values)
})

/** @internal */
const commonAttributes = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  id: Id,
): Effect.fn.Return<Attribute.FunctionSet | undefined, LlvmError> {
  const base = ['nocallback', 'nofree', 'nounwind', 'willreturn']
  if (id === 'assume') {
    return yield* Attribute.functionSet(builder, {
      functionAttributes: yield* flagSet(builder, [...base, 'nosync']),
      parameterAttributes: [yield* flagSet(builder, ['noundef'])],
    })
  }
  if (
    id !== 'memcpy' &&
    id !== 'memcpy.inline' &&
    id !== 'memmove' &&
    id !== 'memset' &&
    id !== 'memset.inline'
  ) {
    return undefined
  }
  const functionEntries: Array<Attribute.Attribute> = []
  for (const name of base) functionEntries.push(yield* Attribute.flag(builder, name))
  functionEntries.push(
    yield* Attribute.integer(builder, 'memory', id === 'memset' || id === 'memset.inline' ? 2 : 3),
  )
  const destination =
    id === 'memcpy' || id === 'memcpy.inline'
      ? ['noalias', 'nocapture', 'writeonly']
      : ['nocapture', 'writeonly']
  const source =
    id === 'memcpy' || id === 'memcpy.inline'
      ? ['noalias', 'nocapture', 'readonly']
      : ['nocapture', 'readonly']
  const parameters =
    id === 'memset' || id === 'memset.inline'
      ? [
          yield* flagSet(builder, destination),
          yield* flagSet(builder, []),
          yield* flagSet(builder, []),
          yield* flagSet(builder, ['immarg']),
        ]
      : [
          yield* flagSet(builder, destination),
          yield* flagSet(builder, source),
          yield* flagSet(builder, []),
          yield* flagSet(builder, ['immarg']),
        ]
  return yield* Attribute.functionSet(builder, {
    functionAttributes: yield* Attribute.set(builder, functionEntries),
    parameterAttributes: parameters,
  })
})

/**
 * Declares or retrieves a canonically mangled LLVM intrinsic.
 *
 * **Details**
 *
 * Common and memory intrinsics have built-in signature recipes. Other catalog entries require an
 * explicit signature; overload types form the LLVM name suffix and must have canonical manglings.
 *
 * **Example** (Resolving an intrinsic)
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Builder from '@silk-effect/llvm/Builder'
 * import * as Intrinsic from '@silk-effect/llvm/Intrinsic'
 *
 * const trap = await Effect.runPromise(Effect.gen(function* () {
 *   const builder = yield* Builder.make()
 *   return yield* Intrinsic.resolve(builder, 'trap')
 * }))
 * ```
 *
 * @category intrinsics
 * @since 0.0.0
 */
export const resolve = Effect.fn('Intrinsic.resolve')(function* (
  builder: Builder.Builder,
  id: Id,
  overloads: ReadonlyArray<Type.Type> = [],
  options: ResolveOptions = {},
): Effect.fn.Return<FunctionActor.Function, LlvmError> {
  if (!inventory.includes(id)) {
    return yield* Effect.fail(
      invalidState({
        operation: 'Intrinsic.resolve',
        message: 'Unknown pinned LLVM intrinsic',
        state: id,
      }),
    )
  }
  const recipe = overloadedRecipes[id]
  const simple = simpleRecipes[id]
  const signature =
    options.signature ??
    (recipe === undefined
      ? simple === undefined
        ? undefined
        : yield* simple(builder)
      : yield* recipe(builder, overloads))
  if (signature === undefined) {
    return yield* Effect.fail(
      invalidInput({
        operation: 'Intrinsic.resolve',
        message: 'This intrinsic requires an explicit typed signature',
        input: { id, overloads },
      }),
    )
  }
  const variadic = 'variadic' in signature ? signature.variadic : undefined
  const type = yield* Type.functionType(
    builder,
    signature.returnType,
    signature.parameters,
    variadic === undefined ? {} : { variadic },
  )
  const attributes = options.attributes ?? (yield* commonAttributes(builder, id))
  return yield* FunctionActor.declare(builder, yield* intrinsicName(builder, id, overloads), type, {
    ...(attributes === undefined ? {} : { attributes }),
  })
})

/** @internal */
const inputType = Effect.fnUntraced(function* (
  body: FunctionBody.FunctionBody,
  input: Value.Input,
): Effect.fn.Return<Type.Type, LlvmError> {
  return yield* FunctionBodyActor.inputType(body, input)
})

/** @internal */
const memoryCallAttributes = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  intrinsic: FunctionActor.Function,
  alignments: ReadonlyArray<Alignment.Alignment | undefined>,
): Effect.fn.Return<Attribute.FunctionSet | undefined, LlvmError> {
  const canonical = (yield* FunctionActor.properties(builder, intrinsic)).attributes
  if (
    canonical === undefined ||
    alignments.every((alignment) => alignment?.byteUnits === undefined)
  ) {
    return canonical
  }
  const entries = yield* Attribute.functionSetEntries(builder, canonical)
  const parameters: Array<Attribute.Set> = []
  const length = Math.max(entries.parameterAttributes.length, alignments.length)
  for (let index = 0; index < length; index += 1) {
    const base = entries.parameterAttributes[index] ?? (yield* Attribute.set(builder, []))
    const alignment = alignments[index]
    parameters.push(
      alignment?.byteUnits === undefined
        ? base
        : yield* Attribute.add(
            builder,
            base,
            yield* Attribute.integer(builder, 'align', alignment.byteUnits),
          ),
    )
  }
  return yield* Attribute.functionSet(builder, {
    functionAttributes: entries.functionAttributes,
    returnAttributes: entries.returnAttributes,
    parameterAttributes: parameters,
  })
})

/**
 * Resolves an intrinsic and appends a direct, type-checked call in one operation.
 *
 * @category intrinsics
 * @since 0.0.0
 */
export const call = Effect.fn('Intrinsic.call')(function* (
  body: FunctionBody.FunctionBody,
  id: Id,
  overloads: ReadonlyArray<Type.Type>,
  args: ReadonlyArray<Value.Input>,
  name?: string,
  options: ResolveOptions = {},
): Effect.fn.Return<Value.Value | undefined, LlvmError> {
  const builder = yield* FunctionBodyActor.builder(body)
  const intrinsic = yield* resolve(builder, id, overloads, options)
  return yield* FunctionBodyActor.callDirect(body, intrinsic, args, name)
})

/**
 * Emits `llvm.assume(true)` with an empty `cold` operand bundle as an optimization hint.
 *
 * @category intrinsics
 * @since 0.0.0
 */
export const assumeCold = Effect.fn('Intrinsic.assumeCold')(function* (
  body: FunctionBody.FunctionBody,
): Effect.fn.Return<void, LlvmError> {
  const builder = yield* FunctionBodyActor.builder(body)
  const assume = yield* resolve(builder, 'assume')
  const condition = yield* Constant.integerUnsigned(builder, yield* Type.integer(builder, 1), 1)
  yield* FunctionBodyActor.callDirect(body, assume, [condition], undefined, {
    operandBundles: [{ tag: 'cold', operands: [] }],
  })
})

/**
 * Emits typed `llvm.memcpy` or `llvm.memcpy.inline` with canonical attributes.
 *
 * @category intrinsics
 * @since 0.0.0
 */
export const memcpy = Effect.fn('Intrinsic.memcpy')(function* (
  body: FunctionBody.FunctionBody,
  destination: Value.Input,
  source: Value.Input,
  length: Value.Input,
  options: MemoryCopyOptions = {},
): Effect.fn.Return<void, LlvmError> {
  const builder = yield* FunctionBodyActor.builder(body)
  const overloads = [
    yield* inputType(body, destination),
    yield* inputType(body, source),
    yield* inputType(body, length),
  ]
  const intrinsic = yield* resolve(builder, options.inline ? 'memcpy.inline' : 'memcpy', overloads)
  const volatile = yield* Constant.integerUnsigned(
    builder,
    yield* Type.integer(builder, 1),
    options.volatile ? 1 : 0,
  )
  const attributes = yield* memoryCallAttributes(builder, intrinsic, [
    options.destinationAlignment,
    options.sourceAlignment,
  ])
  yield* FunctionBodyActor.callDirect(
    body,
    intrinsic,
    [destination, source, length, volatile],
    undefined,
    attributes === undefined ? {} : { attributes },
  )
})

/**
 * Emits typed `llvm.memmove` with canonical attributes and optional alignments.
 *
 * @category intrinsics
 * @since 0.0.0
 */
export const memmove = Effect.fn('Intrinsic.memmove')(function* (
  body: FunctionBody.FunctionBody,
  destination: Value.Input,
  source: Value.Input,
  length: Value.Input,
  options: Omit<MemoryCopyOptions, 'inline'> = {},
): Effect.fn.Return<void, LlvmError> {
  const builder = yield* FunctionBodyActor.builder(body)
  const overloads = [
    yield* inputType(body, destination),
    yield* inputType(body, source),
    yield* inputType(body, length),
  ]
  const intrinsic = yield* resolve(builder, 'memmove', overloads)
  const volatile = yield* Constant.integerUnsigned(
    builder,
    yield* Type.integer(builder, 1),
    options.volatile ? 1 : 0,
  )
  const attributes = yield* memoryCallAttributes(builder, intrinsic, [
    options.destinationAlignment,
    options.sourceAlignment,
  ])
  yield* FunctionBodyActor.callDirect(
    body,
    intrinsic,
    [destination, source, length, volatile],
    undefined,
    attributes === undefined ? {} : { attributes },
  )
})

/**
 * Emits typed `llvm.memset` or `llvm.memset.inline` with canonical attributes.
 *
 * @category intrinsics
 * @since 0.0.0
 */
export const memset = Effect.fn('Intrinsic.memset')(function* (
  body: FunctionBody.FunctionBody,
  destination: Value.Input,
  byte: Value.Input,
  length: Value.Input,
  options: MemorySetOptions = {},
): Effect.fn.Return<void, LlvmError> {
  const builder = yield* FunctionBodyActor.builder(body)
  const overloads = [yield* inputType(body, destination), yield* inputType(body, length)]
  const intrinsic = yield* resolve(builder, options.inline ? 'memset.inline' : 'memset', overloads)
  const volatile = yield* Constant.integerUnsigned(
    builder,
    yield* Type.integer(builder, 1),
    options.volatile ? 1 : 0,
  )
  const attributes = yield* memoryCallAttributes(builder, intrinsic, [options.destinationAlignment])
  yield* FunctionBodyActor.callDirect(
    body,
    intrinsic,
    [destination, byte, length, volatile],
    undefined,
    attributes === undefined ? {} : { attributes },
  )
})
