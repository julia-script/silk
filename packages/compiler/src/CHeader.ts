import type * as Backend from './Backend.js'

type CType =
  | { readonly _tag: 'Scalar'; readonly spelling: string }
  | { readonly _tag: 'Pointer'; readonly mutable: boolean }
  | {
      readonly _tag: 'FunctionPointer'
      readonly parameters: ReadonlyArray<CType>
      readonly result: CType
    }

/** The verified export inventory rendered into one C consumer header. */
export interface CHeader {
  readonly packageName: string
  readonly functions: ReadonlyArray<Backend.ForeignExport>
  readonly data: ReadonlyArray<Backend.ForeignStatic>
}

/** Creates one immutable header model from a validated package name and verified exports. */
export const make = (
  packageName: string,
  functions: ReadonlyArray<Backend.ForeignExport>,
  data: ReadonlyArray<Backend.ForeignStatic>,
): CHeader => Object.freeze({ packageName, functions, data })

const scalar = (type: string): string | undefined => {
  switch (type) {
    case 'void':
      return 'void'
    case 'i8':
      return 'int8_t'
    case 'u8':
      return 'uint8_t'
    case 'i16':
      return 'int16_t'
    case 'u16':
      return 'uint16_t'
    case 'i32':
      return 'int32_t'
    case 'u32':
      return 'uint32_t'
    case 'i64':
      return 'int64_t'
    case 'u64':
      return 'uint64_t'
    case 'f32':
      return 'float'
    case 'f64':
      return 'double'
    default:
      return undefined
  }
}

const splitParameters = (text: string): ReadonlyArray<string> | undefined => {
  if (text.length === 0) return Object.freeze([])
  const parameters: Array<string> = []
  let depth = 0
  let start = 0
  for (let index = 0; index < text.length; index += 1) {
    const character = text[index]
    if (character === '(') depth += 1
    else if (character === ')') depth -= 1
    else if (character === ',' && depth === 0) {
      parameters.push(text.slice(start, index))
      start = index + 1
    }
    if (depth < 0) return undefined
  }
  if (depth !== 0) return undefined
  parameters.push(text.slice(start))
  return Object.freeze(parameters)
}

const decode = (type: string): CType | undefined => {
  const scalarSpelling = scalar(type)
  if (scalarSpelling !== undefined)
    return Object.freeze({ _tag: 'Scalar', spelling: scalarSpelling })
  const pointer = /^pointer<(mut|const);(?:[A-Za-z0-9_.~-]|%[0-9A-F]{2})+>$/.exec(type)
  if (pointer !== null) return Object.freeze({ _tag: 'Pointer', mutable: pointer[1] === 'mut' })
  const prefix = 'extern "C" fn('
  if (!type.startsWith(prefix)) return undefined
  let depth = 1
  let close = -1
  for (let index = prefix.length; index < type.length; index += 1) {
    const character = type[index]
    if (character === '(') depth += 1
    else if (character === ')') depth -= 1
    if (depth === 0) {
      close = index
      break
    }
  }
  if (close < 0 || type.slice(close, close + 3) !== ')->') return undefined
  const parameterTexts = splitParameters(type.slice(prefix.length, close))
  if (parameterTexts === undefined) return undefined
  const parameters = parameterTexts.map(decode)
  const result = decode(type.slice(close + 3))
  if (parameters.some((parameter) => parameter === undefined) || result === undefined)
    return undefined
  return Object.freeze({
    _tag: 'FunctionPointer',
    parameters: Object.freeze(
      parameters.flatMap((parameter) => (parameter === undefined ? [] : [parameter])),
    ),
    result,
  })
}

const parameterList = (parameters: ReadonlyArray<CType>): string =>
  parameters.length === 0
    ? 'void'
    : parameters.map((parameter, index) => declarator(parameter, `arg${index}`)).join(', ')

const declarator = (type: CType, name: string): string => {
  switch (type._tag) {
    case 'Scalar':
      return `${type.spelling} ${name}`
    case 'Pointer':
      return `${type.mutable ? 'void' : 'const void'} *${name}`
    case 'FunctionPointer':
      return declarator(type.result, `(*${name})(${parameterList(type.parameters)})`)
  }
}

const immutableDataDeclarator = (type: CType, name: string): string => {
  switch (type._tag) {
    case 'Scalar':
      return `const ${declarator(type, name)}`
    case 'Pointer':
    case 'FunctionPointer':
      return declarator(type, `const ${name}`)
  }
}

const requireType = (type: string): CType => {
  const decoded = decode(type)
  if (decoded === undefined) throw new RangeError(`Compiler produced unknown C ABI class ${type}`)
  return decoded
}

const guard = (packageName: string): string =>
  `SILK_${packageName.toUpperCase().replace(/[^A-Z0-9]/g, '_')}_H`

const compareText = (left: string, right: string): number => {
  if (left < right) return -1
  if (left > right) return 1
  return 0
}

/** Renders the exact consumer declarations for one verified library inventory. */
export const render = (self: CHeader): string => {
  const declarations: Array<readonly [string, 'data' | 'function', string]> = []
  for (const fn of self.functions) {
    const parameters = fn.parameters.map(requireType)
    const result = requireType(fn.result)
    declarations.push([
      fn.symbol,
      'function',
      `${declarator(result, `${fn.symbol}(${parameterList(parameters)})`)};`,
    ])
  }
  for (const data of self.data) {
    if (data.direction !== 'Export') continue
    declarations.push([
      data.symbol,
      'data',
      `extern ${immutableDataDeclarator(requireType(data.type), data.symbol)};`,
    ])
  }
  declarations.sort(
    (left, right) => compareText(left[0], right[0]) || compareText(left[1], right[1]),
  )
  const includeGuard = guard(self.packageName)
  return [
    `#ifndef ${includeGuard}`,
    `#define ${includeGuard}`,
    '',
    '#include <stdint.h>',
    '',
    '#ifdef __cplusplus',
    'extern "C" {',
    '#endif',
    '',
    ...declarations.map(([, , declaration]) => declaration),
    '',
    '#ifdef __cplusplus',
    '}',
    '#endif',
    '',
    `#endif /* ${includeGuard} */`,
    '',
  ].join('\n')
}

/** Encodes one canonical header as UTF-8 bytes. */
export const encode = (self: CHeader): Uint8Array => new TextEncoder().encode(render(self))
