import * as Diagnostic from './Diagnostic.js'
import * as LiteralForm from './LiteralForm.js'
import * as Option from 'effect/Option'
import * as SourceFile from './SourceFile.js'
import type * as SourceSpan from './SourceSpan.js'
import * as StaticText from './StaticText.js'
import * as SyntaxTree from './SyntaxTree.js'
import * as Type from './Type.js'

/** Unsafe behavioral promises attached to one immediate foreign C call. */
export interface ForeignContract {
  readonly memory: 'none' | 'read' | 'write' | 'readwrite'
  readonly locality: 'external' | 'arguments'
  readonly noCapture: ReadonlyArray<number>
  readonly borrow: ReadonlyArray<number>
  /** Synchronous, calling-thread, dynamic-extent-only invocation of these native callbacks. */
  readonly callbacks: ReadonlyArray<number>
  readonly returned: number | undefined
  readonly noReturn: boolean
  readonly unwind: 'forbidden'
}

/** An ordinary C declaration accesses external memory and may capture raw pointer arguments. */
export const conservative: ForeignContract = Object.freeze({
  memory: 'readwrite',
  locality: 'external',
  noCapture: Object.freeze([]),
  borrow: Object.freeze([]),
  callbacks: Object.freeze([]),
  returned: undefined,
  noReturn: false,
  unwind: 'forbidden',
})

/** Canonical behavior identity excludes source names, property order and current-request origins. */
export const key = (self: ForeignContract): string =>
  `${self.memory}/${self.locality}/capture:${self.noCapture.join(',')}/borrow:${self.borrow.join(',')}/callbacks:${self.callbacks.join(',')}/returned:${self.returned ?? '-'}/noreturn:${self.noReturn}/unwind:${self.unwind}`

export interface Parameter {
  readonly name: string
  readonly type: Type.Type | undefined
  readonly span: SourceSpan.SourceSpan
}

export interface Analysis {
  readonly contract: ForeignContract
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

const parameterKind = (name: 'noCapture' | 'borrow' | 'callbacks', type: Type.Type): boolean => {
  if (name === 'callbacks') return Type.isForeignFunction(type)
  if (name === 'borrow') return Type.isReference(type)
  return Type.isPointer(type)
}

const decoder = new TextDecoder()
const spelling = (source: SourceFile.SourceFile, span: SourceSpan.SourceSpan): string =>
  Option.getOrElse(SourceFile.spelling(source, span), () => '')
const text = (source: SourceFile.SourceFile, node: SyntaxTree.Node): string | undefined => {
  const token = SyntaxTree.directToken(node, 'TextLiteral')
  if (token === undefined) return undefined
  const bytes = Option.getOrUndefined(SourceFile.slice(source, token.span))
  const form = bytes === undefined ? undefined : LiteralForm.recognize(bytes)
  if (bytes === undefined || form === undefined) return undefined
  const decoded = StaticText.decode(Array.from(bytes), form)
  return decoded._tag === 'Decoded'
    ? decoder.decode(Uint8Array.from(decoded.data.bytes))
    : undefined
}

/** Validates the sealed source property and resolved parameter types without executing source. */
export const analyze = (
  source: SourceFile.SourceFile,
  clause: SyntaxTree.Node | undefined,
  parameters: ReadonlyArray<Parameter>,
  result: Type.Type | undefined,
): Analysis => {
  if (clause === undefined) return { contract: conservative, diagnostics: [] }
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const reject = (name: string, problem: string, span: SourceSpan.SourceSpan): void => {
    diagnostics.push(
      Diagnostic.foreignDeclarationRestriction(`foreign contract ${name}: ${problem}`, span),
    )
  }
  const names = clause.children
    .filter(SyntaxTree.isToken)
    .filter((token) => token.kind === 'Identifier')
    .map((token) => spelling(source, token.span))
  if (names[1] !== 'Intrinsic' || names[2] !== 'foreign')
    reject('owner', 'expected Intrinsic.foreign', clause.span)
  const properties = new Map<
    string,
    { readonly value: SyntaxTree.Node; readonly span: SourceSpan.SourceSpan }
  >()
  for (const node of clause.children.filter(SyntaxTree.isNode)) {
    if (node.kind !== 'FunctionProperty') continue
    const nameToken = SyntaxTree.directToken(node, 'Identifier')
    const value = node.children.find(SyntaxTree.isNode)
    if (nameToken === undefined || value === undefined) continue
    const name = spelling(source, nameToken.span)
    if (properties.has(name)) reject(name, 'duplicate property', nameToken.span)
    if (
      !['memory', 'locality', 'noCapture', 'borrow', 'callbacks', 'returned', 'noReturn'].includes(
        name,
      )
    )
      reject(name, 'unsupported property', nameToken.span)
    properties.set(name, { value, span: nameToken.span })
  }
  const choice = <const Choices extends ReadonlyArray<string>>(
    name: string,
    values: Choices,
    fallback: Choices[number],
  ): Choices[number] => {
    const property = properties.get(name)
    if (property === undefined) return fallback
    const value = text(source, property.value)
    const found = values.find((candidate) => candidate === value)
    if (found !== undefined) return found
    reject(name, `expected ${values.join(' or ')}`, property.value.span)
    return fallback
  }
  const memory = choice('memory', ['none', 'read', 'write', 'readwrite'], 'readwrite')
  const locality = choice('locality', ['external', 'arguments'], 'external')
  const parameterSet = (name: 'noCapture' | 'borrow' | 'callbacks'): ReadonlyArray<number> => {
    const property = properties.get(name)
    if (property === undefined) return []
    const tuple = property.value
    if (tuple.kind !== 'TupleLiteralExpression' && tuple.kind !== 'UnitExpression') {
      reject(name, 'expected a tuple of parameter names', tuple.span)
      return []
    }
    const ordinals: Array<number> = []
    for (const element of tuple.children.filter(SyntaxTree.isNode)) {
      const name_ = text(source, element)
      const ordinal = parameters.findIndex((parameter) => parameter.name === name_)
      const parameter = parameters[ordinal]
      if (parameter === undefined || ordinals.includes(ordinal)) {
        reject(name, 'expected unique existing parameter names', element.span)
        continue
      }
      if (parameter.type !== undefined && !parameterKind(name, parameter.type))
        reject(
          name,
          `requires a ${{ callbacks: 'native function pointer', borrow: 'single-value reference', noCapture: 'raw pointer' }[name]} parameter`,
          element.span,
        )
      ordinals.push(ordinal)
    }
    return Object.freeze(ordinals.sort((left, right) => left - right))
  }
  const noCapture = parameterSet('noCapture')
  const borrow = parameterSet('borrow')
  const callbacks = parameterSet('callbacks')
  const returnedProperty = properties.get('returned')
  let returned: number | undefined
  if (returnedProperty !== undefined) {
    const name = text(source, returnedProperty.value)
    const ordinal = parameters.findIndex((parameter) => parameter.name === name)
    const parameter = parameters[ordinal]
    if (
      parameter === undefined ||
      (parameter.type !== undefined &&
        (!Type.isPointer(parameter.type) ||
          (result !== undefined && !Type.equals(parameter.type, result))))
    )
      reject(
        'returned',
        'requires a raw pointer parameter identical to the result type',
        returnedProperty.value.span,
      )
    else if (noCapture.includes(ordinal))
      reject(
        'returned',
        'cannot capture a noCapture parameter through the result',
        returnedProperty.value.span,
      )
    else returned = ordinal
  }
  const noReturnProperty = properties.get('noReturn')
  let noReturn = false
  if (noReturnProperty !== undefined) {
    const token =
      SyntaxTree.directToken(noReturnProperty.value, 'TrueKeyword') ??
      SyntaxTree.directToken(noReturnProperty.value, 'FalseKeyword')
    if (token === undefined)
      reject('noReturn', 'expected a Boolean literal', noReturnProperty.value.span)
    else noReturn = token.kind === 'TrueKeyword'
    if (
      noReturn &&
      ((result !== undefined && !Type.equals(result, Type.unit)) || returnedProperty !== undefined)
    )
      reject(
        'noReturn',
        'requires a unit result without a returned alias',
        noReturnProperty.value.span,
      )
  }
  return Object.freeze({
    contract: Object.freeze({
      memory,
      locality: memory === 'none' ? 'external' : locality,
      noCapture,
      borrow,
      callbacks,
      returned,
      noReturn,
      unwind: 'forbidden',
    }),
    diagnostics: Object.freeze(diagnostics),
  })
}

/** Checks normalized promises against semantic types before accepting a supplied interface. */
export const acceptsTypes = (
  self: ForeignContract,
  parameters: ReadonlyArray<Type.Type>,
  result: Type.Type,
): boolean => {
  const returned = self.returned === undefined ? undefined : parameters[self.returned]
  return (
    self.noCapture.every(
      (ordinal) => parameters[ordinal] !== undefined && Type.isPointer(parameters[ordinal]),
    ) &&
    self.borrow.every(
      (ordinal) => parameters[ordinal] !== undefined && Type.isReference(parameters[ordinal]),
    ) &&
    self.callbacks.every(
      (ordinal) => parameters[ordinal] !== undefined && Type.isForeignFunction(parameters[ordinal]),
    ) &&
    parameters.every((parameter, ordinal) =>
      Type.isReference(parameter)
        ? self.borrow.includes(ordinal)
        : !Type.isForeignFunction(parameter) ||
          (self.callbacks.includes(ordinal) && callbackAccessAdmitted(self, parameter.contract)),
    ) &&
    (self.returned === undefined ||
      (returned !== undefined && Type.isPointer(returned) && Type.equals(returned, result))) &&
    (!self.noReturn || Type.equals(result, Type.unit))
  )
}

/** Tests whether callback access can coexist with the enclosing complete-call loans. */
export const callbackAccessAdmitted = (self: ForeignContract, callback: ForeignContract): boolean =>
  self.borrow.length === 0 || callback.memory === 'none' || callback.locality === 'arguments'

/** Checks resolved type obligations after aliases and parameter types have completed. */
export const validate = (
  self: ForeignContract,
  parameters: ReadonlyArray<Parameter>,
  result: Type.Type | undefined,
  span: SourceSpan.SourceSpan,
): ReadonlyArray<Diagnostic.Diagnostic> => {
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  for (const [name, ordinals] of [
    ['noCapture', self.noCapture],
    ['borrow', self.borrow],
    ['callbacks', self.callbacks],
  ] as const)
    for (const ordinal of ordinals) {
      const parameter = parameters[ordinal]
      if (parameter?.type !== undefined && !parameterKind(name, parameter.type))
        diagnostics.push(
          Diagnostic.foreignDeclarationRestriction(
            `foreign contract ${name}: invalid parameter type`,
            parameter.span,
          ),
        )
    }
  for (const [ordinal, parameter] of parameters.entries()) {
    if (parameter.type === undefined || !Type.isForeignFunction(parameter.type)) continue
    if (!self.callbacks.includes(ordinal))
      diagnostics.push(
        Diagnostic.foreignDeclarationRestriction(
          'callback parameter requires an explicit synchronous callbacks promise',
          parameter.span,
        ),
      )
    if (!callbackAccessAdmitted(self, parameter.type.contract))
      diagnostics.push(
        Diagnostic.foreignDeclarationRestriction(
          'callback access alongside borrowed storage must be argument-local',
          parameter.span,
        ),
      )
  }
  if (self.returned !== undefined) {
    const parameter = parameters[self.returned]
    if (
      parameter?.type !== undefined &&
      (!Type.isPointer(parameter.type) ||
        (result !== undefined && !Type.equals(parameter.type, result)))
    )
      diagnostics.push(
        Diagnostic.foreignDeclarationRestriction(
          'foreign contract returned: result must equal the raw pointer parameter type',
          parameter.span,
        ),
      )
  }
  if (self.noReturn && result !== undefined && !Type.equals(result, Type.unit))
    diagnostics.push(
      Diagnostic.foreignDeclarationRestriction(
        'foreign contract noReturn: result must be unit',
        span,
      ),
    )
  return diagnostics
}

/** Renders normalized promises using a declaration's current parameter names. */
export const source = (self: ForeignContract, names: ReadonlyArray<string>): string => {
  if (key(self) === key(conservative)) return ''
  const tuple = (ordinals: ReadonlyArray<number>): string =>
    `(${ordinals.map((ordinal) => JSON.stringify(names[ordinal] ?? '')).join(', ')}${ordinals.length === 1 ? ',' : ''})`
  const fields = [
    ...(self.memory === 'readwrite' ? [] : [`memory: "${self.memory}"`]),
    ...(self.locality === 'external' ? [] : ['locality: "arguments"']),
    ...(self.noCapture.length === 0 ? [] : [`noCapture: ${tuple(self.noCapture)}`]),
    ...(self.borrow.length === 0 ? [] : [`borrow: ${tuple(self.borrow)}`]),
    ...(self.callbacks.length === 0 ? [] : [`callbacks: ${tuple(self.callbacks)}`]),
    ...(self.returned === undefined
      ? []
      : [`returned: ${JSON.stringify(names[self.returned] ?? '')}`]),
    ...(self.noReturn ? ['noReturn: true'] : []),
  ]
  return ` with Intrinsic.foreign(${fields.join(', ')})`
}

/** Validates serialized normalized behavior against the manifest's classified parameter types. */
export const inspect = (
  input: unknown,
  parameters: ReadonlyArray<string>,
  result: string,
): ForeignContract | undefined => {
  const isRecord = (value: unknown): value is Record<string, unknown> =>
    typeof value === 'object' && value !== null && !Array.isArray(value)
  if (!isRecord(input)) return undefined
  const fields = input
  if (
    Object.keys(fields).some(
      (name) =>
        ![
          'memory',
          'locality',
          'noCapture',
          'borrow',
          'callbacks',
          'returned',
          'noReturn',
          'unwind',
        ].includes(name),
    )
  )
    return undefined
  const { memory, locality, noReturn, unwind, returned } = fields
  if (memory !== 'none' && memory !== 'read' && memory !== 'write' && memory !== 'readwrite')
    return undefined
  if (locality !== 'external' && locality !== 'arguments') return undefined
  if (memory === 'none' && locality !== 'external') return undefined
  if (typeof noReturn !== 'boolean' || unwind !== 'forbidden') return undefined
  const ordinals = (value: unknown, callback = false): ReadonlyArray<number> | undefined => {
    if (!Array.isArray(value)) return undefined
    const numbers: Array<number> = []
    for (const ordinal of value) {
      if (
        typeof ordinal !== 'number' ||
        !Number.isSafeInteger(ordinal) ||
        ordinal < 0 ||
        ordinal >= parameters.length ||
        !parameters[ordinal]?.startsWith(callback ? 'extern "C" fn(' : 'pointer<') ||
        ordinal <= (numbers.at(-1) ?? -1)
      )
        return undefined
      numbers.push(ordinal)
    }
    return Object.freeze(numbers)
  }
  const noCapture = ordinals(fields.noCapture)
  const borrow = ordinals(fields.borrow)
  const callbacks = ordinals(fields.callbacks, true)
  if (
    noCapture === undefined ||
    borrow === undefined ||
    callbacks === undefined ||
    borrow.some((ordinal) => noCapture.includes(ordinal))
  )
    return undefined
  if (
    returned !== undefined &&
    (typeof returned !== 'number' ||
      !Number.isSafeInteger(returned) ||
      returned < 0 ||
      returned >= parameters.length ||
      !result.startsWith('pointer<') ||
      result !== parameters[returned] ||
      noCapture.includes(returned) ||
      borrow.includes(returned))
  )
    return undefined
  if (
    parameters.some(
      (parameter, ordinal) =>
        parameter.startsWith('extern "C" fn(') && !callbacks.includes(ordinal),
    )
  )
    return undefined
  if (noReturn && (result !== 'void' || returned !== undefined)) return undefined
  return Object.freeze({
    memory,
    locality,
    noCapture,
    borrow,
    callbacks,
    returned,
    noReturn,
    unwind,
  })
}

/** Parses the canonical behavioral key embedded in a native function-pointer interface. */
export const inspectKey = (
  text: string,
  parameters: ReadonlyArray<string>,
  result: string,
): ForeignContract | undefined => {
  const match =
    /^(none|read|write|readwrite)\/(external|arguments)\/capture:([0-9.]*)\/borrow:([0-9.]*)\/callbacks:([0-9.]*)\/returned:(-|[0-9]+)\/noreturn:(true|false)\/unwind:forbidden$/.exec(
      text,
    )
  if (match === null) return undefined
  const ordinals = (value: string | undefined): ReadonlyArray<number> =>
    value === undefined || value === '' ? [] : value.split('.').map(Number)
  const contract = inspect(
    {
      memory: match[1],
      locality: match[2],
      noCapture: ordinals(match[3]),
      borrow: ordinals(match[4]),
      callbacks: ordinals(match[5]),
      returned: match[6] === '-' ? undefined : Number(match[6]),
      noReturn: match[7] === 'true',
      unwind: 'forbidden',
    },
    parameters,
    result,
  )
  return contract !== undefined && key(contract).replaceAll(',', '.') === text
    ? contract
    : undefined
}
